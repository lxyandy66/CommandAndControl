data.ep.raw<-as.data.table(read.csv(file="OriginalData/ModiTestId_StepResponseForRecognition_20210625.csv"))

#不能直接label，因为每次重新执行labView时会重置，因此可能重复
data.ep.raw$Time<-as.POSIXct(data.ep.raw$Time)
data.ep.raw$timeLabel<-format(data.ep.raw$Time,format="%Y-%m-%d %H:%M:%S")


#看一下分布，去除原始数据的异常值
ggplot(data.ep.raw,aes(x=Flowrate))+geom_density()
ggplot(data.ep.raw,aes(x=1,y=Flowrate))+geom_boxplot()

data.ep.raw<-data.ep.raw%>%{
    .[Totalpressure>200]$Totalpressure<-NA #
    .[Subpressure>200]$Subpressure<-NA #
    .[Flowrate>10]$Flowrate<-NA #
    .[InWaterT<10|InWaterT>80]$InWaterT<-NA # #切记|会都比较两个判断，但||只会比较一个，满足即返回
    .[OutWaterT<10|OutWaterT>80]$OutWaterT<-NA #
    .[InWindT<10]$InWindT<-NA
    .[OutWindT<10]$OutWindT<-NA
    .[Fre>100]$Fre<-NA #
    .
}


####整合至秒级####
#可能有问题的: Power和Powerset
# 目前直接取平均值
setorder(data.ep.raw,Time)
#注意！getMode有相同出现次数时可能返回多个数值
data.ep.roomResponse.second<-cbind(data.ep.raw[,.(Time=Time[1],ID=ID[1],Label=Label[1],
                                                  testId=getMode(testId,na.rm = TRUE)[1],
                                                  Vset=getMode(Vset,na.rm = TRUE)[1],
                                                  testVset=getMode(testVset,na.rm = TRUE)[1],#阀门测试用
                                                  isOn=getMode(isOn,na.rm = TRUE)[1],#阀门测试用
                                                  testType=getMode(testType,na.rm = TRUE)[1],#阀门测试用
                                                  onRoutine=getMode(onRoutine,na.rm = TRUE)[1]#阀门测试用
                                                  #注意采样时间问题，是用众数还是平均数，对于手动众数没问题，考虑串级自动的时候
                                                  ),by=timeLabel],
                      data.ep.raw[,lapply(.SD,mean,na.rm=TRUE),
                                   .SDcols=c("Flowrate","Totalpressure","Subpressure","InWaterT","OutWaterT",
                                                   "InWindT","OutWindT","Valveopening","Fset","Tset",  #"Vset"  ,   
                                             "t_out_set","t_return_set","flow_set",
                                             "Powerset","Fre","HeatingRate"),by=timeLabel][,-"timeLabel"])%>%
                mutate(.,Time=as.POSIXct(.$Time))%>%as.data.table()
View(table(data.ep.roomResponse.second$Vset))

data.ep.roomResponse.second$isOn<-data.ep.roomResponse.second$Valveopening<data.ep.roomResponse.second$Vset


####根据时间分配TestId####
data.ep.roomResponse.second$testId<-"prepare"
info.ep.testId<-read.xlsx(file="Info_TestId.xlsx",sheetName = "0602")%>%as.data.table(.)
apply(info.ep.testId[,c("start","end","testId")], MARGIN = 1, function(x){
  data.ep.roomResponse.second[Time %within% interval(start=as.POSIXct(x[1]),end=as.POSIXct(x[2]))]$testId<<-as.character(x[3])
  })
data.ep.roomResponse.second<-merge(x=data.ep.roomResponse.second,
                                   y=info.ep.testId[,c("testId","Kp","Ti")],all.x = TRUE,by = "testId")


####根据TestId分配正序的label####
#原来的程序label不一样，测出来label对应一个是1s一个是2s
data.ep.roomResponse.second$timeCount<- -999
for(i in unique(data.ep.roomResponse.second[testId!="prepare"]$testId)){
  data.ep.roomResponse.second[testId==i]$timeCount<-0:(nrow(data.ep.roomResponse.second[testId==i])-1)
}


####阀门-流量处理####
#Grad流量取均值
data.ep.valveFlow<-data.ep.roomResponse.second[testType=="Grad",
                                               .(testId=testId[1],
                                                 Vset=Vset[1],
                                                 onRoutine=onRoutine[1],
                                                 Flowrate=mean(Flowrate,na.rm=TRUE),
                                                 Valveopening=mean(Valveopening,na.rm=TRUE)
                                                 ),by=(testIdVsetDir=paste(testId,Vset,onRoutine,sep = "_"))]
#平均后迟滞环评估
ggplot(data.ep.valveFlow,aes(x=Vset,y=Flowrate,color=testId,shape=onRoutine))+geom_point()+geom_line()
stat.ep.valveFlow<-data.ep.valveFlow[,.(maxHR=(Flowrate[onRoutine==TRUE]-Flowrate[onRoutine==FALSE]),
                                        ),by=(testIdVset=paste(testId,Vset,sep = "_"))]

####可视化####
data.ep.roomResponse.second[testId=="Klow_1.1"]%>%
  ggplot(data=.,aes(x=Label,y=t_out_set,color=testId,lty=testId))+geom_line()+geom_line(aes(x=Label,y=InWindT))+geom_line(aes(x=Label,y=Flowrate*50))+facet_wrap(~testId,nrow = 3)

temp.ep.pre[testId%in%c("V2F_7")&timeCount<600,#"prepare", #&!is.na(Kp)&Kp!=0,,"V2O_1","V2O_2","V2O_3",!testId%in%c("prepare","Casc_Low_1","Casc_Low_2")
                            c("timeCount","testId","Time","Valveopening","Vset","Flowrate","OutWindT","InWindT","t_out_set","t_return_set","Kp","Ti","flow_set")]%>%#,
  mutate(.,para=paste("Kp"=as.character(.$Kp),"Ti"=as.character(.$Ti),sep=","))%>%.[,!names(.)%in%c("Kp","Ti")]%>%
  melt(.,id.var=c("timeCount","testId","Time","para"))%>%#,"Ti","para"
  as.data.table(.)%>%{ #,"InWindT","t_out_set"
    ggplot(data=.[variable %in% c("Valveopening","Vset")],aes(x=timeCount,y=value,color=variable,lty=variable,width=4,group=variable))+
      geom_line()+
      geom_line(data=.[variable %in% c("flow_set","Flowrate")],aes(x=timeCount,y=value*100))+#)+value#"OutWindT",(value-20)*5)
      geom_line(data=.[variable %in% c("InWindT","t_out_set")],aes(x=timeCount,y=value))+
      scale_y_continuous(sec.axis = sec_axis(~./100,name = "Flow rate"))+#./5+20
      facet_wrap(~testId,nrow = 3)+
      theme_bw()+theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),#legend.position = c(0.85,0.2),
                       legend.text = element_text(size=16))#
  }

#流量可视化
#阶跃迟滞环
data.ep.roomResponse.second[!is.na(testId)&testId=="MV=0.5_BV=0_F=33_Step"&testVset<40,
                            c("timeCount","Time","testId","Valveopening","testVset","Flowrate","Totalpressure","Subpressure")]%>%#,"isOn"
  #melt(.,id.var=c("timeCount","testId","Time","Vset"))%>%
  ggplot(data = .,aes(x=Valveopening,y=Flowrate,color=as.factor(testVset)))+
  geom_point(alpha=0.6)+
  geom_path(aes(mapping = timeCount))+
  facet_wrap(~as.factor(testVset),nrow = 2)#Valveopening

#不同工况迟滞环
data.ep.roomResponse.second[!is.na(testId)&testType=="Step"&testVset==60,
                            c("timeCount","Time","testId","Valveopening","testVset","Flowrate","Totalpressure","Subpressure")]%>%#,"isOn"
  #melt(.,id.var=c("timeCount","testId","Time","Vset"))%>%
  ggplot(data = .,aes(x=Valveopening,y=Flowrate,color=as.factor(testId)))+
  geom_point(alpha=0.6)+
  geom_path(aes(mapping = timeCount))#+
  facet_wrap(~as.factor(testVset),nrow = 2)#Valveopening

#低流量粘滞
data.ep.roomResponse.second[!is.na(testId)&testId=="MV=0.5_BV=0_F=33_Step"&Vset.1<12,
                            c("timeCount","Time","testId","Valveopening","Vset.1","Flowrate","Totalpressure","Subpressure")]%>%#,"isOn"
  ggplot(data = .,aes(x=timeCount,y=Flowrate,color=as.factor(Vset.1)))+
  geom_point(alpha=0.6)+
  geom_path()+
  facet_wrap(~as.factor(Vset.1),nrow = 3)


####统计一下各case的情况####
nn<-temp.ep.pre[timeCount<600&testId=="V2F_4"][timeCount %in% c((max(timeCount)-100):max(timeCount))]
mean(temp.ep.pre[timeCount<600&testId=="V2F_4"][timeCount%in% c(max(timeCount)-100:max(timeCount))]$InWindT,na.rm=TRUE)

stat.ep.testId<-temp.ep.pre[timeCount<600,.(
  duration=max(timeLabel,na.rm = TRUE),
  peakime=timeCount[InWindT==max(InWindT,na.rm = TRUE)][1],
  startTout=mean(InWindT[timeCount%in%1:10],na.rm=TRUE),#开始时刻的送风温度
  startTroom=mean(OutWindT[timeCount%in%1:10],na.rm=TRUE),#开始时刻的室内温度
  startFlowrate=mean(Flowrate[timeCount%in%1:10],na.rm=TRUE),#开始时刻的流量
  meanValveBias=mean(abs(Valveopening-Vset),na.rm=TRUE),
  meanLastTout=mean(InWindT[timeCount%in% c((length(timeCount)-100):length(timeCount))],na.rm = TRUE),#最后100s的波动情况#(length(timeCount)-60:length(timeCount))
  sdLastTout=sd(InWindT[timeCount%in% c((length(timeCount)-100):length(timeCount))],na.rm = TRUE),#最后100s的波动情况
  maxTout=max(InWindT,na.rm = TRUE),
  overshoot=(max(InWindT,na.rm = TRUE)/mean(InWindT[timeCount%in% c((length(timeCount)-100):length(timeCount))],na.rm = TRUE))-1
),by=testId]%>%
  merge(x=.,y=info.ep.testId[,c("testId","Kp","Ti")],all.x = TRUE,by = "testId")


  
##查看最后2min变化
data.ep.roomResponse.second[testId=="OrgCascWithSetpoint"]%>%
  .[(nrow(nn)-120):nrow(nn),c("Time","OutWindT","t_return_set")]%>%#,"t_out_set""OutWindT",
  melt(data=.,id.var=c("Time"))%>%
  ggplot(data=.,aes(x=variable,y=value,color=variable))+geom_boxplot()+ylim(25,30)


#输出秒级数据
write.xlsx(data.ep.roomResponse.second,file="SecVer_RoomResponse_20210507.xlsx")

####看一下阀门的开关####
#sheetName = "阀门开关响应"
data.ep.valveFlow<-as.data.table(read.csv(file="OriginalData/SecVer_ResponseOnOffTest_20210506.csv"))%>%
  mutate(.,TestId=as.factor(.$TestId),isOn=(.$from<.$to))%>%as.data.table(.)%>%.[complete.cases(.)]

ggplot(data=data.ep.valveFlow[TestId%in%c("2","3","4","5","6","RTR_2","T2")|(TestId=="T1"&isOn==TRUE)],aes(x=Valveopening,y=Flowrate,color=isOn,lty=TestId))+geom_line()+facet_wrap(~TestId,nrow = 2)+
  theme_bw()+theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.text = element_text(size=16))


####室温基本响应####
# sheetName = "温度响应"
data.ep.temp<-as.data.table(read.xlsx(file="SecVer_ResponseTest_20210506.xlsx",sheetIndex =  2))
ggplot(data=data.ep.temp,aes(x=Label,y=Flowrate,color=isOn,lty=TestId))+geom_line()+facet_wrap(~TestId,nrow = 2)


