data.ep.raw<-as.data.table(read.csv(file="OriginalData/AhuOutletTuning_20210602.csv"))

#不能直接label，因为每次重新执行labView时会重置，因此可能重复
data.ep.raw$timeLabel<-format(data.ep.raw$Time,format="%Y-%m-%d %H:%M:%S")
# data.ep.raw$Time<-as.POSIXct(data.ep.raw$Time)

#看一下分布，去除原始数据的异常值
ggplot(data.ep.raw,aes(x=Fre))+geom_density()
ggplot(data.ep.raw,aes(x=1,y=Totalpressure))+geom_boxplot()

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
data.ep.roomResponse.second<-cbind(data.ep.raw[,.(Time=Time[1],ID=ID[1],Label=Label[1]),by=timeLabel],
                      data.ep.raw[,lapply(.SD,mean,na.rm=TRUE),
                                   .SDcols=c("Flowrate","Totalpressure","Subpressure","InWaterT","OutWaterT",
                                                   "InWindT","OutWindT","Valveopening","Vset","Fset","Tset",       
                                             "t_out_set","t_return_set","flow_set",
                                             "Powerset","Fre","HeatingRate"),by=timeLabel][,-"timeLabel"])%>%
                mutate(.,Time=as.POSIXct(.$Time))%>%as.data.table()


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

####可视化####
data.ep.roomResponse.second[!is.na(Kp)&Kp!=0&Label<1000&testId!="Casc_Low"]%>%
  ggplot(data=.,aes(x=Label,y=t_out_set,color=testId,lty=testId))+geom_line()+geom_line(aes(x=Label,y=OutWindT))+facet_wrap(~testId,nrow = 3)

data.ep.roomResponse.second[testId%in%c("V2O_1","V2O_2","V2O_3"),#"prepare", #&!is.na(Kp)&Kp!=0,
                            c("timeCount","testId","Time","Valveopening","Vset","Flowrate","OutWindT","InWindT","t_out_set","t_return_set","flow_set","Kp","Ti")]%>%#
  mutate(.,para=paste("Kp"=as.character(.$Kp),"Ti"=as.character(.$Ti),sep=","))%>%.[,!names(.)%in%c("Kp","Ti")]%>%
  melt(.,id.var=c("timeCount","testId","Time","para"))%>%#,"Ti","para"
  as.data.table(.)%>%{ #,"InWindT","t_out_set"
    ggplot(data=.[variable %in% c("Valveopening","Vset")],aes(x=timeCount,y=value,color=variable,lty=variable,width=4,group=variable))+
      geom_line()+
      geom_line(data=.[variable %in% c("flow_set","Flowrate")],aes(x=timeCount,y=value*100))+#)+value#"OutWindT",(value-20)*5)
      geom_line(data=.[variable %in% c("InWindT","t_out_set")],aes(x=timeCount,y=value))+
      scale_y_continuous(sec.axis = sec_axis(~./100,name = "Flow rate"))+#./5+20
      facet_wrap(~testId+para,nrow = 3)+
      theme_bw()+theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),#legend.position = c(0.85,0.2),
                       legend.text = element_text(size=16))#
  }


####统计一下各case的情况####
stat.ep.testId<-data.ep.roomResponse.second[,.(
  duration=max(timeLabel,na.rm = TRUE),
  startTout=mean(InWindT[timeCount%in%1:10],na.rm=TRUE),#开始时刻的送风温度
  startTroom=mean(OutWindT[timeCount%in%1:10],na.rm=TRUE),#开始时刻的室内温度
  startFlowrate=mean(Flowrate[timeCount%in%1:10],na.rm=TRUE),#开始时刻的流量
  start
),by=testId]
  
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

ggplot(data=data.ep.valveFlow[!TestId%in%c("T4","RTR_1")],aes(x=Valveopening,y=Flowrate,color=isOn,lty=TestId))+geom_line()+facet_wrap(~TestId,nrow = 2)+
  theme_bw()+theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.text = element_text(size=16))


####室温基本响应####
# sheetName = "温度响应"
data.ep.temp<-as.data.table(read.xlsx(file="SecVer_ResponseTest_20210506.xlsx",sheetIndex =  2))
ggplot(data=data.ep.temp,aes(x=Label,y=Flowrate,color=isOn,lty=TestId))+geom_line()+facet_wrap(~TestId,nrow = 2)


