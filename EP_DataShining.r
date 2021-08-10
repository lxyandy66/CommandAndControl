data.ep.raw<-as.data.table(read.csv(file="OriginalData/ModiTestId_StepResponseForRecognition_20210625.csv"))

#����ֱ��label����Ϊÿ������ִ��labViewʱ�����ã���˿����ظ�
data.ep.raw$Time<-as.POSIXct(data.ep.raw$Time)
data.ep.raw$timeLabel<-format(data.ep.raw$Time,format="%Y-%m-%d %H:%M:%S")


#��һ�·ֲ���ȥ��ԭʼ���ݵ��쳣ֵ
ggplot(data.ep.raw,aes(x=Flowrate))+geom_density()
ggplot(data.ep.raw,aes(x=1,y=Flowrate))+geom_boxplot()

data.ep.raw<-data.ep.raw%>%{
    .[Totalpressure>200]$Totalpressure<-NA #
    .[Subpressure>200]$Subpressure<-NA #
    .[Flowrate>10]$Flowrate<-NA #
    .[InWaterT<10|InWaterT>80]$InWaterT<-NA # #�м�|�ᶼ�Ƚ������жϣ���||ֻ��Ƚ�һ�������㼴����
    .[OutWaterT<10|OutWaterT>80]$OutWaterT<-NA #
    .[InWindT<10]$InWindT<-NA
    .[OutWindT<10]$OutWindT<-NA
    .[Fre>100]$Fre<-NA #
    .
}


####�������뼶####
#�����������: Power��Powerset
# Ŀǰֱ��ȡƽ��ֵ
setorder(data.ep.raw,Time)
#ע�⣡getMode����ͬ���ִ���ʱ���ܷ��ض����ֵ
data.ep.roomResponse.second<-cbind(data.ep.raw[,.(Time=Time[1],ID=ID[1],Label=Label[1],
                                                  testId=getMode(testId,na.rm = TRUE)[1],
                                                  Vset=getMode(Vset,na.rm = TRUE)[1],
                                                  testVset=getMode(testVset,na.rm = TRUE)[1],#���Ų�����
                                                  isOn=getMode(isOn,na.rm = TRUE)[1],#���Ų�����
                                                  testType=getMode(testType,na.rm = TRUE)[1],#���Ų�����
                                                  onRoutine=getMode(onRoutine,na.rm = TRUE)[1]#���Ų����ã�onRoutine����Grad���Զ��ԣ������׶�ΪTRUE����֮ΪFALSE
                                                  #ע�����ʱ�����⣬������������ƽ�����������ֶ�����û���⣬���Ǵ����Զ���ʱ��
                                                  ),by=timeLabel],
                      data.ep.raw[,lapply(.SD,mean,na.rm=TRUE),
                                   .SDcols=c("Flowrate","Totalpressure","Subpressure","InWaterT","OutWaterT",
                                                   "InWindT","OutWindT","Valveopening","Fset","Tset",  #"Vset"  ,   
                                             "t_out_set","t_return_set","flow_set",
                                             "Powerset","Fre","HeatingRate"),by=timeLabel][,-"timeLabel"])%>%
                mutate(.,Time=as.POSIXct(.$Time))%>%as.data.table()
View(table(data.ep.roomResponse.second$Vset))



####����ʱ�����TestId####
data.ep.roomResponse.second$testId<-"prepare"
info.ep.testId<-read.xlsx(file="Info_TestId.xlsx",sheetName = "0602")%>%as.data.table(.)
apply(info.ep.testId[,c("start","end","testId")], MARGIN = 1, function(x){
  data.ep.roomResponse.second[Time %within% interval(start=as.POSIXct(x[1]),end=as.POSIXct(x[2]))]$testId<<-as.character(x[3])
  })
data.ep.roomResponse.second<-merge(x=data.ep.roomResponse.second,
                                   y=info.ep.testId[,c("testId","Kp","Ti")],all.x = TRUE,by = "testId")


####����TestId���������label####
#ԭ���ĳ���label��һ���������label��Ӧһ����1sһ����2s
data.ep.roomResponse.second$timeCount<- -999
for(i in unique(data.ep.roomResponse.second[testId!="prepare"]$testId)){
  data.ep.roomResponse.second[testId==i]$timeCount<-0:(nrow(data.ep.roomResponse.second[testId==i])-1)
}


####ͨ��������������####

#����ˮͷ
data.ep.roomResponse.second<-data.ep.roomResponse.second%>%
                                mutate(.,totalWaterH=.$Totalpressure/9807,subWaterH=.$Subpressure/9807)%>%
                                as.data.table(.)
#����֧·����ϵ��
data.ep.roomResponse.second$resistanceS<-data.ep.roomResponse.second$subWaterH/(data.ep.roomResponse.second$Flowrate*data.ep.roomResponse.second$Flowrate)
data.ep.roomResponse.second[is.infinite(resistanceS)]$resistanceS<-NA

####����-��������####
#�������д���
data.ep.roomResponse.second$isOn<-as.character(data.ep.roomResponse.second$isOn)
data.ep.roomResponse.second[isOn=="TRUE"&Valveopening<Vset]$isOn<-"Ascending"
data.ep.roomResponse.second[isOn=="TRUE"&Valveopening>Vset]$isOn<-"Descending"

#Grad����ȡ��ֵ
data.ep.valveFlow<-data.ep.roomResponse.second[testType=="Grad",
                                               .(testId=testId[1],
                                                 Vset=Vset[1],
                                                 onRoutine=onRoutine[1],
                                                 resistanceS=mean(resistanceS,na.rm=TRUE),
                                                 Flowrate=mean(Flowrate,na.rm=TRUE),
                                                 Valveopening=mean(Valveopening,na.rm=TRUE),
                                                 Subpressure=mean(Subpressure,na.rm=TRUE),
                                                 Totalpressure=mean(Totalpressure,na.rm=TRUE)
                                                 ),by=(testIdVsetDir=paste(testId,Vset,onRoutine,sep = "_"))]
#ƽ������ͻ�����
#ѡ��Grad�����൱��ȡƽ����ʵ����StepҲ����������ֻ�Ǵ��������Ƚ��鷳
####�˴�����������������H��ȷ��####
ggplot(data.ep.valveFlow,aes(x=Flowrate,y=Totalpressure,color=testId,shape=onRoutine))+geom_point()+geom_line()
stat.ep.valveFlow<-data.ep.valveFlow[,.(maxHR=(Flowrate[onRoutine==TRUE]-Flowrate[onRoutine==FALSE])
                                        ),by=(testIdVset=paste(testId,Vset,sep = "_"))]

####���ӻ�####
data.ep.roomResponse.second[testId=="Klow_1.1"]%>%
  ggplot(data=.,aes(x=Label,y=t_out_set,color=testId,lty=testId))+
  geom_line()+geom_line(aes(x=Label,y=InWindT))+geom_line(aes(x=Label,y=Flowrate*50))+facet_wrap(~testId,nrow = 3)

data.ep.roomResponse.second[!is.na(testId)&testType=="Step"&timeCount&testVset<=20,#&testId=="MV=1_BV=0.25_F=33_Step""prepare", #&!is.na(Kp)&Kp!=0,,"V2O_1","V2O_2","V2O_3",!testId%in%c("prepare","Casc_Low_1","Casc_Low_2")
                            c("timeCount","testId","Time","Valveopening","Vset","Flowrate","testVset")]%>%#,,"OutWindT","InWindT","t_out_set","t_return_set","Kp","Ti","flow_set"
  #mutate(.,para=paste("Kp"=as.character(.$Kp),"Ti"=as.character(.$Ti),sep=","))%>%.[,!names(.)%in%c("Kp","Ti")]%>% 
  melt(.,id.var=c("timeCount","testId","testVset"))%>%#,"Ti","para"
  as.data.table(.)%>%{ #,"InWindT","t_out_set"
    ggplot(data=.[variable %in% c("Valveopening","Vset","testVset")],aes(x=timeCount,y=value,lty=testId,color=variable,width=4,group=paste(testId,variable)))+
      geom_line()+
      geom_line(data=.[variable %in% c("Flowrate")],aes(x=timeCount,y=value*100))+#)+value#"OutWindT",(value-20)*5)
      # geom_line(data=.[variable %in% c("InWindT","t_out_set")],aes(x=timeCount,y=value))+
      scale_y_continuous(sec.axis = sec_axis(~./100,name = "Flow rate"))+#./5+20
      facet_wrap(~testVset,nrow = 4)+
      theme_bw()+theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.position = c(0.25,0.75),
                       legend.text = element_text(size=16))#
  }

#�������ӻ�
#ͬһ������ͬ��Ծ�����Ա�
data.ep.roomResponse.second[!is.na(testId)&testId=="MV=1_BV=0_F=33_Step"&resistanceS<0.2&testVset<35,
                            c("timeCount","Time","testId","Valveopening","Subpressure","Totalpressure","resistanceS","testVset","Flowrate","isOn")]%>%#
  #melt(.,id.var=c("timeCount","testId","Time","Vset"))%>%
  ggplot(data = .,aes(x=Valveopening,y=resistanceS,color=as.factor(testVset),mapping = timeCount))+
  geom_point(aes(alpha=0.5,shape=isOn))+
  geom_path()+
  facet_wrap(~as.factor(testVset),nrow = 3)#Valveopening

#��ͬ���������Ա�
data.ep.roomResponse.second[!is.na(testId)&testType=="Step"&resistanceS<0.2&Valveopening<30&testVset<35&testId!="MV=1_BV=0.5_F=33_Step",#
                            c("timeCount","Time","testId","Valveopening","testVset","Flowrate","Totalpressure","Subpressure","resistanceS","isOn")]%>%
  #melt(.,id.var=c("timeCount","testId","Time","Vset"))%>%
  ggplot(data = .,aes(x=Flowrate,y=Subpressure,color=as.factor(testVset),mapping = timeCount,group=paste(testId,testVset,isOn)))+
  geom_point(aes(alpha=0.5,shape=isOn))+
  geom_path(aes(lty=as.factor(testVset)))+facet_wrap(~as.factor(testId),nrow = 2)+
  theme_bw()+theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.text = element_text(size=16))

#������ճ��
data.ep.roomResponse.second[!is.na(testId)&testVset==10&testType=="Step",#testVset=5,7,10
                            c("timeCount","Time","testId","Valveopening","testVset","Flowrate")]%>%#,"isOn"
  ggplot(data = .,aes(x=timeCount,y=Flowrate,color=testId))+
  geom_point(alpha=0.6)+
  geom_path()+
  facet_wrap(~as.factor(testVset)+testId,nrow = 3)

#ˮͷ��ʧ
data.ep.roomResponse.second[timeCount<3000&testId=="MV=1_BV=0.25_F=33_Step"&testVset==30,#testVset=5,7,10
                            c("timeCount","Time","subWaterH","Flowrate","Valveopening","resistanceS")]%>%
  melt(.,id.var=c("timeCount","Time"))%>%{
    ggplot(data = .,aes(x=timeCount,y=value,color=variable))+
      geom_line(data = .[variable=="Flowrate"],aes(x=timeCount,y=value*10))+
      geom_line(data = .[variable=="subWaterH"],aes(x=timeCount,y=value*1000))+
      geom_line(data = .[variable=="resistanceS"],aes(x=timeCount,y=value*10))+
      geom_line(data = .[variable=="Valveopening"],aes(x=timeCount,y=value/10))+ylim(c(0,10))
  }#,"isOn"

# ѹǿ����
# ͨ��ѹǿ����ȷ�������͵Ĺ���
#ѹ��ռ��
data.ep.roomResponse.second[!is.na(testId)&testId!=""&testType!="Grad"&testVset<30&testVset>10,#testVset=5,7,10 &timeCount<3000&timeCount>2500&testId=="MV=1_BV=0.25_F=33_Step"&testId=="MV=0.5_BV=0_F=33_Step"
                            c("timeCount","Time","testId","testType","Subpressure","Totalpressure","Flowrate","Valveopening","resistanceS","isOn","testVset")]%>%{
    ggplot(data = .)+
      geom_point(aes(x=Valveopening,y=Totalpressure,color=testId,shape=isOn),alpha=0.5)+
      # geom_line(aes(x=timeCount,y=Subpressure,color=testId),lty="dashed")+
      # geom_line(aes(x=timeCount,y=Totalpressure,color=testId))+
      # geom_line(aes(x=timeCount,y=Valveopening,color=testId),lty="dotted")+
      # geom_line(aes(x=timeCount,y=Flowrate*100,color=testId),lty="longdash")+
      facet_wrap(.~testVset,nrow=2)+#ylim(c(0.5,1.1))+
      theme_bw()+theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.text = element_text(size=16))
  }#,"isOn"



####���һ�·��������������ֲ�������####
nn1<-(data.ep.roomResponse.second[!is.na(testId)&testId=="MV=1_BV=0_F=33_Step"&testVset==30])
nn1[,c("ID","testType","Totalpressure","totalWaterH","subWaterH","timeLabel",
       "Subpressure","InWaterT","OutWaterT","InWindT","OutWindT","Fset","Tset","t_out_set","t_return_set","flow_set","Powerset","Fre","HeatingRate")]<-NULL
nn1$onRoutine<-as.character(nn1$isOn)
ggplot(data=nn1[resistanceS<0.2],aes(x=timeCount,y=resistanceS))+geom_point()

data.ep.roomResponse.second[testType=="Step"&testVset==Vset&isOn=="FALSE"&!is.na(resistanceS)&resistanceS<0.1&testVset<35]%>%
  ggplot(data=.,aes(x=testId,y=resistanceS,color=as.factor(Vset)))+geom_boxplot()

####ͳ�Ƹ���Ծ�����ȶ�ʱ�Ĳ������####
stat.ep.valveStable<-data.ep.roomResponse.second[testType=="Step"&testVset==Vset&isOn=="FALSE",.(
  statTestId=testId[1],
  Vset=Vset[1],
  lowWsk=-999,#ֱ��boxplotȫ�ջ᷵��logical
  highWsk=-999,#�ô���
  meanResistance=mean(resistanceS,na.rm=TRUE),
  sdResistance=sd(resistanceS,na.rm=TRUE),
  meanFlowrate=mean(Flowrate,na.rm=TRUE),
  sdFlowrate=sd(Flowrate,na.rm=TRUE)
),by=paste(testId,testVset,sep = "_")]
getMatch<-function(x,num){ boxplot.stats(data.ep.roomResponse.second[testId==x[1]&Vset==as.integer(x[2])]$resistanceS)$stats[num] }

stat.ep.valveStable[!is.nan(meanResistance)]<-stat.ep.valveStable[!is.nan(meanResistance)]%>%
  mutate(.,lowWsk=apply(.[,c("statTestId","Vset")], MARGIN = 1,FUN = getMatch,num=1),
         highWsk=apply(.[,c("statTestId","Vset")], MARGIN = 1,FUN = getMatch,num=5))
stat.ep.valveStable$statTestId<-as.character(stat.ep.valveStable$statTestId)

stat.ep.valveStable$stableRange<-stat.ep.valveStable$highWsk-stat.ep.valveStable$lowWsk

####�������仯ʶ��˷�����
#����������ԣ����ǻ��в���ż����������Ĳ���ʶ��
nn1[isOn %in% c("Descending")]$onRoutine<-apply(nn1[isOn=="Descending",c("resistanceS","testId","testVset")],MARGIN = 1,
                                         FUN = function(x){
                                           
                                           if(is.na(x[1])){
                                             return(NA)
                                           }#�мǶ�Ԫ�ص�&������&&
                                           range<-as.numeric(stat.ep.valveStable[statTestId==x[2]&Vset==as.numeric(x[3]),c("lowWsk","highWsk")])
                                           if(anyNA(range)){
                                             return(NA)
                                           }
                                           if(x[1]>range[1]&x[1]<range[2]){
                                             return("Descending_overcome")
                                           }else
                                             return(NA)
                                         })
for(i in unique(nn1$testId)){
  maxStable<-max(stat.ep.valveStable[statTestId==i&Vset]$highWsk)[1]
  minStable<-min(stat.ep.valveStable[statTestId==i]$lowWsk)[1]
  range<-range(nn1[isOn %in% c("Descending")&resistanceS<maxStable&resistanceS>minStable]$Valveopening)
  nn1[isOn %in% c("Descending")&Valveopening>range[1]&Valveopening<range[2]]$onRoutine<-"Descending_overcome"
}
nn1[isOn %in% c("Descending")&
      resistanceS<max(stat.ep.valveStable[statTestId%in%testId]$highWsk)[1]&
      resistanceS>min(stat.ep.valveStable[statTestId%in%testId]$lowWsk)[1]]$onRoutine<-"Descending_overcome"

####�������仯�˷�����
# ����΢����һ��
nn1[isOn %in% c("Descending")&Flowrate>max(Flowrate)*0.95]$onRoutine<-"Descending_overcome"
ggplot(data=nn1,aes(x=Valveopening,y=resistanceS,color=onRoutine))+geom_point()+ylim(c(0,0.5))


####�˷������������ǩ####
for(i in unique(data.ep.roomResponse.second[testType=="Step"]$testId)){
  data.ep.roomResponse.second[]
}


  
####ͳ��һ�¸�case�����####
nn<-temp.ep.pre[timeCount<600&testId=="V2F_4"][timeCount %in% c((max(timeCount)-100):max(timeCount))]
mean(temp.ep.pre[timeCount<600&testId=="V2F_4"][timeCount%in% c(max(timeCount)-100:max(timeCount))]$InWindT,na.rm=TRUE)

stat.ep.testId<-temp.ep.pre[timeCount<600,.(
  duration=max(timeLabel,na.rm = TRUE),
  peakime=timeCount[InWindT==max(InWindT,na.rm = TRUE)][1],
  startTout=mean(InWindT[timeCount%in%1:10],na.rm=TRUE),#��ʼʱ�̵��ͷ��¶�
  startTroom=mean(OutWindT[timeCount%in%1:10],na.rm=TRUE),#��ʼʱ�̵������¶�
  startFlowrate=mean(Flowrate[timeCount%in%1:10],na.rm=TRUE),#��ʼʱ�̵�����
  meanValveBias=mean(abs(Valveopening-Vset),na.rm=TRUE),
  meanLastTout=mean(InWindT[timeCount%in% c((length(timeCount)-100):length(timeCount))],na.rm = TRUE),#���100s�Ĳ������#(length(timeCount)-60:length(timeCount))
  sdLastTout=sd(InWindT[timeCount%in% c((length(timeCount)-100):length(timeCount))],na.rm = TRUE),#���100s�Ĳ������
  maxTout=max(InWindT,na.rm = TRUE),
  overshoot=(max(InWindT,na.rm = TRUE)/mean(InWindT[timeCount%in% c((length(timeCount)-100):length(timeCount))],na.rm = TRUE))-1
),by=testId]%>%
  merge(x=.,y=info.ep.testId[,c("testId","Kp","Ti")],all.x = TRUE,by = "testId")


  
##�鿴���2min�仯
data.ep.roomResponse.second[testId=="OrgCascWithSetpoint"]%>%
  .[(nrow(nn)-120):nrow(nn),c("Time","OutWindT","t_return_set")]%>%#,"t_out_set""OutWindT",
  melt(data=.,id.var=c("Time"))%>%
  ggplot(data=.,aes(x=variable,y=value,color=variable))+geom_boxplot()+ylim(25,30)


#����뼶����
write.xlsx(data.ep.roomResponse.second,file="SecVer_RoomResponse_20210507.xlsx")

####��һ�·��ŵĿ���####
#sheetName = "���ſ�����Ӧ"
data.ep.valveFlow<-as.data.table(read.csv(file="OriginalData/SecVer_ResponseOnOffTest_20210506.csv"))%>%
  mutate(.,TestId=as.factor(.$TestId),isOn=(.$from<.$to))%>%as.data.table(.)%>%.[complete.cases(.)]

ggplot(data=data.ep.valveFlow[TestId%in%c("2","3","4","5","6","RTR_2","T2")|(TestId=="T1"&isOn==TRUE)],aes(x=Valveopening,y=Flowrate,color=isOn,lty=TestId))+geom_line()+facet_wrap(~TestId,nrow = 2)+
  theme_bw()+theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.text = element_text(size=16))


####���»�����Ӧ####
# sheetName = "�¶���Ӧ"
data.ep.temp<-as.data.table(read.xlsx(file="SecVer_ResponseTest_20210506.xlsx",sheetIndex =  2))
ggplot(data=data.ep.temp,aes(x=Label,y=Flowrate,color=isOn,lty=TestId))+geom_line()+facet_wrap(~TestId,nrow = 2)


