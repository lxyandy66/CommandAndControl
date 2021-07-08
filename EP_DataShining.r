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
                                                  onRoutine=getMode(onRoutine,na.rm = TRUE)[1]#���Ų�����
                                                  #ע�����ʱ�����⣬������������ƽ�����������ֶ�����û���⣬���Ǵ����Զ���ʱ��
                                                  ),by=timeLabel],
                      data.ep.raw[,lapply(.SD,mean,na.rm=TRUE),
                                   .SDcols=c("Flowrate","Totalpressure","Subpressure","InWaterT","OutWaterT",
                                                   "InWindT","OutWindT","Valveopening","Fset","Tset",  #"Vset"  ,   
                                             "t_out_set","t_return_set","flow_set",
                                             "Powerset","Fre","HeatingRate"),by=timeLabel][,-"timeLabel"])%>%
                mutate(.,Time=as.POSIXct(.$Time))%>%as.data.table()
View(table(data.ep.roomResponse.second$Vset))

data.ep.roomResponse.second$isOn<-data.ep.roomResponse.second$Valveopening<data.ep.roomResponse.second$Vset


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


####����-��������####
#Grad����ȡ��ֵ
data.ep.valveFlow<-data.ep.roomResponse.second[testType=="Grad",
                                               .(testId=testId[1],
                                                 Vset=Vset[1],
                                                 onRoutine=onRoutine[1],
                                                 Flowrate=mean(Flowrate,na.rm=TRUE),
                                                 Valveopening=mean(Valveopening,na.rm=TRUE)
                                                 ),by=(testIdVsetDir=paste(testId,Vset,onRoutine,sep = "_"))]
#ƽ������ͻ�����
ggplot(data.ep.valveFlow,aes(x=Vset,y=Flowrate,color=testId,shape=onRoutine))+geom_point()+geom_line()
stat.ep.valveFlow<-data.ep.valveFlow[,.(maxHR=(Flowrate[onRoutine==TRUE]-Flowrate[onRoutine==FALSE]),
                                        ),by=(testIdVset=paste(testId,Vset,sep = "_"))]

####���ӻ�####
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

#�������ӻ�
#��Ծ���ͻ�
data.ep.roomResponse.second[!is.na(testId)&testId=="MV=0.5_BV=0_F=33_Step"&testVset<40,
                            c("timeCount","Time","testId","Valveopening","testVset","Flowrate","Totalpressure","Subpressure")]%>%#,"isOn"
  #melt(.,id.var=c("timeCount","testId","Time","Vset"))%>%
  ggplot(data = .,aes(x=Valveopening,y=Flowrate,color=as.factor(testVset)))+
  geom_point(alpha=0.6)+
  geom_path(aes(mapping = timeCount))+
  facet_wrap(~as.factor(testVset),nrow = 2)#Valveopening

#��ͬ�������ͻ�
data.ep.roomResponse.second[!is.na(testId)&testType=="Step"&testVset==60,
                            c("timeCount","Time","testId","Valveopening","testVset","Flowrate","Totalpressure","Subpressure")]%>%#,"isOn"
  #melt(.,id.var=c("timeCount","testId","Time","Vset"))%>%
  ggplot(data = .,aes(x=Valveopening,y=Flowrate,color=as.factor(testId)))+
  geom_point(alpha=0.6)+
  geom_path(aes(mapping = timeCount))#+
  facet_wrap(~as.factor(testVset),nrow = 2)#Valveopening

#������ճ��
data.ep.roomResponse.second[!is.na(testId)&testId=="MV=0.5_BV=0_F=33_Step"&Vset.1<12,
                            c("timeCount","Time","testId","Valveopening","Vset.1","Flowrate","Totalpressure","Subpressure")]%>%#,"isOn"
  ggplot(data = .,aes(x=timeCount,y=Flowrate,color=as.factor(Vset.1)))+
  geom_point(alpha=0.6)+
  geom_path()+
  facet_wrap(~as.factor(Vset.1),nrow = 3)


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


