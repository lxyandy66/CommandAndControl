data.ep.raw<-as.data.table(read.csv(file="InnerTuning_20210522.csv"))

#����ֱ��label����Ϊÿ������ִ��labViewʱ�����ã���˿����ظ�
data.ep.raw$timeLabel<-format(data.ep.raw$Time,format="%Y-%m-%d %H:%M:%S")

####�������뼶####
#�����������: Power��Powerset
# Ŀǰֱ��ȡƽ��ֵ
setorder(data.ep.raw,Time)
data.ep.roomResponse.second<-cbind(data.ep.raw[,.(Time=Time[1],ID=ID[1],Label=Label[1]),by=timeLabel],
                      data.ep.raw[,lapply(.SD,mean,na.rm=TRUE),
                                   .SDcols=c("Flowrate","Totalpressure","Subpressure","InWaterT","OutWaterT",
                                                   "InWindT","OutWindT","Valveopening","Vset","Fset","Tset",       
                                                   "Powerset","Fre","HeatingRate",
                                             "t_out_set","t_return_set"),by=timeLabel][,-"timeLabel"])%>%
                mutate(.,Time=as.POSIXct(.$Time))%>%as.data.table()



####����ʱ�����TestId####
data.ep.roomResponse.second$testId<-"prepare"

info.ep.testId<-read.xlsx(file="Info_0522_TestId.xlsx",sheetIndex=1)%>%as.data.table(.)
apply(info.ep.testId[,c("start","end","testId")], MARGIN = 1, function(x){
  data.ep.roomResponse.second[Time %within% interval(start=as.POSIXct(x[1]),end=as.POSIXct(x[2]))]$testId<<-as.character(x[3])
  })
data.ep.roomResponse.second<-merge(x=data.ep.roomResponse.second,
                                   y=info.ep.testId[,c("testId","Kp","Ti")],all.x = TRUE,by = "testId")



data.ep.roomResponse.second[testId=="K2_3"]%>%ggplot(data=.,aes(x=Label,y=InWindT))+geom_line()

data.ep.roomResponse.second[Kp==4&!is.na(Kp),
                            c("Label","testId","Time","Valveopening","Vset","Flowrate","OutWindT","InWindT","t_out_set","Kp","Ti")]%>%
  mutate(.,para=paste("Kp"=Kp,"Ti"=Ti,sep=","))%>%
  melt(.,id.var=c("Label","testId","Time","Ti","para"))%>%
  as.data.table(.)%>%{
    ggplot(data=.[variable %in% c("Valveopening","Vset")],aes(x=Label,y=value,color=variable,lty=variable,width=2,group=variable))+
      geom_line()+
      geom_line(data=.[variable %in% c("OutWindT","InWindT","t_out_set")],aes(x=Label,y=(value-20)/0.4))+
      scale_y_continuous(sec.axis = sec_axis(~.*0.4+20,name = "Temperature"))+
      facet_wrap(~para+testId,nrow = 2)+
      theme_bw()+theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),
                       legend.text = element_text(size=16),legend.position = c(0.85,0.2))#
    }
  
#����뼶����
write.xlsx(data.ep.roomResponse.second,file="SecVer_RoomResponse_20210507.xlsx")

####��һ�·��ŵĿ���####
#sheetName = "���ſ�����Ӧ"
data.ep.valveFlow<-as.data.table(read.csv(file="SecVer_ResponseOnOffTest_20210506.csv"))%>%
  mutate(.,TestId=as.factor(.$TestId),isOn=(.$from<.$to))%>%as.data.table(.)%>%.[complete.cases(.)]

ggplot(data=data.ep.valveFlow[!TestId%in%c("T4","RTR_1")],aes(x=Valveopening,y=Flowrate,color=isOn,lty=TestId))+geom_line()+facet_wrap(~TestId,nrow = 2)+
  theme_bw()+theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.text = element_text(size=16))


####���»�����Ӧ####
# sheetName = "�¶���Ӧ"
data.ep.temp<-as.data.table(read.xlsx(file="SecVer_ResponseTest_20210506.xlsx",sheetIndex =  2))
ggplot(data=data.ep.temp,aes(x=Label,y=Flowrate,color=isOn,lty=TestId))+geom_line()+facet_wrap(~TestId,nrow = 2)


