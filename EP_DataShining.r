data.ep.raw<-as.data.table(read.csv(file="InnerTuning_20210522.csv"))

#不能直接label，因为每次重新执行labView时会重置，因此可能重复
data.ep.raw$timeLabel<-format(data.ep.raw$Time,format="%Y-%m-%d %H:%M:%S")

####整合至秒级####
#可能有问题的: Power和Powerset
# 目前直接取平均值
setorder(data.ep.raw,Time)
data.ep.roomResponse.second<-cbind(data.ep.raw[,.(Time=Time[1],ID=ID[1],Label=Label[1]),by=timeLabel],
                      data.ep.raw[,lapply(.SD,mean,na.rm=TRUE),
                                   .SDcols=c("Flowrate","Totalpressure","Subpressure","InWaterT","OutWaterT",
                                                   "InWindT","OutWindT","Valveopening","Vset","Fset","Tset",       
                                                   "Powerset","Fre","HeatingRate",
                                             "t_out_set","t_return_set"),by=timeLabel][,-"timeLabel"])%>%
                mutate(.,Time=as.POSIXct(.$Time))%>%as.data.table()



####根据时间分配TestId####
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
  
#输出秒级数据
write.xlsx(data.ep.roomResponse.second,file="SecVer_RoomResponse_20210507.xlsx")

####看一下阀门的开关####
#sheetName = "阀门开关响应"
data.ep.valveFlow<-as.data.table(read.csv(file="SecVer_ResponseOnOffTest_20210506.csv"))%>%
  mutate(.,TestId=as.factor(.$TestId),isOn=(.$from<.$to))%>%as.data.table(.)%>%.[complete.cases(.)]

ggplot(data=data.ep.valveFlow[!TestId%in%c("T4","RTR_1")],aes(x=Valveopening,y=Flowrate,color=isOn,lty=TestId))+geom_line()+facet_wrap(~TestId,nrow = 2)+
  theme_bw()+theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.text = element_text(size=16))


####室温基本响应####
# sheetName = "温度响应"
data.ep.temp<-as.data.table(read.xlsx(file="SecVer_ResponseTest_20210506.xlsx",sheetIndex =  2))
ggplot(data=data.ep.temp,aes(x=Label,y=Flowrate,color=isOn,lty=TestId))+geom_line()+facet_wrap(~TestId,nrow = 2)


