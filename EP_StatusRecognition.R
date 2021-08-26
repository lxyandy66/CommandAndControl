####本脚本用于阀门状态识别测试####
# 两基本数据集  MV=0.75_BV=0.25_FluctuationTest #较为理想情况测试
# 实际情况测试  Real_V2O2R_MV=0.75_BV=0.25_NoLim

includeCol<-c("Time","timeCount","testId","Vset","Valveopening","resistanceS","Flowrate","Totalpressure","Subpressure","totalWaterH","subWaterH","isOn")
data.ep.statusTest.ideal<-nn[!1:10][,..includeCol]
data.ep.statusTest.ideal[isOn=="FALSE"]$isOn<-"Stable"
data.ep.statusTest.normal<-data.ep.recognitionTest[,..includeCol]

####
data.ep.statusTest.ideal<-data.ep.statusTest.ideal%>%{
  .$threeMva<-getMovingAverageValue(.$Valveopening,n = 3)
  .$fourMva<-getMovingAverageValue(.$Valveopening,n = 4)
  .$fiveMva<-getMovingAverageValue(.$Valveopening,n = 5)
  .$sixMva<-getMovingAverageValue(.$Valveopening,n = 6)
  .$diffThreeMva<-as.numeric(.$Valveopening-.$threeMva)
  .$diffFourMva<-as.numeric(.$Valveopening-.$fourMva)
  .$diffFiveMva<-as.numeric(.$Valveopening-.$fiveMva)
  .$diffSixMva<-as.numeric(.$Valveopening-.$sixMva)
  .$diffTarget<-.$Valveopening-.$Vset
  .$isLarger<-(.$Valveopening>c(NA,.$Valveopening))
  .$lowThanTarget<-as.character(.$Valveopening<.$Vset)
  .$isStable<-as.character(.$isOn=="Stable")
  .
}
####阀门状态识别模型####
fit.ep.statusRecon.dt<-rpart(isOn~diffSixMva+diffTarget,data = data.ep.statusTest.normal)
pred.ep.statusRecg.valve<-as.party(fit.ep.statusRecon.dt)
plot(as.party(fit.ep.statusRecon.dt))
data.ep.statusTest.ideal%>%.[complete.cases(.[,c("isOn","diffSixMva","diffTarget")])]%>%{
  predictTest(testSet = .,
              resultValue = .$isOn,predictableModel = as.party(fit.ep.statusRecon.dt))
}
data.ep.statusTest.normal$predValveStatus<-predict(pred.ep.statusRecg.valve,data.ep.statusTest.normal)
data.ep.statusTest.ideal$predValveStatus<-predict(pred.ep.statusRecg.valve,data.ep.statusTest.ideal)
####统计用可视化####
#单组
ggplot(data=data.ep.statusTest.normal,aes(x=isOn))+geom_boxplot(aes(y=abs(diffMva),color="red"))+geom_boxplot(aes(y=abs(diffTarget),color="blue"))
#多组
data.ep.statusTest.normal[,c("timeCount","diffThreeMva","diffFourMva","diffFiveMva","diffSevenMva","isLarger","isOn")]%>%melt(.,id.var=c("timeCount","isLarger","isOn"))%>%
  ggplot(data=.,aes(x=variable,y=abs(value),color=(isOn=="Stable")))+geom_boxplot()#+facet_wrap(.~isLarger)#,"diffTarget","threeMva"


#数据趋势用可视化
data.ep.statusTest.normal[,c("timeCount","Valveopening","Vset","isOn","Flowrate")]%>%melt(.,id.var=c("timeCount","isOn"))%>%
ggplot(data=.,aes(x=timeCount,y=value,color=variable,shape=isOn,group=variable))+geom_line()+geom_point()
