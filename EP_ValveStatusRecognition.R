####本脚本用于阀门状态识别测试####
# 两基本数据集  MV=0.75_BV=0.25_FluctuationTest #较为理想情况测试
# 实际情况测试  Real_V2O2R_MV=0.75_BV=0.25_NoLim

includeCol<-c("Time","timeCount","testId","Vset","Valveopening","resistanceS","Flowrate","Totalpressure","Subpressure","totalWaterH","subWaterH","isOn")
data.ep.statusTest.ideal<-data.ep.fluctuationTest[!1:10][,..includeCol]
data.ep.statusTest.ideal[isOn=="FALSE"]$isOn<-"Stable"
data.ep.statusTest.ideal$isOn<-as.character(data.ep.statusTest.ideal$isOn)
data.ep.statusTest.normal<-data.ep.realTest[testId=="Real_V2O2R_MV=0.75_BV=0.25_NoLim",..includeCol]

####
data.ep.statusTest.ideal<-data.ep.statusTest.ideal%>%{
  .$diffOneStepValve<-abs(.$Valveopening-c(NA,.$Valveopening))
  .$twoValveMva<-getMovingAverageValue(.$Valveopening,n = 2)
  .$threeValveMva<-getMovingAverageValue(.$Valveopening,n = 3)
  .$fourValveMva<-getMovingAverageValue(.$Valveopening,n = 4)
  .$fiveValveMva<-getMovingAverageValue(.$Valveopening,n = 5)
  .$sixValveMva<-getMovingAverageValue(.$Valveopening,n = 6)
  .$sevenValveMva<-getMovingAverageValue(.$Valveopening,n = 7)
  .$diffTwoValveMva<-as.numeric(.$Valveopening-.$twoValveMva)
  .$diffThreeValveMva<-as.numeric(.$Valveopening-.$threeValveMva)
  .$diffFourValveMva<-as.numeric(.$Valveopening-.$fourValveMva)
  .$diffFiveValveMva<-as.numeric(.$Valveopening-.$fiveValveMva)
  .$diffSixValveMva<-as.numeric(.$Valveopening-.$sixValveMva)
  .$diffSevenValveMva<-as.numeric(.$Valveopening-.$sevenValveMva)
  .$diffValveTarget<-.$Valveopening-.$Vset
  .$isLarger<-(.$Valveopening>c(NA,.$Valveopening))
  .$isStable<-as.character(.$isOn=="Stable")
  .
}
#间接识别打标签
data.ep.statusTest.normal$isDec<-data.ep.statusTest.normal$isOn=="Descending"
data.ep.statusTest.normal[isOn=="Stable"]$isDec<-NA
data.ep.statusTest.normal$isDec<-as.factor(data.ep.statusTest.normal$isDec)



####阀门状态决策树识别模型####
# 上升/下降/稳定
# 测试各逐步平均效果
for(i in c("difftwoValveMva","diffthreeValveMva","difffourValveMva","difffiveValveMva","diffsixValveMva","diffsevenValveMva")){
  #直接识别效果
  pred.ep.statusRecg.valve.dir<-rpart(as.formula(paste("isOn~diffOneStepValve+diffTarget+isLarger+",i,collapse = "")),
                                               data = data.ep.statusTest.normal,cp=0.002)%>%prune(tree = ., cp= .$cptable[which.min(.$cptable[,"xerror"]),"CP"])%>%
                                                      as.party(.)
  plot(pred.ep.statusRecg.valve.dir)
  nn1<-data.ep.statusTest.ideal[!1:6]%>%{
    predictTest(testSet = .,
                resultValue = .$isOn,predictableModel = pred.ep.statusRecg.valve.dir)
  }
  cat(i,"in ideal(Dir):\t",nn1$overall["Accuracy"],"\n")
  nn1<-data.ep.statusTest.normal[!1:6]%>%{
    predictTest(testSet = .,
                resultValue = .$isOn,predictableModel = pred.ep.statusRecg.valve.dir)
  }
  cat(i,"in real(Dir):\t",nn1$overall["Accuracy"],"\n")
  
  #间接识别稳定与否效果
  pred.ep.statusRecg.valve.stable<-as.party(rpart(as.formula(paste("isStable~diffOneStepValve+diffTarget+isLarger+",i,collapse = "")),
                                               data = data.ep.statusTest.normal))
  nn1<-data.ep.statusTest.ideal[!1:6]%>%{
    predictTest(testSet = .,resultValue = .$isStable,predictableModel = pred.ep.statusRecg.valve.stable)
  }
  cat(i,"in ideal(Stable):\t",nn1$overall["Accuracy"],"\n")
  nn1<-data.ep.statusTest.normal[!1:6]%>%{
    predictTest(testSet = .,
                resultValue = .$isStable,predictableModel = pred.ep.statusRecg.valve.stable)
  }
  cat(i,"in real(Stable):\t",nn1$overall["Accuracy"],"\n")
}

####选取效果较好的滑动平均应用####
pred.ep.statusRecg.valve.dir<-rpart(isOn~diffOneStepValve+diffSixValveMva+diffValveTarget,cp=0.005,
                                     data = data.ep.statusTest.normal)%>%prune(tree = ., cp= .$cptable[which.min(.$cptable[,"xerror"]),"CP"])%>%
                                            as.party(.)

plot(pred.ep.statusRecg.valve.dir)
data.ep.statusTest.normal[!1:6]%>%{
  predictTest(testSet = .,
              resultValue = .$isOn,predictableModel = pred.ep.statusRecg.valve.dir)
}


####间接识别####
pred.ep.statusRecg.valve.stable<-as.party(rpart(isStable~diffOneStepValve+diffValveTarget+isLarger+diffFiveValveMva,data = data.ep.statusTest.normal))
plot(pred.ep.statusRecg.valve.stable)
data.ep.statusTest.ideal[!1:6]%>%predictTest(testSet = .,resultValue = .$isStable,predictableModel = pred.ep.statusRecg.valve.stable)
#第一步赋值预测的稳定与否
data.ep.statusTest.normal$predStable<-predict(pred.ep.statusRecg.valve.stable,data.ep.statusTest.normal)
data.ep.statusTest.ideal$predStable<-predict(pred.ep.statusRecg.valve.stable,data.ep.statusTest.ideal)

#测试分步骤预测的多步移动平均
for(i in c("difftwoValveMva","diffthreeValveMva","difffourValveMva","difffiveValveMva","diffsixValveMva","diffsevenValveMva")){
  pred.ep.statusRecg.valve.isOn<-as.party(rpart(as.formula(paste("isDec~diffOneStepValve+diffTarget+isLarger+predStable+",i,collapse = "")),
                                               data = data.ep.statusTest.normal))
  plot(pred.ep.statusRecg.valve.isOn)
  nn1<-data.ep.statusTest.ideal[!1:6]%>%{
    predictTest(testSet = .,
                resultValue = .$isOn,predictableModel = pred.ep.statusRecg.valve.isOn)
  }
  cat(i,"in ideal(inDir):\t",nn1$overall["Accuracy"],"\n")
  nn1<-data.ep.statusTest.normal[!1:6]%>%{
    predictTest(testSet = .,
                resultValue = .$isOn,predictableModel = pred.ep.statusRecg.valve.isOn)
  }
  cat(i,"in real(inDir):\t",nn1$overall["Accuracy"],"\n")
}
pred.ep.statusRecg.valve.isDec<-as.party(rpart(isDec~difffourValveMva +diffOneStepValve+diffTarget+isLarger+predStable,
                                              data = data.ep.statusTest.normal[predStable!=TRUE]))
plot(pred.ep.statusRecg.valve.isDec)
data.ep.statusTest.normal[!1:6][predStable!=TRUE]%>%predictTest(testSet = .,resultValue = .$isDec,predictableModel = pred.ep.statusRecg.valve.isDec)

####合并分步识别的结果####
data.ep.statusTest.ideal$predDec<-predict(pred.ep.statusRecg.valve.isDec,data.ep.statusTest.ideal)
data.ep.statusTest.normal$predDec<-predict(pred.ep.statusRecg.valve.isDec,data.ep.statusTest.normal)
#normal数据集最终整体效果0.8922...跟直接预测没区别
data.ep.statusTest.normal<-data.ep.statusTest.normal%>%{
  .[predStable==TRUE]$predValveStatus<-"Stable"
  .[predStable==FALSE&predDec==TRUE]$predValveStatus<-"Descending"
  .[predStable==FALSE&predDec==FALSE]$predValveStatus<-"Ascending"
  .
}

#直接识别效果输出
data.ep.statusTest.ideal$predValveStatus<-predict(pred.ep.statusRecg.valve.dir,data.ep.statusTest.ideal)#as.character(data.ep.statusTest.normal$predValveStatus)



#####应用于阶跃响应数据#####
# 建立移动平均与前一步误差建立
data.ep.responseTest$predValveStatus<-data.ep.responseTest%>%mutate(.,diffOneStepValve=-999,sixValveMva=-999,diffSixValveMva=-999,predValveStatus="")%>%as.data.table()%>%{
  for(i in unique(.$testId)){
    .[testId==i]$diffOneStepValve<-(.[testId==i]$Valveopening-c(NA,.[testId==i]$Valveopening))
    .[testId==i]$sixValveMva<-getMovingAverageValue(.[testId==i]$Valve,n = 6)
    .[testId==i]$diffSixValveMva<-(.[testId==i]$Valveopening-.[testId==i]$sixValveMva)
  }
  .$diffValveTarget<-abs(.$Valveopening-.$Vset)
  .[complete.cases(.[,c("diffOneStepValve","diffSixValveMva","diffValveTarget")])]$predValveStatus<-
    predict(pred.ep.statusRecg.valve.dir,
            newdata=.[complete.cases(.[,c("diffOneStepValve","diffSixValveMva","diffValveTarget")])])###切记！predict里面传参要预测的话是newdata，或者直接传不指定
  .[diffOneStepValve==-999]$diffOneStepValve<-NA
  .[sixValveMva==-999]$sixValveMva<-NA
  .[diffSixValveMva==-999]$diffSixValveMva<-NA
  .[predValveStatus==""]$predValveStatus<-NA
  .$predValveStatus
}

####统计用可视化####
#单组
ggplot(data=data.ep.statusTest.normal,aes(x=isOn))+geom_boxplot(aes(y=abs(diffMva),color="red"))+geom_boxplot(aes(y=abs(diffTarget),color="blue"))
#多组
data.ep.statusTest.ideal[,c("timeCount","diffthreeValveMva","diffourValveMva","difffiveValveMva","diffsevenValveMva","isLarger","isOn")]%>%melt(.,id.var=c("timeCount","isLarger","isOn"))%>%
  ggplot(data=.,aes(x=variable,y=abs(value),color=(isOn=="Stable")))+geom_boxplot()#+facet_wrap(.~isLarger)#,"diffTarget","threeValveMva"


#数据趋势用可视化
data.ep.realTest[testId=="Real_V2O2R_MV=0.75_BV=0.25_NoLim"][,#][1:100,
    c("testId","timeCount","Flowrate","Valveopening","flowrateStatus")]%>%melt(.,id.var=c("testId","timeCount","flowrateStatus"))%>%{#,"flowrateStatus","fiveFlowrateMva","flowrateStatus"
      ggplot(data = .,aes(x=timeCount,y=value,color=variable))+
        geom_line(data = .[variable=="Flowrate"],aes(x=timeCount,y=value*40))+
        # geom_point(data = .[variable=="fiveFlowrateMva"],aes(x=timeCount,shape=flowrateStatus,y=value*40))+
        geom_point(data = .[variable=="Flowrate"],aes(x=timeCount,y=value*40,shape=flowrateStatus))+#,shape=flowrateStatus
        geom_line(data = .[variable=="Valveopening"],aes(x=timeCount,y=value))
    }#"Vset",,"sixValveMva"
#+
  # geom_line(data = .[variable=="subWaterH"],aes(x=timeCount,y=value*1000))+
