####本脚本用于流量状态识别测试####
# 数据集与阀门状态识别测试相同
# 两基本数据集  MV=0.75_BV=0.25_FluctuationTest #较为理想情况测试
# 实际情况测试  Real_V2O2R_MV=0.75_BV=0.25_NoLim


####打标####
data.ep.statusTest.ideal$flowrateStatus<-as.character(NA)
data.ep.fluctuationTest[191:204]$flowrateStatus<-"Increasing"
data.ep.fluctuationTest[191:204]$flowrateStatus<-"Decreasing"
data.ep.fluctuationTest[204:232]$flowrateStatus<-"Stable"
##as.character(NA)#Stable
data.ep.statusTest.normal<-data.ep.realTest[testId=="Real_V2O2R_MV=0.75_BV=0.25_NoLim",c(..includeCol,"flowrateStatus")]
data.ep.statusTest.ideal<-data.ep.fluctuationTest[,c(..includeCol,"flowrateStatus")]

####3-sigma方法####
# 效果不是很好，部分2-3s跳跃容易识别错误
sd(data.ep.statusTest.normal[350:450]$Flowrate) #稳态sd为0.003469357

data.ep.statusTest.normal<-
  data.ep.statusTest.normal%>%{
    .$fiveFlowrateMva<-getMovingAverageValue(.$Flowrate,n=6)
    .$flowrateStatus<-apply(X = .[,c("fiveFlowrateMva","Flowrate")],MARGIN = 1,FUN = function(x){
      if(anyNA(x)){return(NA)}
      # browser()
      if(x[2]>(x[1]+3*flowrateStableSd)){return("Increasing")}
      if(x[2]<(x[1]-3*flowrateStableSd)){return("Decreasing")}
      else return("Stable")
    })
    .
  }

data.ep.statusTest.normal<-data.ep.statusTest.normal%>%{
  .$diffOneStepFlowrate<-abs(.$Flowrate-c(NA,.$Flowrate))
  .$diffTwoFlowrateMva<-as.numeric(.$Flowrate-getMovingAverageValue(.$Flowrate,n = 2))
  .$diffThreeFlowrateMva<-as.numeric(.$Flowrate-getMovingAverageValue(.$Flowrate,n = 3))
  .$diffFourFlowrateMva<-as.numeric(.$Flowrate-getMovingAverageValue(.$Flowrate,n = 4))
  .$diffFiveFlowrateMva<-as.numeric(.$Flowrate-getMovingAverageValue(.$Flowrate,n = 5))
  .$diffSixFlowrateMva<-as.numeric(.$Flowrate-getMovingAverageValue(.$Flowrate,n = 6))
  .$diffSevenFlowrateMva<-as.numeric(.$Flowrate-getMovingAverageValue(.$Flowrate,n = 7))
  .$sdTwoFlowrate<-runSD(.$Flowrate,n = 2)
  .$sdThreeFlowrate<-runSD(.$Flowrate,n = 3)
  .$sdFourFlowrate<-runSD(.$Flowrate,n = 4)
  .$sdFiveFlowrate<-runSD(.$Flowrate,n = 5)
  .$sdSixFlowrate<-runSD(.$Flowrate,n = 6)
  .$sdSevenFlowrate<-runSD(.$Flowrate,n = 7)
  .
}

ggplot(data.ep.statusTest.normal,aes(x=Flowrate,y=sdThreeFlowrate))+geom_point()

####选取合适的时间间隔####
for(i in c( "diffTwoFlowrateMva","diffThreeFlowrateMva","diffFourFlowrateMva","diffFiveFlowrateMva","diffSixFlowrateMva","diffSevenFlowrateMva")){
  #直接识别效果
  for(j in c("sdTwoFlowrate","sdThreeFlowrate","sdFourFlowrate","sdFiveFlowrate","sdSixFlowrate","sdSevenFlowrate")){
    pred.ep.statusRecg.flowrate<-rpart(as.formula(paste("flowrateStatus~diffOneStepFlowrate",i,j,sep = "+")),
                                        data = data.ep.statusTest.normal,cp=0.002)%>%prune(tree = ., cp= .$cptable[which.min(.$cptable[,"xerror"]),"CP"])%>%as.party(.)
    plot(pred.ep.statusRecg.flowrate)
    nn1<-data.ep.statusTest.ideal[!1:6]%>%{
      predictTest(testSet = .,
                  resultValue = .$flowrateStatus,predictableModel = pred.ep.statusRecg.flowrate)
    }
    cat(i,"  ",j,"in ideal:\t",nn1$overall["Accuracy"],"\n")
    nn1<-data.ep.statusTest.normal[!1:6]%>%{
      predictTest(testSet = .,
                  resultValue = .$flowrateStatus,predictableModel = pred.ep.statusRecg.flowrate)
    }
    cat(i,"  ",j,"in real:\t",nn1$overall["Accuracy"],"\n")
  }
}

pred.ep.statusRecg.flowrate<-rpart(flowrateStatus~diffOneStepFlowrate+diffFiveFlowrateMva+sdThreeFlowrate,
                                   data = data.ep.statusTest.normal,cp=0.002)%>%prune(tree = ., cp= .$cptable[which.min(.$cptable[,"xerror"]),"CP"])%>%as.party(.)
plot(pred.ep.statusRecg.flowrate)

