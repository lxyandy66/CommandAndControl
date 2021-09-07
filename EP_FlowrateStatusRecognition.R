####本脚本用于流量与压力状态识别测试####
# 数据集与阀门状态识别测试相同
# 两基本数据集  MV=0.75_BV=0.25_FluctuationTest #较为理想情况测试
# 实际情况测试  Real_V2O2R_MV=0.75_BV=0.25_NoLim


####打标####
data.ep.realTest$subPressureStatus<-as.character(NA)
data.ep.fluctuationTest[186:186]$subPressureStatus<-"Increasing"
data.ep.fluctuationTest[393:394]$subPressureStatus<-"Decreasing"
data.ep.fluctuationTest[185:186]$subPressureStatus<-"Stable"
####手动label一下####

data.ep.statusTest.normal<-data.ep.realTest[testId=="Real_V2O2R_MV=0.75_BV=0.25_NoLim",c(..includeCol,"subPressureStatus")]
data.ep.statusTest.ideal<-data.ep.fluctuationTest[,c(..includeCol,"subPressureStatus")]

####3-sigma方法####
# 用于阶跃测试和阶梯测试都还不错，比决策树好
# 流量识别
sd(data.ep.statusTest.normal[350:450]$Flowrate) #稳态sd为0.003469357
data.ep.responseTest[timeCount%in%c(600:800)&testId=="MV=1_BV=0.5_F=33_Grad"]%>%
  {sd(.$Flowrate,na.rm = TRUE)} #稳态sd: 0.75/0.25=0.008135075;0.75/0=0.0113142;0.75/0=0.009382877
flowrateStableSd<-0.005
# 压力识别
sd(data.ep.statusTest.normal[350:450]$Subpressure) #稳态sd为0.2975711
data.ep.responseTest[timeCount%in%c(560:660)&testId=="MV=1_BV=0.25_F=33_Step"]%>%
  {sd(.$Totalpressure,na.rm = TRUE)} #稳态sd: 0.75/0.25=0.2308988;0.75/0=0.3076749;0.75/0.5=0.0400008;1/0.25Step=0.12,0.13
subPressureStableSd<-0.075
# 批量打标
data.ep.realTest$predTotPresStatus<-
  data.ep.realTest%>%mutate(.,fivePressureMva=as.numeric(NA),predTotPresStatus=as.character(NA))%>%as.data.table()%>%{
    for(i in unique(.$testId)){
      .[testId==i]$fivePressureMva<-getMovingAverageValue(.[testId==i]$Totalpressure,n=3)
      .[testId==i]$predTotPresStatus<-apply(X = .[testId==i][,c("fivePressureMva","Totalpressure")],MARGIN = 1,FUN = function(x){
        if(anyNA(x)){return(NA)}
        # browser()
        if(x[2]>(x[1]+3*subPressureStableSd)){return("Increasing")}
        if(x[2]<(x[1]-3*subPressureStableSd)){return("Decreasing")}
        else return("Stable")
      })
    }
    .$predTotPresStatus
  }

data.ep.fluctuationTest$subPressureStatus<-
  data.ep.fluctuationTest%>%mutate(.,fivePressureMva=as.numeric(NA))%>%as.data.table()%>%{
      .$fivePressureMva<-getMovingAverageValue(.$Subpressure,n=5)
      .$subPressureStatus<-apply(X = .[,c("fivePressureMva","Subpressure")],MARGIN = 1,FUN = function(x){
        if(anyNA(x)){return(NA)}
        # browser()
        if(x[2]>(x[1]+3*subPressureStableSd)){return("Increasing")}
        if(x[2]<(x[1]-3*subPressureStableSd)){return("Decreasing")}
        else return("Stable")
      })
    .$subPressureStatus
  }


####实际实验打标测试####
####流量打标####
data.ep.statusTest.ideal<-data.ep.statusTest.ideal%>%{
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

ggplot(data.ep.statusTest.ideal,aes(x=Flowrate,y=sdThreeFlowrate))+geom_point()

####不同时间间隔性能比较####
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

####选取合适的时间间隔####
pred.ep.statusRecg.flowrate<-rpart(flowrateStatus~diffOneStepFlowrate+diffSevenFlowrateMva+sdSevenFlowrate,
                                   data = data.ep.statusTest.ideal,cp=0.002)%>%prune(tree = ., cp= .$cptable[which.min(.$cptable[,"xerror"]),"CP"])%>%as.party(.)
plot(pred.ep.statusRecg.flowrate)
data.ep.statusTest.ideal[!1:6]%>%{
  predictTest(testSet = .,resultValue = .$flowrateStatus,predictableModel = pred.ep.statusRecg.flowrate)
}
data.ep.statusTest.normal[!1:6]%>%{
  predictTest(testSet = .,resultValue = .$flowrateStatus,predictableModel = pred.ep.statusRecg.flowrate)
}

####实际用预测####
temp.inputFlowrateCol<-c("diffOneStepFlowrate","diffSevenFlowrateMva","sdSevenFlowrate")
data.ep.responseTest$predFlowrateStatus<-data.ep.responseTest%>%
  mutate(.,predFlowrateStatus=as.character(NA),diffOneStepFlowrate=as.numeric(NA),
         diffSevenFlowrateMva=as.numeric(NA),sdSevenFlowrate=as.numeric(NA))%>%
  mutate_all(funs(ifelse(is.nan(.), NA, .)))%>%
  as.data.table(.)%>%{
  for(i in unique(.$testId)){
    if(anyNA(.[testId==i]$Flowrate)){
      .[testId==i]$Flowrate<-na.approx(.[testId==i]$Flowrate)
    }
    .[testId==i]$diffOneStepFlowrate<-abs(.[testId==i]$Flowrate-c(NA,.[testId==i]$Flowrate))
    .[testId==i]$diffSevenFlowrateMva<-as.numeric(.[testId==i]$Flowrate-getMovingAverageValue(.[testId==i]$Flowrate,n = 7))
    .[testId==i]$sdSevenFlowrate<-runSD(.[testId==i]$Flowrate,n = 7)
    .[testId==i][complete.cases(.[testId==i][,..temp.inputFlowrateCol])]$predFlowrateStatus<-predict(pred.ep.statusRecg.flowrate,
                                             newdata=.[testId==i][complete.cases(.[testId==i][,..temp.inputFlowrateCol])])
  }
  .$predFlowrateStatus
}



####压力打标########

data.ep.statusTest.ideal<-data.ep.statusTest.ideal%>%{
  .$diffOneStepSubPre<-abs(.$Subpressure-c(NA,.$Subpressure))
  .$diffTwoSubPreMva<-as.numeric(.$Subpressure-getMovingAverageValue(.$Subpressure,n = 2))
  .$diffThreeSubPreMva<-as.numeric(.$Subpressure-getMovingAverageValue(.$Subpressure,n = 3))
  .$diffFourSubPreMva<-as.numeric(.$Subpressure-getMovingAverageValue(.$Subpressure,n = 4))
  .$diffFiveSubPreMva<-as.numeric(.$Subpressure-getMovingAverageValue(.$Subpressure,n = 5))
  .$diffSixSubPreMva<-as.numeric(.$Subpressure-getMovingAverageValue(.$Subpressure,n = 6))
  .$diffSevenSubPreMva<-as.numeric(.$Subpressure-getMovingAverageValue(.$Subpressure,n = 7))
  .$sdTwoSubPre<-runSD(.$Subpressure,n = 2)
  .$sdThreeSubPre<-runSD(.$Subpressure,n = 3)
  .$sdFourSubPre<-runSD(.$Subpressure,n = 4)
  .$sdFiveSubPre<-runSD(.$Subpressure,n = 5)
  .$sdSixSubPre<-runSD(.$Subpressure,n = 6)
  .$sdSevenSubPre<-runSD(.$Subpressure,n = 7)
  .
}

ggplot(data.ep.statusTest.ideal,aes(x=Subpressure,y=sdThreeSubPre))+geom_point()

####不同时间间隔性能比较####
for(i in c( "diffTwoSubPreMva","diffThreeSubPreMva","diffFourSubPreMva","diffFiveSubPreMva","diffSixSubPreMva","diffSevenSubPreMva")){
  #直接识别效果
  for(j in c("sdTwoSubPre","sdThreeSubPre","sdFourSubPre","sdFiveSubPre","sdSixSubPre","sdSevenSubPre")){
    pred.ep.statusRecg.subpressure<-rpart(as.formula(paste("subPressureStatus~diffOneStepSubPre",i,j,sep = "+")),
                                          data = data.ep.statusTest.normal,cp=0.002)%>%prune(tree = ., cp= .$cptable[which.min(.$cptable[,"xerror"]),"CP"])%>%as.party(.)
    plot(pred.ep.statusRecg.subpressure)
    nn1<-data.ep.statusTest.ideal[!1:6]%>%{
      predictTest(testSet = .,
                  resultValue = .$subPressureStatus,predictableModel = pred.ep.statusRecg.subpressure)
    }
    cat(i,"  ",j,"in ideal:\t",nn1$overall["Accuracy"],"\n")
    nn1<-data.ep.statusTest.normal[!1:6]%>%{
      predictTest(testSet = .,
                  resultValue = .$subPressureStatus,predictableModel = pred.ep.statusRecg.subpressure)
    }
    cat(i,"  ",j,"in real:\t",nn1$overall["Accuracy"],"\n")
  }
}
# 综合来看5mva+4sd比较好
pred.ep.statusRecg.subpressure<-rpart(subPressureStatus~diffOneStepSubPre+diffFiveSubPreMva+sdFourSubPre,
                                   data = data.ep.statusTest.normal,cp=0.002)%>%prune(tree = ., cp= .$cptable[which.min(.$cptable[,"xerror"]),"CP"])%>%as.party(.)
plot(pred.ep.statusRecg.subpressure)
data.ep.statusTest.ideal[!1:6]%>%{
  predictTest(testSet = .,resultValue = .$subPressureStatus,predictableModel = pred.ep.statusRecg.subpressure)
}
data.ep.statusTest.normal[!1:6]%>%{
  predictTest(testSet = .,resultValue = .$subPressureStatus,predictableModel = pred.ep.statusRecg.subpressure)
}
temp.inputSubpressureCol<-c("diffOneStepSubPre","diffFiveSubPreMva","sdFourSubPre")

data.ep.realTest$predSubPresStatus<-data.ep.realTest%>%
  mutate(.,predSubPresStatus=as.character(NA),diffOneStepSubPre=as.numeric(NA),
         diffFiveSubPreMva=as.numeric(NA),sdFourSubPre=as.numeric(NA))%>%
  mutate_all(funs(ifelse(is.nan(.), NA, .)))%>%
  as.data.table(.)%>%{
    for(i in unique(.$testId)){
      if(anyNA(.[testId==i]$Subpressure)){
        cat("NA detected \n")
        .[testId==i]$Subpressure<-na.approx(.[testId==i]$Subpressure)
      }
      .[testId==i]$diffOneStepSubPre<-abs(.[testId==i]$Subpressure-c(NA,.[testId==i]$Subpressure))
      .[testId==i]$diffFiveSubPreMva<-as.numeric(.[testId==i]$Subpressure-getMovingAverageValue(.[testId==i]$Subpressure,n = 5))
      .[testId==i]$sdFourSubPre<-runSD(.[testId==i]$Subpressure,n = 4)
      .[testId==i][complete.cases(.[testId==i][,..temp.inputSubpressureCol])]$predSubPresStatus<-predict(pred.ep.statusRecg.subpressure,
                                                                                                       newdata=.[testId==i][complete.cases(.[testId==i][,..temp.inputSubpressureCol])])
    }
    .$predSubPresStatus
    # browser()
  }

# [,-c("ID","Label","timeLabel","InWaterT","OutWaterT","InWindT","OutWindT","Fset","Tset","t_out_set","t_return_set","Powerset","Fre","HeatingRate")]

