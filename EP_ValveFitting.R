####本脚本用于阀门阻力变化曲线的拟合####

#有两种选项，一是直接拟合，二是根据物理参数组合拟合


valveResisitancePhyFunc<-function(x,a1,a2,a3,a4,a5){
  a1/((a2*x+a3)^2)+a4*a5^x
}

valveResisitancePhyFunc<-function(x,Wf,R0,Lambda,hor,ver){
  ver+(Wf*R0)/(((1-Lambda)*(x+hor)+Lambda)^2)#+(1-Wf)*R0*Lambda^(2*(x-1))
}

#简单的可以nls，但是复杂的容易报错
# 放弃，使用MATLAB识别
  nls(resistanceS~valveResisitancePhyFunc(Valveopening,Wf,R0,Lambda,hor,ver),#(Valveopening,a1,a2,a3,a4,a5)
        start = list(Wf=1.251,R0=2,Lambda=-0.06,hor=-2.68,ver=0.06),
        #start = list(a1=0.003,a2=1.001,a3=0.001,a4=-0.001,a5=-0.5),
        data=data.ep.recognitionTest[isOn=="Ascending"])#nls的文档里提过不能使用无误差的数据进行拟合
  
fittingStarter<-selfStart(~valveResisitancePhyFunc(Valveopening,Wf,R0,Lambda),
                          initial = function(){}
                          parameters = c("Wf","R0","Lambda"))

####水头拟合测试####
# 测试二次模型的可靠性
#使用Grad测试中数据进行拟合
list.ep.terminalFitting<-list()
data.ep.waterHeadFitting<-data.table(testId="",S2V_b=-999,S2V_k=-999,S2VType="",H2Q_b=-999,H2Q_a=-999,H2QType="")[-1]

for(i in unique(data.ep.valveFlow[Flowrate!=0]$testId)){
  #拟合末端支路水头的H~Q，用二次两项式
  list.ep.terminalFitting[["H~Q"]][["2Item"]][[i]]<-lm(subWaterH~I(Flowrate^2),data=data.ep.valveFlow[testId==i&Flowrate!=0])
  # 拟合末端支路中阀门上压降比例和阀门开度规律
  list.ep.terminalFitting[["SubRatio~Valve"]][["linear"]][[i]]<-
    lm(I(Subpressure/Totalpressure)~Valveopening,data=data.ep.valveFlow[testId==i&Valveopening<30])
  data.ep.waterHeadFitting<-rbind(data.ep.waterHeadFitting,
                                  data.table(testId=i,
                                             S2V_b=list.ep.terminalFitting[["SubRatio~Valve"]][["linear"]][[i]]$coefficients[1],
                                             S2V_k=list.ep.terminalFitting[["SubRatio~Valve"]][["linear"]][[i]]$coefficients[2],
                                             S2VType="linear",
                                             H2Q_b=list.ep.terminalFitting[["H~Q"]][["2Item"]][[i]]$coefficients[1],
                                             H2Q_a=list.ep.terminalFitting[["H~Q"]][["2Item"]][[i]]$coefficients[2],
                                             H2QType="2Item"))
  cat(i,"\t","H~Q",list.ep.terminalFitting[["H~Q"]][["2Item"]][[i]]$coefficients,
      "R~V",list.ep.terminalFitting[["SubRatio~Valve"]][["linear"]][[i]]$coefficients,"\n")
  
}

#### 尝试使用测试的支管压力/总压力比例对水利工况识别 ####
# 效果不好，不如直接识别
data.ep.waterHeadFitting$mvOpening<-getSplitMember(data.ep.waterHeadFitting$testId,"_",1,singleUse = FALSE)
data.ep.waterHeadFitting$bvOpening<-getSplitMember(data.ep.waterHeadFitting$testId,"_",2,singleUse = FALSE)
ggplot(data.ep.waterHeadFitting)+
  geom_point(aes(x=S2V_b,y=H2Q_b,color=mvOpening,shape=bvOpening))

#H~Q S~V关系拟合
#二次显著+I(S2V_b)
fit.ep.H2Q.b<-lm(H2Q_b~I(S2V_b^2)+S2V_b,data=data.ep.waterHeadFitting)#从实验数据拟合出的整个规律
summary(fit.ep.H2Q.b)

fit.ep.H2Q.a<-lm(H2Q_a~S2V_k+H2Q_b,data=data.ep.waterHeadFitting[bvOpening!="BV=0.5"])#从实验数据拟合出的整个规律
summary(fit.ep.H2Q.a)#I(S2V_k^2)+H2Q_b

# 二次模型H～Q拟合适用性验证
fit.ep.fun.h2q<-lm(subWaterH~I(Flowrate^2)+Flowrate,data=data.ep.valveFlow[testId=="MV=1_BV=0.25_F=33_Grad"])
summary(fit.ep.fun.h2q)
nn<-data.ep.valveFlow[testId=="MV=1_BV=0.25_F=33_Grad"]%>%mutate(.,estSubWaterH=predict(fit.ep.fun.h2q,.[,"Flowrate"]))%>%as.data.table()
ggplot(nn)+geom_point(aes(x=Flowrate,y=subWaterH))+geom_line(aes(x=Flowrate,y=estSubWaterH,color="green"))+
  theme_bw()+theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.text = element_text(size=16))+ylim(c(0,0.008))
####导出数据用于MATLAB拟合尝试####
write.csv(x=data.ep.roomResponse.second[testId==""&!is.na(testId)&testType=="Step"],file="StepResponseForFitting.csv")


####导入一个正常的数据####
data.ep.recognitionTest<-data.ep.realTest[testId=="Real_V2O2R_MV=0.75_BV=0.25_NoLim"]
data.ep.realTest$isOn<-"Stable"
data.ep.realTest[testId=="Real_V2O2R_MV=0.75_BV=0.25_NoLim"][472:478]$isOn<-"Ascending"
data.ep.realTest[testId=="Real_V2O2R_MV=0.75_BV=0.25_NoLim"][43:50]$isOn<-"Descending"

# 数据中原始情况
ggplot(data=data.ep.recognitionTest,aes(x=Flowrate,y=subWaterH,color=isOn))+geom_point()+geom_line()

#### 支路压降占比与阀门开度关系拟合 ####
#这一部分会用到案例的情况
# 但是实际上用压降比拟合结果似乎还没直接拟合来的好
fit.ep.local.s2v<-lm(I(Subpressure/Totalpressure)~Valveopening,data = data.ep.recognitionTest)
summary(fit.ep.local.s2v)
fit.ep.local.h2q.b<-predict(fit.ep.H2Q.b,data.table(S2V_b=fit.ep.local.s2v$coefficients["(Intercept)"]))#获取H=aQ^2+b中b

fit.ep.local.h2q.a<-predict(fit.ep.H2Q.a,data.table(S2V_k=fit.ep.local.s2v$coefficients["Valveopening"],
                                                    H2Q_b=fit.ep.local.h2q.b))#获取H=aQ^2+b中a

fun.ep.local.h2q<-function(q){return(fit.ep.local.h2q.a*q^2+fit.ep.local.h2q.b)}
fun.ep.h2s<-function(q,h){return(h/(q^2))}

####直接拟合H2Q####
# 最终使用
fit.ep.local.dirH2q<-lm(subWaterH~I(Flowrate^2)+Flowrate,data = data.ep.recognitionTest)
summary(fit.ep.local.dirH2q)

# 拟合预计表格

####水头拟合及评估####

data.ep.local.predict<-  data.table(Flowrate=seq(0,1,0.05),subWaterHeadSupply=-999,resistanceSet=-999)
# 达到流量设定点对应所需要的水头
data.ep.local.predict$subWaterHeadSupply<-predict(fit.ep.local.dirH2q,
                                                  data.table(Flowrate=data.ep.local.predict$Flowrate))
# 达到流量设定点对应所需要阻力
data.ep.local.predict$resistanceSet<-fun.ep.h2s(data.ep.local.predict$Flowrate,data.ep.local.predict$subWaterHeadSupply)


ggplot()+geom_point(data=data.ep.local.predict,aes(x=Flowrate,y=subWaterHeadSupply,color="red"))+
  geom_line(data=data.ep.local.predict,aes(x=Flowrate,y=subWaterHeadSupply,color="red"))+
  # geom_point(data=data.ep.local.predict,aes(x=Flowrate,y=subWaterHEstDir,color="grey"))+
  # geom_line(data=data.ep.local.predict,aes(x=Flowrate,y=subWaterHEstDir,color="grey"))+
  geom_point(data=data.ep.local.standard,aes(x=Flowrate,y=subWaterH,color="blue",shape=onRoutine))+
  geom_line(data=data.ep.local.standard,aes(x=Flowrate,y=subWaterH,color="blue",lty=onRoutine))+
  geom_point(data=data.ep.recognitionTest,aes(x=Flowrate,y=subWaterH,color="green"))+
  geom_path(data=data.ep.recognitionTest,aes(x=Flowrate,y=subWaterH,color="green"))+
  ylim(c(0,0.008))+theme_bw()+theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.text = element_text(size=16))


# 选出数据供MATLAB识别
# 其中阀门移动但阻力不变，即克服迟滞的部分不计入迟滞拟合

#### 阀门开度～阻力模型适用性验证####
# 阶跃测试数据供拟合
data.ep.responseTest[testId=="MV=1_BV=0.25_F=33_Step"&!is.na(resistanceS)&testVset>15]%>%write.csv(.,file="StepTestForFitting.csv")
# MATLAB得到系数后作图
info.ep.local.fittedResponseTest<-data.table(Wf=c(-0.002845,0.02251),R0=c(-0.03052,0.02748),Lambda=c(-0.02996,-0.0891))#MV/BV=1/0.25 0.96/0.978 不开robust
  # data.table(Wf=c(0.006769,0.002521),R0=c(0.03056,0.2377),Lambda=c(-0.01296,-0.0903))#开robustV/BV=1/0.25 0.99/0.99
# 与阶跃对比
data.ep.resistanceFitting.valid<-data.ep.responseTest[testId=="MV=1_BV=0.25_F=33_Step"&!is.na(resistanceS)&testVset>15]
data.ep.resistanceFitting.valid[,c("ID","Label","InWaterT","OutWaterT","InWindT","OutWindT","Fset",
                                   "Tset","t_out_set","t_return_set","flow_set","Powerset","Fre","HeatingRate")]<-NULL
data.ep.resistanceFitting.valid$estResistanceS<- -999
for(i in unique(data.ep.resistanceFitting.valid$isOn)){
  if(!is.na(i)&i!=""&i!="FALSE"){
    data.ep.resistanceFitting.valid[isOn==i]$estResistanceS<-valveResisitancePhyFunc(data.ep.resistanceFitting.valid[isOn==i]$Valveopening,
                                                                                     i=="Descending",mappingTable = info.ep.local.fittedResponseTest)
  }
}
data.ep.resistanceFitting.valid[estResistanceS==-999]$estResistanceS<-NA
ggplot(data.ep.resistanceFitting.valid[resistanceS<0.8&estResistanceS<0.8&isOn!="FALSE"&Valveopening<100&Valveopening>80,
          c("Valveopening","isOn","resistanceS","estResistanceS","testVset")],aes(color=as.factor(testVset),shape=isOn))+geom_point(aes(x=Valveopening,y=resistanceS,alpha=0.5))+
  geom_line(aes(x=Valveopening,y=estResistanceS,lty=isOn,color="green"))+
  theme_bw()+theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.text = element_text(size=16))



write.csv(data.ep.recognitionTest,file = "RealTestForFitting.csv")


nn<-read.csv(file="RealTestForFitting_removed.csv")%>%mutate(.,valveSlot=Valveopening*(1+runif(nrow(.),0,0.01)))%>%as.data.table(.)
nn[Valveopening>10&isOn=="Descending"]$valveSlot<-round(nn[Valveopening>10&isOn=="Descending"]$Valveopening*2)/2
nn[Valveopening>17&isOn=="Ascending"]$valveSlot<-round(nn[Valveopening>17&isOn=="Ascending"]$Valveopening*2)/2
nn1<-nn[,.(logCount=length(timeLabel),
           Vset=mean(Vset,na.rm=TRUE),
           resistanceS=mean(resistanceS,na.rm=TRUE),
           Valveopening=valveSlot[1],
           subWaterH=mean(subWaterH,na.rm=TRUE)),by=.(valveSlot,isOn)]
write.csv(nn1,file="RealTestForFitting_removed_combined.csv")

ggplot(nn1,aes(x=Valveopening,y=resistanceS,color=isOn))+geom_point()

#下降 上升
info.ep.local.fittedR2V<-data.table(Wf=c(0.0003768,0.001141),R0=c(0.4809,0.4531),Lambda=c(-0.02829,-0.08164))#合并 0.01噪声 带上升添加（下降段很好
  # data.table(Wf=c(0.8632,1.742),R0=c(0.0005815,0.001168),Lambda=c(0.002499,0.001373))#带起始点参数 0.69/0.9

# 合并 0.01噪声 未添加data.table(Wf=c(0.0008397,0.001306),R0=c(0.346,0.3908),Lambda=c(-0.01294,-0.08229))
# 合并 不加噪声data.table(Wf=c(0.0003964,0.001618),R0=c(0.794,0.3185),Lambda=c(-0.009108,-0.08122))
#未将后段重复数据合并 data.table(Wf=c(0.0008938,0.0002913),R0=c(0.4367,2.201),Lambda=c(-0.001793,-0.0598))

valveResisitancePhyFunc<-function(valveOpening,isDecrease=FALSE,mappingTable=info.ep.local.fittedR2V){
  index=ifelse(isDecrease,1,2)
  Wf=mappingTable$Wf[index]
  R0=mappingTable$R0[index]
  Lambda=mappingTable$Lambda[index]
  (Wf*R0)/(((1-Lambda)*(valveOpening/100)+Lambda)^2)#+(1-Wf)*R0*Lambda^(2*(x-1))
}
valveResisitanceInversePhyFunc<-function(resistanceSet,isDecrease=FALSE,mappingTable=info.ep.local.fittedR2V){
  index=ifelse(isDecrease,1,2)
  Wf=mappingTable$Wf[index]
  R0=mappingTable$R0[index]
  Lambda=mappingTable$Lambda[index]
  return(100*(sqrt(Wf*R0/resistanceSet)-Lambda)/(1-Lambda))
}

fun.ep.local.forwardModel<-function(Q,valveOpening,isDecrease=FALSE,mappingTable=info.ep.local.fittedR2V){
  return(predict(fit.ep.local.dirH2q,
                 data.table(Flowrate=Q))-valveResisitancePhyFunc(valveOpening,isDecrease,mappingTable)*Q^2)
}
fun.ep.local.forwardModel.direct<-function(Q,resistance){
  return(predict(fit.ep.local.dirH2q,data.table(Flowrate=Q))-resistance*Q^2)
}

#反向模型
data.ep.local.predict<-data.ep.local.predict%>%mutate(.,VopenDes=valveResisitanceInversePhyFunc(resistanceSet,TRUE),
                                                      VopenAsc=valveResisitanceInversePhyFunc(resistanceSet,FALSE))
ggplot(data.ep.local.predict)+
  geom_point(aes(x=VopenDes,y=Flowrate,color="red"))+geom_line(aes(x=VopenDes,y=Flowrate,color="red"))+
  geom_point(aes(x=VopenAsc,y=Flowrate,color="green"))+geom_line(aes(x=VopenAsc,y=Flowrate,color="green"))+
  geom_point(data=data.ep.recognitionTest[isOn!="Stable"],aes(x=Valveopening,y=Flowrate,alpha=0.5,color="blue",shape=isOn))+ylim(c(0,1))+
  theme_bw()+theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.text = element_text(size=16))

#正向模型，开度-阻力-流量
data.ep.local.predict.valveResistance<-data.table(Valveopening=seq(0,100))%>%
  mutate(.,resistanceDec=valveResisitancePhyFunc(Valveopening,TRUE),
         resistanceAsc=valveResisitancePhyFunc(Valveopening,FALSE))%>%as.data.table(.)
data.ep.local.predict.valveResistance[Valveopening<8]$resistanceAsc<-NA
data.ep.local.predict.valveResistance[Valveopening<3]$resistanceDec<-NA

data.ep.local.predict.valveResistance$flowrateAsc<-apply(X = data.ep.local.predict.valveResistance[,"Valveopening"],MARGIN = 1,FUN = function(x){
  return(uniroot(fun.ep.local.forwardModel,c(0,20),valveOpening=x,isDecrease=FALSE,mappingTable = info.ep.local.fittedR2V)$root)
})


#合并标准测试中的阻力
data.ep.local.predict.valveResistance<-merge(x=data.ep.local.predict.valveResistance,#为什么要用data.ep.resistanceFitting.valid
                                             y=data.ep.local.standard[onRoutine==TRUE,c("Vset","resistanceS")],by.x="Valveopening",by.y="Vset",all.x=TRUE)
data.ep.local.predict.valveResistance$resistanceOnStd<-data.ep.local.predict.valveResistance$resistanceS
data.ep.local.predict.valveResistance$resistanceS<-NULL
data.ep.local.predict.valveResistance<-merge(x=data.ep.local.predict.valveResistance,
                                             y=data.ep.local.standard[onRoutine==FALSE,c("Vset","resistanceS")],by.x="Valveopening",by.y="Vset",all.x=TRUE)
data.ep.local.predict.valveResistance$resistanceOffStd<-data.ep.local.predict.valveResistance$resistanceS
data.ep.local.predict.valveResistance$resistanceS<-NULL
data.ep.local.predict.valveResistance$flowrateDecStd<-apply(X=data.ep.local.predict.valveResistance[,"resistanceOffStd"],MARGIN = 1,
                                                            FUN = function(x){
                                                              if(is.na(x)|is.nan(x)){return(NA)}
                                                              uniroot(fun.ep.local.forwardModel.direct,c(0,20),resistance=x)$root
                                                            })
data.ep.local.predict.valveResistance$resistanceOnStd<-valveResisitancePhyFunc(data.ep.local.predict.valveResistance$Valveopening,
                                                                               isDecrease = FALSE,mappingTable = info.ep.local.fittedResponseTest)
data.ep.local.predict.valveResistance$resistanceOffStd<-valveResisitancePhyFunc(data.ep.local.predict.valveResistance$Valveopening,
                                                                               isDecrease = TRUE,mappingTable = info.ep.local.fittedResponseTest)


##阻力评估
ggplot()+
  geom_point(data=data.ep.responseTest[testId=="MV=0.75_BV=0.25_F=33_Grad"],
             aes(x=Valveopening,y=resistanceS,alpha=0,color="grey"))+
  geom_point(data=data.ep.recognitionTest,#nn,#data.ep.responseTest[testId=="MV=0.75_BV=0.25_F=33_Step"&resistanceS<1],
             aes(x=Valveopening,y=resistanceS,color="blue"))+
  geom_point(data=data.ep.local.predict.valveResistance,aes(x=Valveopening,y=resistanceAsc,color="red"))+
  geom_line(data=data.ep.local.predict.valveResistance,aes(x=Valveopening,y=resistanceAsc,color="red"))+
  geom_point(data=data.ep.local.predict.valveResistance,aes(x=Valveopening,y=resistanceDec,color="green"))+
  geom_line(data=data.ep.local.predict.valveResistance,aes(x=Valveopening,y=resistanceDec,color="green"))+
  theme_bw()+theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.text = element_text(size=16))+ylim(c(0,0.75))+xlim(c(0,50))

##阀门-流量曲线
# 去掉一些异常点
ggplot(data=data.ep.local.predict.valveResistance%>%{
  .[Valveopening<8]$flowrateAsc<-NA
  .[Valveopening<3]$flowrateDec<-NA
  .}%>%
    .[,c("Valveopening","flowrateAsc","flowrateDec","flowrateAscStd","flowrateDecStd")]%>%
         melt(.,id.var="Valveopening")%>%.[complete.cases(.)])+
  geom_point(data=data.ep.responseTest[testId=="MV=0.75_BV=0.25_F=33_Grad"],aes(x=Valveopening,y=Flowrate,color="GradTest",alpha=0.01))+
  geom_point(aes(x=Valveopening,y=value,color=variable,group=variable))+geom_line(aes(x=Valveopening,y=value,color=variable,group=variable))+
  # geom_point(data=data.ep.recognitionTest[Valveopening<50],aes(x=Valveopening,y=Flowrate,color="blue"))+
  theme_bw()+theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.text = element_text(size=16))+ylim(c(0,1))

