####本脚本用于阀门阻力变化曲线的拟合####

#有两种选项，一是直接拟合，二是根据物理参数组合拟合
valveResisitancePhyFunc<-function(x,a1,a2,a3,a4,a5){
  a1/((a2*x+a3)^2)+a4*a5^x
}

valveResisitancePhyFunc<-function(x,Wf,R0,Lambda,hor,ver){
  ver+(Wf*R0)/(((1-Lambda)*(x+hor)+Lambda)^2)#+(1-Wf)*R0*Lambda^(2*(x-1))
}

#如果不符合该拟合条件，大部分情况是因为开度过小，流量无法稳定
data.ep.valveFitting<-data.table(read.csv(file="resistanceTest.csv"))
data.ep.valveFitting<-data.ep.roomResponse.second[testId=="MV=0.5_BV=0_F=33_Step"&resistanceS<1&testVset<35&isOn=="Ascending"]
ggplot(data.ep.valveFitting,aes(x=Valveopening,y=resistanceS))+geom_point()
nn<-data.ep.valveFitting[!is.na(resistanceS)&!is.nan(resistanceS)]
#简单的可以nls，但是复杂的容易报错
  nls(resistanceS~valveResisitancePhyFunc(Valveopening,Wf,R0,Lambda,hor,ver),#(Valveopening,a1,a2,a3,a4,a5)
        start = list(Wf=1.251,R0=2,Lambda=-0.06,hor=-2.68,ver=0.06),
        #start = list(a1=0.003,a2=1.001,a3=0.001,a4=-0.001,a5=-0.5),
        data=data.ep.valveFitting)
  
fittingStarter<-selfStart(~valveResisitancePhyFunc(Valveopening,Wf,R0,Lambda),
                          initial = function(){}
                          parameters = c("Wf","R0","Lambda"))
####测试用####
temp.test.fitting<-data.table(x=c(1:10))%>%mutate(.,y=x^2+2*x+1)%>%as.data.table(.)
temp.test.fitting[x==3]$y<-temp.test.fitting[x==3]$y+0.3
testFittingFun<-function(x,a1,a2,a3){
  a1*x^2+a2*x+a3
}
nls(y~testFittingFun(x,a1,a2,a3),data = temp.test.fitting)#nls的文档里提过不能使用无误差的数据进行拟合


####水头拟合####
#使用Grad测试中数据
list.ep.terminalFitting<-list()
data.ep.waterHeadFitting<-data.table(testId="",S2V_b=-999,S2V_k=-999,S2VType="",H2Q_b=-999,H2Q_a=-999,H2QType="")[-1]

nn<-data.ep.valveFlow[testId=="MV=1_BV=0_F=33_Grad"]
nn$ratio<-normalize(nn$Subpressure/nn$Totalpressure)
nn1<-glm(ratio~Valveopening,data=nn,family=binomial(link='logit'))
summary(nn1)

for(i in unique(data.ep.valveFlow[Flowrate!=0]$testId)){
  #拟合末端支路水头的H~Q，用二次两项式
  list.ep.terminalFitting[["H~Q"]][["2Item"]][[i]]<-lm(Subpressure~I(Flowrate^2),data=data.ep.valveFlow[testId==i&Flowrate!=0])
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
summary(lm(H2Q_a~S2V_k,data=data.ep.waterHeadFitting[testId!="MV=1_BV=0.5_F=33_Grad"]))#



temp.test.fitting<-data.ep.valveFlow[testId=="MV=1_BV=0_F=33_Grad"&Flowrate!=0]
#二次三项式
nn2<-nls(Subpressure~a1*Flowrate^2+a2*Flowrate+a3,data=temp.test.fitting,start = list(a1=-1,a2=-0.1,a3=60),
         control = nls.control(maxiter = 1000,tol = 1e-07))
#二次两项式
nn2<-lm(Subpressure~I(Flowrate^2)+I(Flowrate),data=temp.test.fitting)
ggplot(data=temp.test.fitting,aes(x=Flowrate,y=Subpressure))+geom_point()+geom_line()
summary(nn2)



nls(y~a1*x^2+a2*x+a3,data = temp.test.fitting,control = nls.control(maxiter = 1000,tol = 1e-08))


####导出数据用于MATLAB拟合尝试####
write.csv(x=data.ep.roomResponse.second[testId==""&!is.na(testId)&testType=="Step"],file="StepResponseForFitting.csv")


####导入一个正常的数据####



