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
for(i in unique(data.ep.valveFlow[Flowrate!=0]$testId)){
  #拟合末端支路水头的H~Q
  list.ep.terminalFitting[[i]][["H~Q"]][["2Item"]]<-lm(Subpressure~I(Flowrate^2),data=data.ep.valveFlow[testId==i&Flowrate!=0])
  # 拟合末端支路中
  list.ep.terminalFitting[[i]][["SubRatio~Valve"]][["linear"]]<-
    lm(I(Subpressure/Totalpressure)~Valveopening,data=data.ep.valveFlow[testId==i&Valveopening<30])
  
  cat(i,"\t","H~Q",list.ep.terminalFitting[[i]][["H~Q"]][["2Item"]]$coefficients,
      "R~V",list.ep.terminalFitting[[i]][["SubRatio~Valve"]][["linear"]]$coefficients,"\n")
}


temp.test.fitting<-data.ep.valveFlow[testId=="MV=1_BV=0_F=33_Grad"&Flowrate!=0]
nn2<-nls(Subpressure~a1*Flowrate^2+a2*Flowrate+a3,data=temp.test.fitting,start = list(a1=-1,a2=-0.1,a3=60),
         control = nls.control(maxiter = 1000,tol = 1e-07))
nn2<-lm(Subpressure~I(Flowrate^2)+I(Flowrate),data=temp.test.fitting)
ggplot(data=temp.test.fitting,aes(x=Flowrate,y=Subpressure))+geom_point()+geom_line()
summary(nn2)



nls(y~a1*x^2+a2*x+a3,data = temp.test.fitting,control = nls.control(maxiter = 1000,tol = 1e-08))


####导出数据用于MATLAB拟合尝试####
write.csv(x=data.ep.roomResponse.second[testId==""&!is.na(testId)&testType=="Step"],file="StepResponseForFitting.csv")


####导入一个正常的数据####



