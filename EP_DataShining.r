##数据导入的一些预处理
data.ep.raw<-as.data.table(read.csv(file="OriginalData/RealTest_V2O2R_MV=0_BV=0_20210803.csv"))

#适用于响应测试用
data.ep.raw$testVset<-data.ep.raw$note
data.ep.raw$note<-NULL
data.ep.raw<-data.ep.raw[testId!=""&!is.na(testId)]
#截取判断测试的类型
data.ep.raw$testType<-"Real"#apply(X = data.ep.raw[,"testId"],MARGIN = 1,FUN = function(x){substring(x,first = nchar(x)-3)})


#不能直接label，因为每次重新执行labView时会重置，因此可能重复
data.ep.raw$Time<-as.POSIXct(data.ep.raw$Time)#注意如果之前用excel基本处理之后，时间格式可能不能统一
data.ep.raw$timeLabel<-format(data.ep.raw$Time,format="%Y-%m-%d %H:%M:%S")
data.ep.raw$testId<-as.character(data.ep.raw$testId)
data.ep.raw$testType<-as.character(data.ep.raw$testType)

#看一下分布，去除原始数据的异常值
ggplot(data.ep.raw,aes(x=Flowrate))+geom_density()
ggplot(data.ep.raw,aes(x=1,y=Flowrate))+geom_boxplot()

data.ep.raw<-data.ep.raw%>%{
    .[Totalpressure>200]$Totalpressure<-NA #
    .[Subpressure>200]$Subpressure<-NA #
    .[Flowrate>10]$Flowrate<-NA #
    .[InWaterT<10|InWaterT>80]$InWaterT<-NA # #切记|会都比较两个判断，但||只会比较一个，满足即返回
    .[OutWaterT<10|OutWaterT>80]$OutWaterT<-NA #
    .[InWindT<10]$InWindT<-NA
    .[OutWindT<10]$OutWindT<-NA
    .[Fre>100]$Fre<-NA #
    .
}
data.ep.raw<-data.ep.raw[testId!=""&!is.na(testId)]

####整合至秒级####
#可能有问题的: Power和Powerset
# 目前直接取平均值
setorder(data.ep.raw,Time)
#注意！getMode有相同出现次数时可能返回多个数值
data.ep.roomResponse.second<-cbind(data.ep.raw[,.(Time=Time[1],ID=ID[1],Label=Label[1],
                                                  testId=getMode(testId,na.rm = TRUE)[1],
                                                  Vset=getMode(Vset,na.rm = TRUE)[1]
                                                  # testVset=getMode(testVset,na.rm = TRUE)[1],#阀门测试用
                                                  # isOn=getMode(isOn,na.rm = TRUE)[1],#阀门测试用
                                                  # testType=getMode(testType,na.rm = TRUE)[1],#阀门测试用
                                                  # onRoutine=getMode(onRoutine,na.rm = TRUE)[1]#阀门测试用，onRoutine对于Grad测试而言，上升阶段为TRUE，反之为FALSE
                                                  #注意采样时间问题，是用众数还是平均数，对于手动众数没问题，考虑串级自动的时候
                                                  ),by=timeLabel],
                      data.ep.raw[,lapply(.SD,mean,na.rm=TRUE),
                                   .SDcols=c("Flowrate","Totalpressure","Subpressure","InWaterT","OutWaterT",
                                                   "InWindT","OutWindT","Valveopening","Fset","Tset",  #"Vset"  ,   
                                             "t_out_set","t_return_set","flow_set",
                                             "Powerset","Fre","HeatingRate"),by=timeLabel][,-"timeLabel"])%>%
                mutate(.,Time=as.POSIXct(.$Time))%>%as.data.table()
View(table(data.ep.roomResponse.second$Vset))


####拆分阀门开度####
data.ep.roomResponse.second$mvOpening<-tstrsplit(data.ep.roomResponse.second$testId,"_")[[1]]
data.ep.roomResponse.second$bvOpening<-tstrsplit(data.ep.roomResponse.second$testId,"_")[[2]]
  

####根据时间分配TestId####
#作废不执行，现已直接在数据采集中加入testId
data.ep.roomResponse.second$testId<-"prepare"
info.ep.testId<-read.xlsx(file="Info_TestId.xlsx",sheetName = "0602")%>%as.data.table(.)
apply(info.ep.testId[,c("start","end","testId")], MARGIN = 1, function(x){
  data.ep.roomResponse.second[Time %within% interval(start=as.POSIXct(x[1]),end=as.POSIXct(x[2]))]$testId<<-as.character(x[3])
  })
data.ep.roomResponse.second<-merge(x=data.ep.roomResponse.second,
                                   y=info.ep.testId[,c("testId","Kp","Ti")],all.x = TRUE,by = "testId")


####根据TestId分配正序的label####
#原来的程序label不一样，测出来label对应一个是1s一个是2s
data.ep.roomResponse.second$timeCount<- -999
for(i in unique(data.ep.roomResponse.second[testId!="prepare"]$testId)){
  data.ep.roomResponse.second[testId==i]$timeCount<-0:(nrow(data.ep.roomResponse.second[testId==i])-1)
}


####通过阀门流量处理####

#计算水头
data.ep.roomResponse.second<-data.ep.roomResponse.second%>%
                                mutate(.,totalWaterH=.$Totalpressure/9807,subWaterH=.$Subpressure/9807)%>%
                                as.data.table(.)
#计算支路流阻系数
data.ep.roomResponse.second$resistanceS<-data.ep.roomResponse.second$subWaterH/(data.ep.roomResponse.second$Flowrate*data.ep.roomResponse.second$Flowrate)
data.ep.roomResponse.second[is.infinite(resistanceS)]$resistanceS<-NA

####阀门-流量处理####
#上行下行处理
##针对本来没有isOn预处理的情况

####onRoutine赋值####
# 针对导入前没有对onRoutine预处理情况
data.ep.roomResponse.second[testType=="Grad"]<-data.ep.roomResponse.second[testType=="Grad"]%>%{
  for(i in unique(.$testId)){
    if(is.na(getMode(.[testId==i]$onRoutine)[1])){
      cat(i,nrow(.[testId==i][1:(0.5*nrow(.[testId==i]))]),"\n")
      .[testId==i][1:(0.5*nrow(.[testId==i]))]$onRoutine<-TRUE
      .[testId==i][(0.5*nrow(.[testId==i])):(nrow(.[testId==i]))]$onRoutine<-FALSE
      .[testId==i][testVset==100]$onRoutine<-TRUE
    }
  }
  .
}


ggplot(nn,aes(x=Time,y=Valveopening,color=as.factor(onRoutine),group=1))+geom_path()+geom_path(aes(x=Time,y=Vset,color="yellow"))

data.ep.roomResponse.second$isOn<-""
data.ep.roomResponse.second$isOn<-data.ep.roomResponse.second%>%{
  for(i in unique(.$testId)){
    .[testId==i]$isOn<-apply(X=cbind(.[testId==i]$Valveopening,c(0,.[testId==i]$Valveopening)),
                             MARGIN = 1,
                             FUN = function(x){
                               cat(x[1],"\t",x[2],"\n")
                               if(abs(x[1]-x[2])<0.5){return("FALSE")}
                               if(x[1]>x[2]){return("Ascending")}
                               if(x[1]<x[2]){return("Descending")}
                             })
    # .[testId==i]$isOn<-{
    #   if(abs(.[testId==i]$Valveopening-c(0,.[testId==i]$Valveopening))<0.4){
    #     "FALSE"
    #   }else if(){
    #     
    #   }
    # }
  }
  .$isOn
}
##isOn通用处理
data.ep.roomResponse.second$isOn<-as.character(data.ep.roomResponse.second$isOn)
data.ep.roomResponse.second[isOn=="TRUE"&Valveopening<Vset]$isOn<-"Ascending"#这部分不太对，考虑在实际工况下长期有阀门波动值低于/高于设定值
data.ep.roomResponse.second[isOn=="TRUE"&Valveopening>Vset]$isOn<-"Descending"

####通过Grad测试计算每组稳定时的情况####
#Grad流量取均值
data.ep.valveFlow<-data.ep.responseTest[testType=="Grad",
                                               .(testId=testId[1],
                                                 count=length(timeCount),
                                                 Vset=Vset[1],
                                                 onRoutine=onRoutine[1],
                                                 mvOpening=getSplitMember(testId,"_",index=1),#tstrsplit(testId, "_", fixed=TRUE)[[1]],我自己写的函数还好用些
                                                 bvOpening=getSplitMember(testId,"_",index=2),#tstrsplit(testId, "_", fixed=TRUE)[[2]],
                                                 resistanceS=mean(resistanceS[!is.nan(resistanceS)],na.rm=TRUE),
                                                 Flowrate=mean(Flowrate,na.rm=TRUE),#[10:length(timeCount)]
                                                 Valveopening=mean(Valveopening,na.rm=TRUE),
                                                 Subpressure=mean(Subpressure,na.rm=TRUE),
                                                 Totalpressure=mean(Totalpressure,na.rm=TRUE),
                                                 subWaterH=mean(subWaterH,na.rm=TRUE),
                                                 totalWaterH=mean(totalWaterH,na.rm=TRUE)
                                                 ),by=(testIdVsetDir=paste(testId,onRoutine,Vset,sep = "_"))]#
#平均后迟滞环评估
#选用Grad测试相当于取平均，实际上Step也可以做到，只是处理起来比较麻烦
####此处可用于流量方程中H的确定####
# H与Q关系
ggplot(data.ep.valveFlow,aes(x=Flowrate,y=subWaterH,color=testId,shape=onRoutine,lty=bvOpening))+
  geom_point(aes())+geom_line(aes())+geom_line(data = nn,aes(x=Flowrate,y=estSubWaterH,color="blue"))+#+facet_wrap(.~as.factor(Vset),nrow=1)
  # geom_line(data = data.ep.local.predict,aes(x=Flowrate,y=subWaterH))
  theme_bw()+theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.text = element_text(size=16))
ggplot(data.ep.valveFlow,aes(x=Valveopening,y=Subpressure/Totalpressure,color=testId,shape=onRoutine,#/Totalpressure
                             lty=bvOpening))+geom_point()+geom_line()+facet_wrap(~mvOpening,nrow=1)


write.xlsx(data.ep.valveFlow,file = "PressureDetermine_v2.xlsx")
stat.ep.valveFlow<-data.ep.valveFlow[,.(maxHR=(Flowrate[onRoutine==TRUE]-Flowrate[onRoutine==FALSE])
                                        ),by=(testIdVset=paste(testId,Vset,sep = "_"))]

ggplot(data = data.table(x=nn$x,y=nn$y,yin=nn$yin))+geom_line(aes(x=x,y=yin))+geom_line(aes(x=x,y=y,color="red"))

####可视化####
data.ep.roomResponse.second%>%
  ggplot(data=.,aes(x=Label,y=t_out_set,color=testId,lty=testId))+
  geom_line()+geom_line(aes(x=Label,y=InWindT))+geom_line(aes(x=Label,y=Flowrate*50))+facet_wrap(~testId,nrow = 3)


data.ep.responseTest[mvOpening=="MV=0.75"&bvOpening=="BV=0.25"&timeCount<200,#&timeCount&testVset<=20,#&"prepare", #&!is.na(Kp)&Kp!=0,,"V2O_1","V2O_2","V2O_3",!testId%in%c("prepare","Casc_Low_1","Casc_Low_2")
                            c("timeCount","testId","Valveopening","Vset","Flowrate",
                              "InWindT","t_out_set","testType",
                              "OutWindT","t_return_set",
                              "resistanceS")]%>%#,,"testVset","testVset",,"Kp","Ti","flow_set"
  # mutate_at(.,c("resistanceS"),function(x){ifelse(x>1|is.na(x),20,x*20)})%>%
  #mutate(.,para=paste("Kp"=as.character(.$Kp),"Ti"=as.character(.$Ti),sep=","))%>%.[,!names(.)%in%c("Kp","Ti")]%>% 
  melt(.,id.var=c("timeCount","testType","testId"))%>%#,"Ti","para","testVset"
  as.data.table(.)%>%{ #,"InWindT","t_out_set","resistanceS"
    ggplot(data=.[variable %in% c("Valveopening","Vset")],aes(x=timeCount,y=value,color=variable,width=4,group=paste(testId,variable)))+
      geom_path()+#geom_point()+
      geom_line(data=.[variable %in% c("Flowrate")],aes(x=timeCount,y=value*100))+#)+value#"OutWindT",(value-20)*5)
      # geom_line(data=.[variable %in% c("InWindT","t_out_set")],aes(x=timeCount,y=value))+
      # geom_line(data=.[variable %in% c("OutWindT","t_return_set")],aes(x=timeCount,y=value))+
      scale_y_continuous(sec.axis = sec_axis(~./100,name = "Flow rate"))+#./5+20
      facet_wrap(~testType,ncol = 2)+
      theme_bw()+theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),#legend.position = c(0.25,0.75),
                       legend.text = element_text(size=16))#
  }

#阀门局部移动引起迟滞变化
nn<-read.xlsx(file = "迟滞阀门波动阻力变化.xlsx",sheetIndex = 1)%>%as.data.table(.)
nn[,c("timeCount","Valveopening","subWaterH","Flowrate")]%>%melt(.,id.var=c("timeCount"))%>%{
  ggplot(data=.,aes(color=variable))+
    geom_line(data=.[variable=="Flowrate"],aes(x=timeCount,y=value))+
    geom_line(data=.[variable=="Valveopening"],aes(x=timeCount,y=value/100))+
    geom_line(data=.[variable=="subWaterH"],aes(x=timeCount,y=value*100))+
    theme_bw()+theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.text = element_text(size=16))#
}

#流量可视化
#同一工况不同阶跃参数对比
data.ep.responseTest[testType=="Step"&bvOpening!="BV=0.5"&Valveopening<30&resistanceS<0.25&testId=="MV=0.75_BV=0.25_F=33_Step",#"&testVset==15&testId=="MV=0.75_BV=0.25_F=33_Step"&testVset<40,
                            c("timeCount","Time","testId","Valveopening","Subpressure","Totalpressure","resistanceS","Flowrate")]%>%#,"testVset","isOn"
  #melt(.,id.var=c("timeCount","testId","Time","Vset"))%>%
  ggplot(data = .,aes(x=Valveopening,y=resistanceS,group=testId))+#,color=as.factor(testVset),,shape=isOn,color=isOn,mapping=timeCount
  geom_point(aes(alpha=0.2))+#,shape=isOn+,color=testId
  # geom_path()+
  # facet_wrap(~as.factor(testVset),nrow = 3)+#Valveopening
  theme_bw()+theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),#legend.position = c(0.25,0.75),
                   legend.text = element_text(size=16))

#不同工况参数对比
data.ep.responseTest[mvOpening=="MV=0.75"&bvOpening=="BV=0.25"&Valveopening<40&resistanceS<1,#&testVset<=35&testVset>10
                            c("testType","timeCount","Time","testId","Valveopening","testVset","Flowrate","Totalpressure","Subpressure","resistanceS","isOn","mvOpening","bvOpening")]%>%
  # mutate_at(.,c("resistanceS"),function(x){ifelse(x>1|is.na(x),1,x)})%>%
  #melt(.,id.var=c("timeCount","testId","Time","Vset"))%>%
  ggplot(data = .,aes(x=Valveopening,y=resistanceS,color=as.factor(testVset),mapping = timeCount,group=paste(testId,testVset)))+
  geom_point(aes(alpha=0.5,shape=bvOpening))+
  # geom_path()+
  facet_wrap(~as.factor(testType),nrow = 1)+
  theme_bw()+theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),#legend.position = c(0.9,0.2),
                   legend.text = element_text(size=16))

#低流量粘滞
data.ep.roomResponse.second[!is.na(testId)&testVset==10&testType=="Step",#testVset=5,7,10
                            c("timeCount","Time","testId","Valveopening","testVset","Flowrate")]%>%#,"isOn"
  ggplot(data = .,aes(x=timeCount,y=Flowrate,color=testId))+
  geom_point(alpha=0.6)+
  geom_path()+
  facet_wrap(~as.factor(testVset)+testId,nrow = 3)

#水头损失
data.ep.roomResponse.second[timeCount<3000&testVset==30,#testVset=5,7,10
                            c("timeCount","Time","subWaterH","Flowrate","Valveopening","resistanceS")]%>%
  melt(.,id.var=c("timeCount","Time"))%>%{
    ggplot(data = .,aes(x=timeCount,y=value,color=variable))+
      geom_line(data = .[variable=="Flowrate"],aes(x=timeCount,y=value*10))+
      geom_line(data = .[variable=="subWaterH"],aes(x=timeCount,y=value*1000))+
      geom_line(data = .[variable=="resistanceS"],aes(x=timeCount,y=value*10))+
      geom_line(data = .[variable=="Valveopening"],aes(x=timeCount,y=value/10))+
      ylim(c(0,10))
  }#,"isOn"

# 压强比例
# 通过压强比例确定各迟滞的工况
#压力占比
data.ep.roomResponse.second[!is.na(testId)&testId!=""&testType!="Grad"&testVset<30&testVset>10,#testVset=5,7,10 &timeCount<3000&timeCount>2500&testId=="MV=1_BV=0.25_F=33_Step"&testId=="MV=0.5_BV=0_F=33_Step"
                            c("timeCount","Time","testId","testType","Subpressure","Totalpressure","Flowrate","Valveopening","resistanceS","isOn","testVset")]%>%{
    ggplot(data = .)+
      geom_point(aes(x=Valveopening,y=Totalpressure,color=testId,shape=isOn),alpha=0.5)+
      # geom_line(aes(x=timeCount,y=Subpressure,color=testId),lty="dashed")+
      # geom_line(aes(x=timeCount,y=Totalpressure,color=testId))+
      # geom_line(aes(x=timeCount,y=Valveopening,color=testId),lty="dotted")+
      # geom_line(aes(x=timeCount,y=Flowrate*100,color=testId),lty="longdash")+
      facet_wrap(.~testVset,nrow=2)+#ylim(c(0.5,1.1))+
      theme_bw()+theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.text = element_text(size=16))
  }#,"isOn"



####检查一下阀门阻力基本保持不变的情况####
nn1<-(backup.ep.roomResponse.second[!is.na(testId)&testId=="MV=1_BV=0_F=33_Step"&testVset==30])
nn1[,c("ID","testType","Totalpressure","totalWaterH","subWaterH","timeLabel",
       "Subpressure","InWaterT","OutWaterT","InWindT","OutWindT","Fset","Tset","t_out_set","t_return_set","flow_set","Powerset","Fre","HeatingRate")]<-NULL
nn1$onRoutine<-as.character(nn1$isOn)
ggplot(data=nn1[resistanceS<0.2],aes(x=timeCount,y=resistanceS))+geom_point()

data.ep.roomResponse.second[testType=="Step"&testVset==Vset&isOn=="FALSE"&!is.na(resistanceS)&resistanceS<0.1&testVset<35]%>%
  ggplot(data=.,aes(x=testId,y=resistanceS,color=as.factor(Vset)))+geom_boxplot()

####统计各阶跃工况稳定时的波动情况####
stat.ep.valveStable<-data.ep.roomResponse.second[testType=="Step"&testVset==Vset&isOn=="FALSE",.(
  statTestId=testId[1],
  Vset=Vset[1],
  lowWsk=-999,#直接boxplot全空会返回logical
  highWsk=-999,#好蠢啊
  meanResistance=mean(resistanceS,na.rm=TRUE),
  sdResistance=sd(resistanceS,na.rm=TRUE),
  meanFlowrate=mean(Flowrate,na.rm=TRUE),
  sdFlowrate=sd(Flowrate,na.rm=TRUE)
),by=paste(testId,testVset,sep = "_")]
getMatch<-function(x,num){ boxplot.stats(data.ep.roomResponse.second[testId==x[1]&Vset==as.integer(x[2])]$resistanceS)$stats[num] }

stat.ep.valveStable[!is.nan(meanResistance)]<-stat.ep.valveStable[!is.nan(meanResistance)]%>%
  mutate(.,lowWsk=apply(.[,c("statTestId","Vset")], MARGIN = 1,FUN = getMatch,num=1),
         highWsk=apply(.[,c("statTestId","Vset")], MARGIN = 1,FUN = getMatch,num=5))
stat.ep.valveStable$statTestId<-as.character(stat.ep.valveStable$statTestId)

stat.ep.valveStable$stableRange<-stat.ep.valveStable$highWsk-stat.ep.valveStable$lowWsk

####以阻力变化识别克服迟滞
#这个方法可以，但是会有部分偶尔波动出界的不被识别
nn1[isOn %in% c("Descending")]$onRoutine<-apply(nn1[isOn=="Descending",c("resistanceS","testId","testVset")],MARGIN = 1,
                                         FUN = function(x){
                                           
                                           if(is.na(x[1])){
                                             return(NA)
                                           }#切记多元素的&，而非&&
                                           range<-as.numeric(stat.ep.valveStable[statTestId==x[2]&Vset==as.numeric(x[3]),c("lowWsk","highWsk")])
                                           if(anyNA(range)){
                                             return(NA)
                                           }
                                           if(x[1]>range[1]&x[1]<range[2]){
                                             return("Descending_overcome")
                                           }else
                                             return(NA)
                                         })
for(i in unique(nn1$testId)){
  maxStable<-max(stat.ep.valveStable[statTestId==i&Vset]$highWsk)[1]
  minStable<-min(stat.ep.valveStable[statTestId==i]$lowWsk)[1]
  range<-range(nn1[isOn %in% c("Descending")&resistanceS<maxStable&resistanceS>minStable]$Valveopening)
  nn1[isOn %in% c("Descending")&Valveopening>range[1]&Valveopening<range[2]]$onRoutine<-"Descending_overcome"
}
nn1[isOn %in% c("Descending")&
      resistanceS<max(stat.ep.valveStable[statTestId%in%testId]$highWsk)[1]&
      resistanceS>min(stat.ep.valveStable[statTestId%in%testId]$lowWsk)[1]]$onRoutine<-"Descending_overcome"

####以流量变化克服迟滞
# 会稍微保守一点
nn1[isOn %in% c("Descending")&Flowrate>max(Flowrate)*0.95]$onRoutine<-"Descending_overcome"
ggplot(data=nn1,aes(x=Valveopening,y=resistanceS,color=onRoutine))+geom_point()+ylim(c(0,0.5))


####克服迟滞情况附标签####
for(i in unique(data.ep.roomResponse.second[testType=="Step"]$testId)){
  data.ep.roomResponse.second[]
}


  
####统计一下各case的情况####
nn<-temp.ep.pre[timeCount<600&testId=="V2F_4"][timeCount %in% c((max(timeCount)-100):max(timeCount))]
mean(temp.ep.pre[timeCount<600&testId=="V2F_4"][timeCount%in% c(max(timeCount)-100:max(timeCount))]$InWindT,na.rm=TRUE)

stat.ep.testId<-temp.ep.pre[timeCount<600,.(
  duration=max(timeLabel,na.rm = TRUE),
  peakime=timeCount[InWindT==max(InWindT,na.rm = TRUE)][1],
  startTout=mean(InWindT[timeCount%in%1:10],na.rm=TRUE),#开始时刻的送风温度
  startTroom=mean(OutWindT[timeCount%in%1:10],na.rm=TRUE),#开始时刻的室内温度
  startFlowrate=mean(Flowrate[timeCount%in%1:10],na.rm=TRUE),#开始时刻的流量
  meanValveBias=mean(abs(Valveopening-Vset),na.rm=TRUE),
  meanLastTout=mean(InWindT[timeCount%in% c((length(timeCount)-100):length(timeCount))],na.rm = TRUE),#最后100s的波动情况#(length(timeCount)-60:length(timeCount))
  sdLastTout=sd(InWindT[timeCount%in% c((length(timeCount)-100):length(timeCount))],na.rm = TRUE),#最后100s的波动情况
  maxTout=max(InWindT,na.rm = TRUE),
  overshoot=(max(InWindT,na.rm = TRUE)/mean(InWindT[timeCount%in% c((length(timeCount)-100):length(timeCount))],na.rm = TRUE))-1
),by=testId]%>%
  merge(x=.,y=info.ep.testId[,c("testId","Kp","Ti")],all.x = TRUE,by = "testId")



#输出秒级数据
write.xlsx(data.ep.roomResponse.second,file="SecVer_RoomResponse_20210507.xlsx")

####看一下阀门的开关####
#sheetName = "阀门开关响应"
data.ep.valveFlow<-as.data.table(read.csv(file="OriginalData/SecVer_ResponseOnOffTest_20210506.csv"))%>%
  mutate(.,TestId=as.factor(.$TestId),isOn=(.$from<.$to))%>%as.data.table(.)%>%.[complete.cases(.)]

ggplot(data=data.ep.valveFlow[TestId%in%c("2","3","4","5","6","RTR_2","T2")|(TestId=="T1"&isOn==TRUE)],aes(x=Valveopening,y=Flowrate,color=isOn,lty=TestId))+geom_line()+facet_wrap(~TestId,nrow = 2)+
  theme_bw()+theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.text = element_text(size=16))


####室温基本响应####
# sheetName = "温度响应"
data.ep.temp<-as.data.table(read.xlsx(file="SecVer_ResponseTest_20210506.xlsx",sheetIndex =  2))
ggplot(data=data.ep.temp,aes(x=Label,y=Flowrate,color=isOn,lty=TestId))+geom_line()+facet_wrap(~TestId,nrow = 2)


