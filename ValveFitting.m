dataValveTest=readtable("RealTestForFitting_removed.csv");

% plot(dataValveTest.Valveopening,dataValveTest.Flowrate);

valvePhyFitFun=fittype('(Wf*R0)/(((1-Lambda)*((valveOpening/100))+Lambda)^2)',...#+hor+ver,'hor','ver'+((1-Wf)*R0*Lambda^(2*(valveOpening/100-1)))
    'independent',{'valveOpening'},'dependent',{'Resistance'},'coefficients',{'Wf','R0','Lambda'});



scatter(dataValveTest(strcmp(dataValveTest.isOn,"Ascending"),:).Valveopening,dataValveTest(strcmp(dataValveTest.isOn,"Ascending"),:).resistanceS,"r")
hold on
scatter(dataValveTest(dataValveTest.isOn=="Descending",:).Valveopening,dataValveTest(dataValveTest.isOn=="Descending",:).resistanceS,"b")

% 预处理
% dataValveTestForFitting=dataValveTest(strcmp(dataValveTest.testId,'MV=0.5_BV=0_F=33_Step')&dataValveTest.isOn=="Descending",:);
dataValveTestForFitting=dataValveTest(strcmp(dataValveTest.isOn,"Descending"),:);%Ascending,Descending
% dataValveTestForFitting.resistanceS=cellfun(@str2double,dataValveTestForFitting.resistanceS);
% dataValveTestForFitting=dataValveTestForFitting(~isnan(dataValveTestForFitting.resistanceS)&dataValveTestForFitting.resistanceS<10,:);

[fittedResultDsc,eva]=fit(dataValveTestForFitting.Valveopening,dataValveTestForFitting.resistanceS,valvePhyFitFun,...
    'StartPoint',[0.5 0.0075 0],'StartPoint',[1.8 0.0075 0],'Lower',[-3,0,0],'Upper',[1,0.05,1])%,'Robust','on','StartPoint',[1.1 0.007 0.01],'Robust','on',
plot(fittedResultDsc,dataValveTestForFitting.Valveopening,dataValveTestForFitting.resistanceS)

x=0:100;
yDsc=fittedResultDsc(x);

dataValveTestForFitting=dataValveTest(strcmp(dataValveTest.isOn,"Ascending"),:);%Ascending,Descending
% dataValveTestForFitting.resistanceS=cellfun(@str2double,dataValveTestForFitting.resistanceS);
% dataValveTestForFitting=dataValveTestForFitting(~isnan(dataValveTestForFitting.resistanceS)&dataValveTestForFitting.resistanceS<10,:);

[fittedResultAsc,eva]=fit(dataValveTestForFitting.Valveopening,dataValveTestForFitting.resistanceS,valvePhyFitFun,...
    'StartPoint',[0.5 0.0075 0],'Lower',[-3,0,0],'Upper',[1,0.05,1])%,'Robust','on','StartPoint',[1 0.0007 0.001],'StartPoint',[1 0.007 0.01]
plot(fittedResultAsc,dataValveTestForFitting.Valveopening,dataValveTestForFitting.resistanceS,"g")

yAsc=fittedResultAsc(x);

plot(x,yDsc,"r");
hold on
plot(x,yAsc,"b");
