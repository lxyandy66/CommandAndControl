dataValveTest=readtable("StepResponseForFitting.csv");

plot(dataValveTest.Valveopening,dataValveTest.Flowrate);

valvePhyFitFun=fittype('(Wf*R0)/(((1-Lambda)*(valveOpening+hor)+Lambda)^2)+ver',...
    'independent',{'valveOpening'},'dependent',{'Resistance'},'coefficients',{'Wf','R0','Lambda','hor','ver'});

% 预处理
dataValveTestForFitting=dataValveTest(strcmp(dataValveTest.testId,'MV=0.5_BV=0_F=33_Step')&dataValveTest.isOn=="Descending",:);
dataValveTestForFitting.resistanceS=cellfun(@str2double,dataValveTestForFitting.resistanceS);
dataValveTestForFitting=dataValveTestForFitting(~isnan(dataValveTestForFitting.resistanceS)&dataValveTestForFitting.resistanceS<10,:);

[fittedResult,eva]=fit(dataValveTestForFitting.Valveopening,dataValveTestForFitting.resistanceS,valvePhyFitFun,'Robust','on')
plot(fittedResult,dataValveTestForFitting.Valveopening,dataValveTestForFitting.resistanceS)