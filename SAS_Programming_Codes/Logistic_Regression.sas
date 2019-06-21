PROC IMPORT OUT= WORK.DEPRESSION 
            DATAFILE= "C:\Stevens\SAS\Adavancce_SAS\Raw_data\Depress.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

proc freq data=WORK.depression order=data;
   tables drink*sex / chisq;
   run;

/* Split the original data to get a data for depressed people, and then get a frequency table for drinking and sex*/
data depressed;
set depression;
if (Cesd >= '16') then output;
run;

proc freq data=WORK.depressed order=data;
   tables drink*sex / chisq;
   run;

/* Split the original data to get a data for normal people, and then get a frequency table for drinking and sex*/
data normal;
set depression;
if (Cesd < '16') then output;
run;

proc freq data=WORK.normal order=data;
   tables drink*sex / chisq;
   run;


/* Logit using interaction term for Cesd and Sex, expb gives the exponentiated parameters*/
proc logistic data = work.depression;
	class Cesd Sex;
	model drink = Sex Cesd Sex*Cesd / expb;
	run;
quit;

/*to clear the result viewer*/
dm 'odsresults; clear'; 

/* Stepwise logistic regression of depression data set*/

title 'Stepwise Regression on Depression Data';
ods graphics on;
   proc logistic data=depression outest=betas covout plots(only)=(roc(id=obs) effect);
      model acuteill(event = '1') =age educ income cases drink
					/ selection=stepwise
                     slentry=0.3
                     slstay=0.35
                     details
                     lackfit;
      output out=pred p=phat lower=lcl upper=ucl
             predprob=(individual crossvalidate);
   run;
   ods graphics off;

/*To print the predicted parameter estimates and predicted probabilities*/
   proc print data=betas;
      title2 'Parameter Estimates and Covariance Matrix';
   run;
   proc print data=pred;
      title2 'Predicted Probabilities and 95% Confidence Limits';
   run;
