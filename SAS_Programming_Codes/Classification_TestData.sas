PROC IMPORT OUT= WORK.DEPRESSION 
            DATAFILE= "C:\Stevens\SAS\Adavancce_SAS\Raw_data\Depress.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;


/*LDA*/
title 'Linear Discriminant Analysis';
proc discrim data=depression anova all distance
pool=yes crossvalidate outcross=cross1 posterr;
priors equal;
class Chronill;
var Age Educ Income Cases Drink;
ods output LinearDiscFunc= LinearDiscFunc;
run;

/*to clear the result viewer*/
dm 'odsresults; clear'; 

/*KNN*/
/* Create the test data*/
data test;    
   infile datalines ;

   input Age 1-2 Educ 4 Income 6-7 Cases 9 Drink 11; 
        
datalines;
40 7 20 0 2
;
run; 


title 'KNN Analysis';
proc discrim data=depression 
method = npar k=5
testdata = test
testout = test_out;
class Chronill;
var Age Educ Income Cases Drink;
run;

/*to clear the result viewer*/
dm 'odsresults; clear';

/*Logit*/
title 'Stepwise Regression on Depression Data';
proc logistic data=depression outest=betas;
      model chronill(event = '1') =age educ income cases drink
					/ selection=stepwise
                     slentry=0.3
                     slstay=0.35
                     details
                     lackfit;
      output out=pred p=phat lower=lcl upper=ucl
             predprob=(individual crossvalidate);
run;

/*to clear the result viewer*/
dm 'odsresults; clear';


/*Naive Baysian Classification*/

