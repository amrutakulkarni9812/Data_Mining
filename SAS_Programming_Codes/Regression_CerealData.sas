libname sas_data "C:\Stevens\SAS\Adavancce_SAS\SAS_data" access=read;

proc copy in=sas_data out=work;
	select cereal_ds;
RUN;

proc reg data=cereal_ds outest=est_cereal;
	model rating=sugars  shelf	/dwProb;
run;
quit;

proc reg data=cereal_ds outest=est_cereal;
	model  rating =sugars fiber sodium fat protein  / dwProb pcorr1 ;
	OUTPUT OUT=reg_cerealOUT PREDICTED=PRCDT RESIDUAL=x_Res
			   L95M=c_l95m U95M=C_U95M l95=c_L95 u95=c_U95
	rstudent=C_rstudent h=lev cookd=Cookd dffits=dffit
	STDP=C_s_predicted STDR=C_s_residual STUDENT=C_student;
	plot rating * sugars = '*';
   quit;

proc reg data=cereal_ds outest=est_cereal;
	model rating=sugars /dwProb;
run;
quit;

proc reg data=cereal_ds outest=est_cereal;
	model rating=fiber /dwProb;
run;
quit;

proc reg data=cereal_ds outest=est_cereal;
	model rating=fat /dwProb;
run;
quit;

proc reg data=cereal_ds outest=est_cereal;
	model rating=sodium /dwProb;
run;
quit;

proc reg data=cereal_ds outest=est_cereal;
	model rating=sugars fiber /dwProb;
run;
quit;

proc reg data=cereal_ds outest=est_cereal;
	model rating=sugars fat /dwProb;
run;
quit;

proc reg data=cereal_ds outest=est_cereal;
	model rating=sugars sodium /dwProb;
run;
quit;

proc reg data=cereal_ds outest=est_cereal;
	model  rating =sugars fiber sodium fat protein  / dwProb pcorr1 VIF selection=forward;
	OUTPUT OUT=reg_cerealOUT PREDICTED=PRCDT RESIDUAL=x_Res
			   L95M=c_l95m U95M=C_U95M l95=c_L95 u95=c_U95
	rstudent=C_rstudent h=lev cookd=Cookd dffits=dffit
	STDP=C_s_predicted STDR=C_s_residual STUDENT=C_student;
	
   quit;

 title "Simple Regression for cerel dataset rating vs. sugars";
   proc reg data=cereal_ds outest=est_cereal;
	model  rating =sugars  / dwProb ;
	OUTPUT OUT=reg_cerealOUT PREDICTED=c_predict RESIDUAL=c_Res
			   L95M=c_l95m U95M=C_U95M l95=c_L95 u95=c_U95
	rstudent=C_rstudent h=lev cookd=Cookd dffits=dffit
	STDP=C_s_predicted STDR=C_s_residual STUDENT=C_student;
	
   quit;

   title "Simple Regression for cerel dataset rating vs. sugars fiber potass ";

     proc reg data=cereal_ds outest=est_cereal;
	model  rating =sugars fiber potass/ dwProb VIF;
	OUTPUT OUT=reg_cerealOUT PREDICTED=  RESIDUAL=Res
			   L95M=c_l95m U95M=C_U95M l95=c_L95 u95=c_U95
	rstudent=C_rstudent h=lev cookd=Cookd dffits=dffit
	STDP=C_s_predicted STDR=C_s_residual STUDENT=C_student;
	
   quit;

   data cereal_ds2;
   	set cereal_ds;
	if shelf=1 then shelf1=1;
	else shelf1=0;
	if shelf=2 then shelf2=1;
	else shelf2=0;
	if shelf=3 then shelf3=1;
	else shelf3=0;
run;


     proc reg data=cereal_ds2 outest=est_cereal;
	model  rating =calories shelf1 shelf2 shelf3/ dwProb VIF;
	OUTPUT OUT=reg_cerealOUT PREDICTED=  RESIDUAL=Res
			   L95M=c_l95m U95M=C_U95M l95=c_L95 u95=c_U95
	rstudent=C_rstudent h=lev cookd=Cookd dffits=dffit
	STDP=C_s_predicted STDR=C_s_residual STUDENT=C_student;
	
   quit;


   data cereal_ds2;
   	set cereal_ds;
	if shelf=1 then shelf1=1;
	else shelf1=0;
	if shelf=2 then shelf2=1;
	else shelf2=0;
	if shelf=3 then shelf3=1;
	else shelf3=0;
	shelf2_cal=shelf2*calories;
run;

     proc reg data=cereal_ds2 outest=est_cereal;
	model  rating =calories shelf1 shelf2 shelf2_cal/ dwProb VIF;
	OUTPUT OUT=reg_cerealOUT PREDICTED=  RESIDUAL=Res
			   L95M=c_l95m U95M=C_U95M l95=c_L95 u95=c_U95
	rstudent=C_rstudent h=lev cookd=Cookd dffits=dffit
	STDP=C_s_predicted STDR=C_s_residual STUDENT=C_student;
	
   quit;


proc copy in=sas_data out=work;
	select Churn;;
RUN;

Proc freq data=churn2;
	table churn_ind*voiceplan_ind;
run; 

proc logistic data=churn2 descending;
	class voiceplan_ind(ref='0')/ param=ref;
	model churn_ind=voiceplan_ind;
quit;

proc logistic data=churn2 descending;
	class Service_cat(ref='0')/ param=ref;
	model churn_ind=Service_cat;
quit;

proc logistic data=churn2 descending;
	class Service_ind(ref='0')/ param=ref;
	model churn_ind=Service_ind;
quit;
