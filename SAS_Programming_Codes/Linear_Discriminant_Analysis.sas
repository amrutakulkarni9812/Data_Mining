PROC IMPORT OUT= WORK.chem_mod 
            DATAFILE= "C:\Stevens\SAS\Adavancce_SAS\Raw_data\financialperformance.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

proc format;
value grpf 1= "Mature/Troubled" 2="Growth";
data chem_mod;
set chem_mod;
group = (P_E>=9) + 1; if P_E=. then group=.;
rename symbol=company;
format group grpf.;
run;
ods graphics on;
proc discrim data=chem_mod anova all distance
pool=yes crossvalidate outcross=cross1 posterr;
priors equal;
class group;
var ROR5___ D_E SALESGR5___ EPS5___ NPM1___ PAYOUTR1;
ods output LinearDiscFunc= LinearDiscFunc;
run;

proc transpose data=LinearDiscFunc out=myout;
id Variable;
run;
data myout1;
set myout;
_TYPE_="SCORE";
run;
data chem_mod_new;
set chem_mod;
Constant=1;run;
proc score data=chem_mod_new score=myout1 out=new;
run;
proc sort data=new; by company; run;
proc sort data=cross1; by company; run;
data newout;
merge cross1 (rename = (growth=Post_growth Mature_Troubled = Post_Mat_Troub))
new (rename = (growth=Score_growth Mature_Troubled = Score_Mat_Troub));
by company;
diff = Score_Growth - Score_Mat_Troub;
label post_growth = "Posterior Probability for Growth Group";
label Score_growth = "Discriminant Score for Growth Group";
label post_Mat_Troub = "Posterior Probability for Mature/Troubled Group";
label Score_Mat_Troub = "Discriminant Score for Mature/Troubled Group";
label diff = "Discriminant Score";
run;
symbol1 v=star i=none color=black;
axis1 label=( angle=90);
proc gplot data=newout;
plot post_growth*diff / vaxis=axis1;
run;
