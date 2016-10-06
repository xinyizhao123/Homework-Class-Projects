****************************;
* BIOS 508 Final Project    ;
* Xinyi Zhao (2168379)      ;
* December 20, 2015         ;
****************************;

*------------------------------------;
*            Read data               ;
*------------------------------------;

*** Transform TXT to CSV using R;
* > IDP <- read.delim("C:/Users/zhaohexu/Dropbox/Academic/Courses/Fall 2015/BIOS 508/final project/IDP.txt");

*** Read CSV;
FILENAME REFFILE "/folders/myfolders/sasuser.v94/IDP.csv" TERMSTR=CR;

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=final;
	GETNAMES=YES;
RUN;

PROC CONTENTS DATA=final; RUN;

proc print data=final; run;


*------------------------------------;
*           Data cleaning            ;
*------------------------------------;

data idp;
set final;
if retain=. then delete;
if age le 24 then agecat=1; /*age =<24*/
else if age le 44 then agecat=2; /*age 25-44*/
else agecat=3; /*age >=45*/
if sex=1 then female=0;
if sex=2 then female=1;
agecat1=0; agecat3=0;
if agecat=1 then agecat1=1;
if agecat=3 then agecat3=1;/*ref(age)=25-44*/
run;
 
proc print data=idp; run;
 
proc freq data=idp;
tables age*agecat/list;
run;

proc freq data=idp;
tables agecat*agecat1*agecat3/list;
run;

proc freq data=idp;
tables sex*female/list;
run;


*----------------------------------------;
*         Descriptive statistics         ;
*----------------------------------------;

proc means data=idp;
var age;
run;

proc freq data=idp;
tables female race;
run;

proc freq data=idp;
tables transmission payer;
run;


*----------------------------------------;
*        Univariate associations         ;
*----------------------------------------;

**************** for Q1 ******************;

**** two independent sample T-test;

* means of age by retain;
proc means data=idp;
var age;
class retain;
run;

* compare the variance of the two samples (use BOXPLOT);
proc sgplot data=idp;
hbox age / category=retain;
run;
* (equal variance);

* test the normality assumption (using HISTOGRAM and PROBPLOT);
proc univariate data=idp;
var age;
histogram age/normal;
probplot age;
class retain;
run;
* (normal);

* complete a two sample t-test;
proc ttest data=idp alpha=0.05;
var age;
class retain;
run;

*** contigency table and chi-square test;
proc freq data=idp;
tables female*retain race*retain/expected chisq;
run;

proc freq data=idp;
tables transmission*retain payer*retain/expected chisq;
run;
* (no sparse data);

******* Test for linear trend;

proc logistic descending data=idp;
model retain=age;
run; 
* (Devss=895.219);

proc logistic descending data=idp;
model retain=agecat1 agecat3;
run;
* (Devss=887.760);
* (chi-square=7.459>3.84, thus p=0.006<0.05, choose categorized age);

*** unadjusted model;

proc logistic descending data=idp;
model retain=agecat1 agecat3/rl;
run;

proc logistic descending data=idp;
model retain=female/rl;
run;

proc logistic descending data=idp;
model retain=race/rl;
run;

proc logistic descending data=idp;
model retain=transmission/rl;
run;

proc logistic descending data=idp;
model retain=payer/rl;
run;

**************** for Q2 ******************;

**** two independent sample T-test;

* means of age by VS;
proc means data=idp;
var age;
class VS;
run;

* compare the variance of the two samples (use BOXPLOT);
proc sgplot data=idp;
hbox age / category=VS;
run;
* (equal variance);

* test the normality assumption (using HISTOGRAM and PROBPLOT);
proc univariate data=idp;
var age;
histogram age/normal;
probplot age;
class VS;
run;
* (normal);

* complete a two sample t-test;
proc ttest data=idp alpha=0.05;
var age;
class VS;
run;

*** contigency table and chi-square test;
proc freq data=idp;
tables retain*VS/expected chisq;
run;

proc freq data=idp;
tables female*VS race*VS/expected chisq;
run;

proc freq data=idp;
tables transmission*VS payer*VS/expected chisq;
run;
* (no sparse data);

******* Test for linear trend;

proc logistic descending data=idp;
model VS=age/aggregate;
run; 
* (Devss=843.949);

proc logistic descending data=idp;
model VS=agecat1 agecat3/aggregate;
run;
* (Devss=837.817);
* (chi-square=6.132>3.84, thus p=0.013<0.05, choose categorized age);

*** unadjusted model;

proc logistic descending data=idp;
model VS=retain/rl;
run;

proc logistic descending data=idp;
model VS=agecat1 agecat3/rl;
run;

proc logistic descending data=idp;
model VS=female/rl;
run;

proc logistic descending data=idp;
model VS=race/rl;
run;

proc logistic descending data=idp;
model VS=transmission/rl;
run;

proc logistic descending data=idp;
model VS=payer/rl;
run;


*--------------------------------;
*        Model selection         ;
*--------------------------------;

*** selection using change in deviance;

**************** for Q1 ******************;

*** STEP 1 ***;

* Model 0 -- intercept only;
proc logistic descending data=idp;
model retain=
/aggregate=(agecat1 agecat3 female race transmission payer) scale=none;
run;
* (Deviance=55.9083, df=35);

* Model 1 -- age only;
proc logistic descending data=idp;
model retain=agecat1 agecat3
/aggregate=(agecat1 agecat3 female race transmission payer) scale=none;
run;
* (D=46.7076, df=33, significant);

* Model 2 -- female only;
proc logistic descending data=idp;
model retain=female
/aggregate=(agecat1 agecat3 female race transmission payer) scale=none;
run;
* (D=55.7707, df=34);

* Model 3 -- race only;
proc logistic descending data=idp;
model retain=race
/aggregate=(agecat1 agecat3 female race transmission payer) scale=none;
run;
* (D=39.3937, df=34, significant);

* Model 4 -- transmission only;
proc logistic descending data=idp;
model retain=transmission
/aggregate=(agecat1 agecat3 female race transmission payer) scale=none;
run;
* (D=55.9080, df=34);

* Model 5 -- payer only;
proc logistic descending data=idp;
model retain=payer
/aggregate=(agecat1 agecat3 female race transmission payer) scale=none;
run;
* (D=49.8179, df=34, significant);

* choose age, race and payer;

*** STEP 2 ***;

* Model 6 -- age + race + payer;
proc logistic descending data=idp;
model retain=agecat1 agecat3 race payer
/aggregate=(agecat1 agecat3 female race transmission payer) scale=none;
run;
* (D=30.7251, df=31);

* Model 7 -- race + payer;
proc logistic descending data=idp;
model retain=race payer
/aggregate=(agecat1 agecat3 female race transmission payer) scale=none;
run;
* (D=40.1857, df=33, significant);

* Model 8 -- age + payer;
proc logistic descending data=idp;
model retain=agecat1 agecat3 payer
/aggregate=(agecat1 agecat3 female race transmission payer) scale=none;
run;
* (D=38.2107, df=32, significant);

* Model 9 -- age + race;
proc logistic descending data=idp;
model retain=agecat1 agecat3 race
/aggregate=(agecat1 agecat3 female race transmission payer) scale=none;
run;
* (D=38.4980, df=32, significant);

* do not drop any of age, race or payer;

*** STEP 3 ***;

* Model 10 -- age + race + payer + female;
proc logistic descending data=idp;
model retain=agecat1 agecat3 race payer female
/aggregate=(agecat1 agecat3 female race transmission payer) scale=none;
run;
* (D=30.6689, df=30);

* Model 11 -- age + race + payer + transmission;
proc logistic descending data=idp;
model retain=agecat1 agecat3 race payer transmission
/aggregate=(agecat1 agecat3 female race transmission payer) scale=none;
run;
* (D=30.7250, df=30);

* female and transmission are still not significant --do not include;

*** STEP 4 ***;

* do not need because any X cannot be dropped in STEP 2;

**** final model: age + race + payer;
proc logistic descending data=idp;
model retain=agecat1 agecat3 race payer;
run;


**************** for Q2 ******************;

*** STEP 1 ***;

* Model 0 -- retain + intercept only;
proc logistic descending data=idp;
model VS=retain
/aggregate=(retain agecat1 agecat3 female race transmission payer) scale=none;
run;
* (Deviance=74.0974, df=60);

* Model 1 – retain + age only;
proc logistic descending data=idp;
model VS=retain agecat1 agecat3
/aggregate=(retain agecat1 agecat3 female race transmission payer) scale=none;
run;
* (D=70.6124, df=58);

* Model 2 -- retain + female only;
proc logistic descending data=idp;
model VS=retain female
/aggregate=(retain agecat1 agecat3 female race transmission payer) scale=none;
run;
* (D=74.0717, df=59);

* Model 3 -- retain + race only;
proc logistic descending data=idp;
model VS=retain race
/aggregate=(retain agecat1 agecat3 female race transmission payer) scale=none;
run;
* (D=69.5428, df=59, significant);

* Model 4 -- retain + transmission only;
proc logistic descending data=idp;
model VS=retain transmission
/aggregate=(retain agecat1 agecat3 female race transmission payer) scale=none;
run;
* (D=74.0881, df=59);

* Model 5 -- retain + payer only;
proc logistic descending data=idp;
model VS=retain payer
/aggregate=(retain agecat1 agecat3 female race transmission payer) scale=none;
run;
* (D=67.5218, df=59, significant);

* choose race and payer;

*** STEP 2 ***;

* Model 6 -- retain + race + payer;
proc logistic descending data=idp;
model VS=retain race payer
/aggregate=(retain agecat1 agecat3 female race transmission payer) scale=none;
run;
* (D=63.3626, df=58);

* Model 7 -- retain + race;
proc logistic descending data=idp;
model VS=retain race
/aggregate=(retain agecat1 agecat3 female race transmission payer) scale=none;
run;
* (D=69.5428, df=59, significant);

* Model 8 -- retain + payer;
proc logistic descending data=idp;
model VS=retain payer
/aggregate=(retain agecat1 agecat3 female race transmission payer) scale=none;
run;
* (D=67.5218, df=59, significant);

* do not drop any of race or payer;

*** STEP 3 ***;

* Model 9 -- retain + race + payer + age;
proc logistic descending data=idp;
model VS=retain race payer agecat1 agecat3
/aggregate=(retain agecat1 agecat3 female race transmission payer) scale=none;
run;
* (D=59.5205, df=56);

* Model 10 -- retain + race + payer + female;
proc logistic descending data=idp;
model VS=retain race payer female
/aggregate=(retain agecat1 agecat3 female race transmission payer) scale=none;
run;
* (D=62.6686, df=57);

* Model 11 -- retain + race + payer + transmission;
proc logistic descending data=idp;
model VS=retain race payer transmission
/aggregate=(retain agecat1 agecat3 female race transmission payer) scale=none;
run;
* (D=63.0403, df=57);

* age, female and transmission are still not significant --do not include;

*** STEP 4 ***;

* do not need because any X cannot be dropped in STEP 2;

*** final model: retain + race + payer;
proc logistic descending data=idp;
model VS=retain race payer;
run;


*----------------------------------;
*        Model diagnostics         ;
*----------------------------------;

**************** for Q1 ******************;

*** HL test and influential points;
proc logistic descending data=idp;
model retain=agecat1 agecat3 race payer/lackfit influence(stdres);
id id;
output out=inf H=lev;
run;

data inf;
set inf;
if lev > 2*4/62; /*number of unique covariate levels*/
run;

proc print data=inf;
run;

* (p=0.9891 -> good fit, no outlier, no influential observation)

*** collinearity;
filename collin "/folders/myfolders/sasuser.v94/collin_2011.sas";
%include collin;

proc logistic descending data=idp covout outest=info;
model retain=agecat1 agecat3 race payer/covb;
run;

%collin(covdsn=info, output=col);

**************** for Q2 ******************;

*** HL test and influential points;
proc logistic descending data=idp;
model VS=retain race payer/lackfit influence(stdres);
id id;
output out=inf2 H=lev;
run;

data inf2;
set inf2;
if lev > 2*3/62; /*number of unique covariate levels*/
run;

proc print data=inf2;
run;

* (p=0.6469 -> good fit, several outlier, no influential observation)

*** collinearity;
proc logistic descending data=idp covout outest=file;
model VS=retain race payer/covb;
run;

%collin(covdsn=file, output=col);

