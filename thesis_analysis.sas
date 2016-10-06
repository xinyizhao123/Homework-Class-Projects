proc contents data=sasuser.ts;
run;

*-------------------;
*  Creating Format  ;
*-------------------;

proc format;
value eduf
  1 = "Less than high school"
  2 = "High school diploma or GED"
  3 = "More than high school";

value incomef
  1 = "Under $20,000"
  2 = "$20,000 to $44,999"
  3 = "$45,000 and over";

value racef
  1 = "Mexican American"
  2 = "Non-hispanic White"
  3 = "Non-hispanic Black"
  4 = "Other";

value npregf
  0 = "0"
  1 = "1"
  2 = "2"
  3 = "3+";

value pregmf
  1 = "1-3"
  2 = "4-6"
  3 = "7+";

value bmif
  1 = "Normal (<25)"
  2 = "Overweight (25-29.99)"
  3 = "Obese and above (30+)";

value cotf
  0 = "<= 0.1 ng/mL"
  1 = ">0.1 ng/mL";

value yesnof
  0 = "No"
  1 = "Yes";

value quartilef
  1 = "Q1"
  2 = "Q2"
  3 = "Q3"
  4 = "Q4";
run;

*-----------------;
*  Data Cleaning  ;
*-----------------;

data ts;
set sasuser.ts;
/*re-categorize annual family income*/
if income gt 0 and income le 4 then incomecat=1;
if income eq 13 then incomecat=1;
if income ge 5 and income le 7 then incomecat=2;
if income gt 7 and income lt 13 then incomecat=3;
/*1 = under $20,000
  2 = $20,000 to $44,999
  3 = $45,000 and over */

/* re-categorize race*/
if race=1 then racecat=1;
if race=2 or race=5 then racecat=4;
if race=3 then racecat=2;
if race=4 then racecat=3;
/* 1 = Mexican American
   2 = Non-hispanic White
   3 = Non hispanic Black
   4 = Other */

/* re-categorize former pregnancy times */
npr=npreg-1;
if npr=0 then np=0;
if npr=1 then np=1;
if npr=2 then np=2;
if npr ge 3 then np=3;
/* 0 = 0
   1 = 1
   2 = 2
   3 = 3+ */

/* dichotomize smoking */
if smk=1 then smkcat=1;
if smk=2 then smkcat=1;
if smk=3 then smkcat=0;

/* re-code alcohol use*/
if alc=1 then alq=1;
if alc=2 then alq=0;

/*pregnancy month category*/
if pregm ge 1 and pregm le 3 then tri=1;
if pregm ge 4 and pregm le 6 then tri=2;
if pregm ge 7 then tri=3;

/*BMI category*/
if bmi ge 0 and bmi lt 25 then bmicat=1;
if bmi ge 25 and bmi lt 30 then bmicat=2;
if bmi ge 30 then bmicat=3;
/* 1 = normal
   2 = overweight
   3 = obese and above*/

/* cotinine category*/
if cot ge 0 and cot le 0.1 then cotcat=0;
if cot gt 0.1 then cotcat=1;

/*log-transformed PFOA and PFOS*/
log_PFOA=log(PFOA);
log_PFOS=log(PFOS);

/*PFOA and PFOS category (by quartiles)*/
/*PFOA: Q1=1.2, Q2=2.3, Q3=3.2, Q4=13.6
  PFOS: Q1=6.1, Q2=10.05, Q3=14.70, Q4=39.0*/
if pfoa ge 0 and pfoa le 1.2 then pfoaq=1;
if pfoa gt 1.2 and pfoa le 2.3 then pfoaq=2;
if pfoa gt 2.3 and pfoa le 3.2 then pfoaq=3;
if pfoa gt 3.2 then pfoaq=4;

if pfos ge 0 and pfos le 6.1 then pfosq=1;
if pfos gt 6.1 and pfos le 10.05 then pfosq=2;
if pfos gt 10.05 and pfos le 14.70 then pfosq=3;
if pfos gt 14.70 then pfosq=4;

label incomecat="Annual Family Income Category"
      racecat="Race Category"
      alq="Had at least 12 alcohol drinks in any one year?"
      smkcat="Current Smoker"
      npr="Number of Previous Pregnancy"
      np="# Previous Pregnancy"
      tri="Pregnancy Month Group"
      bmicat="BMI Classification"
      cotcat="Serum Cotinine Level"
      pfoaq="PFOA Quartile"
      pfosq="PFOS Quartile";

format edu eduf.
       incomecat incomef.
       racecat racef.
       np npregf.
       tri pregmf.
       bmicat bmif.
       cotcat cotf.
       act yesnof.
       pfoaq quartilef.
       pfosq quartilef.;
run;

*check re-categorized variables;
proc freq data=ts;
tables income*incomecat/list;
run;

proc freq data=ts;
tables race*racecat/list;
run;

proc freq data=ts;
tables alc*alq/list;
run;

proc freq data=ts;
tables smk*smkcat/list;
run;

proc freq data=ts;
tables npreg*npr*np/list;
run;

proc freq data=ts;
tables pregm*tri/list;
run;

proc freq data=ts;
tables bmi*bmicat/list;
run;

proc freq data=ts;
tables cot*cotcat/list;
run;

proc freq data=ts;
tables pfoa*pfoaq/list;
run;

proc freq data=ts;
tables pfos*pfosq/list;
run;

proc contents data=ts;
run;

*--------------------------;
*  Descriptive Statistics  ;
*--------------------------;

proc means data=ts mean std median q1 q3 nmiss;
var age bmi fat cot pfoa pfos tcho hdl ldl;
run;

proc freq data=ts;
tables pfoaq pfosq racecat edu incomecat act pregm np tri bmicat cotcat;
run;

proc univariate data=ts plot;
var tcho hdl ldl pfoa pfos;
histogram tcho hdl ldl pfoa pfos/normal;
probplot tcho hdl ldl pfoa pfos;
run;

*---------------------------;
*  Univariate Associations  ;
*---------------------------;

* (1) continuous/dichotomous variable;
%macro uni(x);
proc univariate data=ts;
var &x;
histogram &x/normal;
run;

proc sgplot data=ts;
scatter Y=tcho X=&x;
reg Y=tcho X=&x;
label;
run;

proc sgplot data=ts;
scatter Y=hdl X=&x;
reg Y=hdl X=&x;
label;
run;

proc sgplot data=ts;
scatter Y=ldl X=&x;
reg Y=ldl X=&x;
label;
run;

proc reg data=ts;
model tcho=&x/clb;
run;

proc reg data=ts;
model hdl=&x/clb;
run;

proc reg data=ts;
model ldl=&x/clb;
run;

proc reg data=ts;
model log_pfoa=&x/clb;
run;

proc reg data=ts;
model log_pfos=&x/clb;
run;
%mend;

options mprint symbolgen;

%uni(pfoa);
%uni(pfos);
%uni(pfoaq);
%uni(pfosq);
%uni(log_pfoa);
%uni(log_pfos);
%uni(age);
%uni(bmi);
%uni(fat);
%uni(cot);
%uni(pregm);
%uni(npr);
%uni(act);
%uni(cotcat);

* (2) categorical variable (>2 levels);
%macro unic(x, ref);
proc sgplot data=ts;
vbar &x;
run;

proc glm data=ts;
class &x(ref=&ref);
model tcho=&x/solution clparm;
run;

proc glm data=ts;
class &x(ref=&ref);
model hdl=&x/solution clparm;
run;

proc glm data=ts;
class &x(ref=&ref);
model ldl=&x/solution clparm;
run;

proc glm data=ts;
class &x(ref=&ref);
model log_pfoa=&x/solution clparm;
run;

proc glm data=ts;
class &x(ref=&ref);
model log_pfos=&x/solution clparm;
run;
%mend;

options mprint symbolgen;

%unic(pfoaq, "Q1");
%unic(pfosq, "Q1");
%unic(edu, "Less than high school");
%unic(incomecat, "Under $20,000");
%unic(racecat, "Non-hispanic White");
%unic(tri, "1-3");
%unic(np, "0");
%unic(bmicat, "Normal (<25)");
%unic(act, "Yes");

* Pregnancy month and exposures;
proc sgplot data=ts;
scatter Y=pfoa X=pregm;
reg Y=pfoa X=pregm;
label;
run;

proc sgplot data=ts;
scatter Y=pfos X=pregm;
reg Y=pfos X=pregm;
label;
run;

proc reg data=ts;
model pfoa=pregm;
run;

proc reg data=ts;
model pfos=pregm;
run;

proc glm data=ts;
class tri(ref="1-3");
model pfoa=tri/solution;
run;

proc glm data=ts;
class tri(ref="1-3");
model pfos=tri/solution;
run;

**** Compare different scales of predictors;
* continuous scale;
%macro con(x);
proc mixed method=ml data=ts;
model tcho=&x/solution;
run;

proc mixed method=ml data=ts;
model hdl=&x/solution;
run;

proc mixed method=ml data=ts;
model ldl=&x/solution;
run;
%mend;

%con(pfoa);
%con(log_pfoa);
%con(pfos);
%con(log_pfos);
%con(bmi);
%con(cot);
%con(cotcat);
%con(pregm);
%con(npr);

* categorical scale;
%macro cat(x, ref);
proc mixed method=ml data=ts;
class &x(ref=&ref);
model tcho=&x/solution;
run;

proc mixed method=ml data=ts;
class &x(ref=&ref);
model hdl=&x/solution;
run;

proc mixed method=ml data=ts;
class &x(ref=&ref);
model ldl=&x/solution;
run;
%mend;

%cat(pfoaq, "Q1");
%cat(pfosq, "Q1");
%cat(bmicat, "Normal (<25)");
%cat(tri, "1-3");
%cat(np, "0");

*-----------------------------;
*  Multivariate Associations  ;
*-----------------------------;

****** Model 1: PFOA and total cholesterol;

* (1) only exposure;
proc glm data=ts;
model tcho=log_pfoa/solution;
run;

* (2) 2-predictor model;
%macro twopredcon(x);
proc glm data=ts;
model tcho=log_pfoa &x/solution;
run;
%mend;

%macro twopredcat(x,ref);
proc glm data=ts;
class &x(ref=&ref);
model tcho=log_pfoa &x/solution;
run;
%mend;

%twopredcon(age);
%twopredcon(pregm);
%twopredcon(fat);
%twopredcon(cotcat);
%twopredcat(edu, "Less than high school");
%twopredcat(incomecat, "Under $20,000");
%twopredcat(racecat, "Non-hispanic White");
%twopredcat(np, "0");
%twopredcat(bmicat, "Normal (<25)");
%twopredcat(act, "Yes");

* (3) 3-predictor model;

%macro threepredcon(x);
proc glm data=ts;
model tcho=log_pfoa pregm &x/solution;
run;
%mend;

%macro threepredcat(x,ref);
proc glm data=ts;
class &x(ref=&ref);
model tcho=log_pfoa pregm &x/solution;
run;
%mend;

%threepredcon(age);
%threepredcon(fat);
%threepredcon(cotcat);
%threepredcat(edu, "Less than high school");
%threepredcat(incomecat, "Under $20,000");
%threepredcat(racecat, "Non-hispanic White");
%threepredcat(np, "0");
%threepredcat(bmicat, "Normal (<25)");
%threepredcat(act, "Yes");

* (4) 4-predictor model;

%macro fourpredcon(x);
proc glm data=ts;
class np(ref="0");
model tcho=log_pfoa pregm np &x/solution;
run;
%mend;

%macro fourpredcat(x,ref);
proc glm data=ts;
class np(ref="0") &x(ref=&ref);
model tcho=log_pfoa pregm np &x/solution;
run;
%mend;

%fourpredcon(age);
%fourpredcon(fat);
%fourpredcon(cotcat);
%fourpredcat(edu, "Less than high school");
%fourpredcat(incomecat, "Under $20,000");
%fourpredcat(racecat, "Non-hispanic White");
%fourpredcat(bmicat, "Normal (<25)");
%fourpredcat(act, "Yes");

* (5) 5-predictor model;

%macro fivepredcon(x);
proc glm data=ts;
class np(ref="0") racecat(ref="Non-hispanic White");
model tcho=log_pfoa pregm np racecat &x/solution;
run;
%mend;

%macro fivepredcat(x,ref);
proc glm data=ts;
class np(ref="0") racecat(ref="Non-hispanic White") &x(ref=&ref);
model tcho=log_pfoa pregm np racecat &x/solution;
run;
%mend;

%fivepredcon(age);
%fivepredcon(fat);
%fivepredcon(cotcat);
%fivepredcat(edu, "Less than high school");
%fivepredcat(incomecat, "Under $20,000");
%fivepredcat(bmicat, "Normal (<25)");
%fivepredcat(act, "Yes");

* (6) 6-predictor model;

%macro sixpredcon(x);
proc glm data=ts;
class np(ref="0") racecat(ref="Non-hispanic White") incomecat(ref="Under $20,000");
model tcho=log_pfoa pregm np racecat incomecat &x/solution;
run;
%mend;

%macro sixpredcat(x,ref);
proc glm data=ts;
class np(ref="0") racecat(ref="Non-hispanic White") incomecat(ref="Under $20,000") &x(ref=&ref);
model tcho=log_pfoa pregm np racecat incomecat &x/solution;
run;
%mend;

%sixpredcon(age);
%sixpredcon(fat);
%sixpredcon(cotcat);
%sixpredcat(edu, "Less than high school");
%sixpredcat(bmicat, "Normal (<25)");
%sixpredcat(act, "Yes");

* (7) 7-predictor model;

%macro sevenpredcon(x);
proc glm data=ts;
class np(ref="0") racecat(ref="Non-hispanic White") incomecat(ref="Under $20,000") bmicat(ref="Normal (<25)");
model tcho=log_pfoa pregm np racecat incomecat bmicat &x/solution;
run;
%mend;

%macro sevenpredcat(x,ref);
proc glm data=ts;
class np(ref="0") racecat(ref="Non-hispanic White") incomecat(ref="Under $20,000") bmicat(ref="Normal (<25)") &x(ref=&ref);
model tcho=log_pfoa pregm np racecat incomecat bmicat &x/solution;
run;
%mend;

%sevenpredcon(age);
%sevenpredcon(fat);
%sevenpredcon(cotcat);
%sevenpredcat(edu, "Less than high school");
%sevenpredcat(act, "Yes");

* (8) 8-predictor model;

%macro eightpredcon(x);
proc glm data=ts;
class np(ref="0") racecat(ref="Non-hispanic White") incomecat(ref="Under $20,000") bmicat(ref="Normal (<25)");
model tcho=log_pfoa pregm np racecat incomecat bmicat cotcat &x/solution;
run;
%mend;

%macro eightpredcat(x,ref);
proc glm data=ts;
class np(ref="0") racecat(ref="Non-hispanic White") incomecat(ref="Under $20,000") bmicat(ref="Normal (<25)") &x(ref=&ref);
model tcho=log_pfoa pregm np racecat incomecat bmicat cotcat &x/solution;
run;
%mend;

%eightpredcon(age);
%eightpredcon(fat);
%eightpredcat(edu, "Less than high school");
%eightpredcat(act, "Yes");

* (9) 9-predictor model;

%macro ninepredcon(x);
proc glm data=ts;
class np(ref="0") racecat(ref="Non-hispanic White") incomecat(ref="Under $20,000") bmicat(ref="Normal (<25)");
model tcho=log_pfoa pregm np racecat incomecat bmicat cotcat fat &x/solution;
run;
%mend;

%macro ninepredcat(x,ref);
proc glm data=ts;
class np(ref="0") racecat(ref="Non-hispanic White") incomecat(ref="Under $20,000") bmicat(ref="Normal (<25)") &x(ref=&ref);
model tcho=log_pfoa pregm np racecat incomecat bmicat cotcat fat &x/solution;
run;
%mend;

%ninepredcon(age);
%ninepredcat(edu, "Less than high school");
%ninepredcat(act, "Yes");

* add additional covariates to be consistent with HDL model;

proc glm data=ts;
class np(ref="0") racecat(ref="Non-hispanic White") incomecat(ref="Under $20,000") bmicat(ref="Normal (<25)") edu(ref="Less than high school");
model tcho=log_pfoa pregm np racecat incomecat bmicat cotcat fat edu age/solution;
run;


****** Model 2: PFOS and total cholesterol;

* (1) only exposure;
proc glm data=ts;
model tcho=log_pfos/solution;
run;

* (2) 2-predictor model;
%macro twopredcon(x);
proc glm data=ts;
model tcho=log_pfos &x/solution;
run;
%mend;

%macro twopredcat(x,ref);
proc glm data=ts;
class &x(ref=&ref);
model tcho=log_pfos &x/solution;
run;
%mend;

%twopredcon(age);
%twopredcon(pregm);
%twopredcon(fat);
%twopredcon(cotcat);
%twopredcat(edu, "Less than high school");
%twopredcat(incomecat, "Under $20,000");
%twopredcat(racecat, "Non-hispanic White");
%twopredcat(np, "0");
%twopredcat(bmicat, "Normal (<25)");
%twopredcat(act, "Yes");

* (3) 3-predictor model;

%macro threepredcon(x);
proc glm data=ts;
class incomecat(ref="Under $20,000");
model tcho=log_pfos incomecat &x/solution;
run;
%mend;

%macro threepredcat(x,ref);
proc glm data=ts;
class incomecat(ref="Under $20,000") &x(ref=&ref);
model tcho=log_pfos incomecat &x/solution;
run;
%mend;

%threepredcon(age);
%threepredcon(pregm);
%threepredcon(fat);
%threepredcon(cotcat);
%threepredcat(edu, "Less than high school");
%threepredcat(racecat, "Non-hispanic White");
%threepredcat(np, "0");
%threepredcat(bmicat, "Normal (<25)");
%threepredcat(act, "Yes");

* (4) 4-predictor model;

%macro fourpredcon(x);
proc glm data=ts;
class incomecat(ref="Under $20,000");
model tcho=log_pfos incomecat pregm &x/solution;
run;
%mend;

%macro fourpredcat(x,ref);
proc glm data=ts;
class incomecat(ref="Under $20,000") &x(ref=&ref);
model tcho=log_pfos incomecat pregm &x/solution;
run;
%mend;

%fourpredcon(Age);
%fourpredcon(fat);
%fourpredcon(cotcat);
%fourpredcat(edu, "Less than high school");
%fourpredcat(racecat, "Non-hispanic White");
%fourpredcat(np, "0");
%fourpredcat(bmicat, "Normal (<25)");
%fourpredcat(act, "Yes");

* (5) 5-predictor model;

%macro fivepredcon(x);
proc glm data=ts;
class incomecat(ref="Under $20,000") np(ref="0");
model tcho=log_pfos incomecat pregm np &x/solution;
run;
%mend;

%macro fivepredcat(x,ref);
proc glm data=ts;
class incomecat(ref="Under $20,000") np(ref="0") &x(ref=&ref);
model tcho=log_pfos incomecat pregm np &x/solution;
run;
%mend;

%fivepredcon(age);
%fivepredcon(fat);
%fivepredcon(cotcat);
%fivepredcat(edu, "Less than high school");
%fivepredcat(racecat, "Non-hispanic White");
%fivepredcat(bmicat, "Normal (<25)");
%fivepredcat(act, "Yes");

* (6) 6-predictor model;

%macro sixpredcon(x);
proc glm data=ts;
class np(ref="0") racecat(ref="Non-hispanic White") incomecat(ref="Under $20,000");
model tcho=log_pfos pregm np racecat incomecat &x/solution;
run;
%mend;

%macro sixpredcat(x,ref);
proc glm data=ts;
class np(ref="0") racecat(ref="Non-hispanic White") incomecat(ref="Under $20,000") &x(ref=&ref);
model tcho=log_pfos pregm np racecat incomecat &x/solution;
run;
%mend;

%sixpredcon(age);
%sixpredcon(fat);
%sixpredcon(cotcat);
%sixpredcat(edu, "Less than high school");
%sixpredcat(bmicat, "Normal (<25)");
%sixpredcat(act, "Yes");

* (7) 7-predictor model;

%macro sevenpredcon(x);
proc glm data=ts;
class np(ref="0") racecat(ref="Non-hispanic White") incomecat(ref="Under $20,000");
model tcho=log_pfos pregm np racecat incomecat fat &x/solution;
run;
%mend;

%macro sevenpredcat(x,ref);
proc glm data=ts;
class np(ref="0") racecat(ref="Non-hispanic White") incomecat(ref="Under $20,000") &x(ref=&ref);
model tcho=log_pfos pregm np racecat incomecat fat &x/solution;
run;
%mend;

%sevenpredcon(age);
%sevenpredcon(cotcat);
%sevenpredcat(edu, "Less than high school");
%sevenpredcat(bmicat, "Normal (<25)");
%sevenpredcat(act, "Yes");

* (8) 8-predictor model;

%macro eightpredcon(x);
proc glm data=ts;
class np(ref="0") racecat(ref="Non-hispanic White") incomecat(ref="Under $20,000");
model tcho=log_pfos pregm np racecat incomecat fat cotcat &x/solution;
run;
%mend;

%macro eightpredcat(x,ref);
proc glm data=ts;
class np(ref="0") racecat(ref="Non-hispanic White") incomecat(ref="Under $20,000") &x(ref=&ref);
model tcho=log_pfos pregm np racecat incomecat fat cotcat &x/solution;
run;
%mend;

%eightpredcon(age);
%eightpredcat(edu, "Less than high school");
%eightpredcat(bmicat, "Normal (<25)");
%eightpredcat(act, "Yes");

* add additional covariates to be consistent with HDL model;
proc glm data=ts;
class np(ref="0") racecat(ref="Non-hispanic White") incomecat(ref="Under $20,000") bmicat(ref="Normal (<25)") edu(ref="Less than high school");
model tcho=log_pfos pregm np racecat incomecat bmicat cotcat fat edu age/solution;
run;


****** Model 3: PFOA and HDL;

* (1) only exposure;
proc glm data=ts;
model HDL=log_pfoa/solution;
run;

* (2) 2-predictor model;
%macro twopredcon(x);
proc glm data=ts;
model HDL=log_pfoa &x/solution;
run;
%mend;

%macro twopredcat(x,ref);
proc glm data=ts;
class &x(ref=&ref);
model HDL=log_pfoa &x/solution;
run;
%mend;

%twopredcon(age);
%twopredcon(pregm);
%twopredcon(fat);
%twopredcon(cotcat);
%twopredcat(edu, "Less than high school");
%twopredcat(incomecat, "Under $20,000");
%twopredcat(racecat, "Non-hispanic White");
%twopredcat(np, "0");
%twopredcat(bmicat, "Normal (<25)");
%twopredcat(act, "Yes");

* (3) 3-predictor model;

%macro threepredcon(x);
proc glm data=ts;
class incomecat(ref="Under $20,000");
model HDL=log_pfoa incomecat &x/solution;
run;
%mend;

%macro threepredcat(x,ref);
proc glm data=ts;
class incomecat(ref="Under $20,000") &x(ref=&ref);
model HDL=log_pfoa incomecat &x/solution;
run;
%mend;

%threepredcon(age);
%threepredcon(pregm);
%threepredcon(fat);
%threepredcon(cotcat);
%threepredcat(edu, "Less than high school");
%threepredcat(racecat, "Non-hispanic White");
%threepredcat(np, "0");
%threepredcat(bmicat, "Normal (<25)");
%threepredcat(act, "Yes");

* (4) 4-predictor model;

%macro fourpredcon(x);
proc glm data=ts;
class incomecat(ref="Under $20,000");
model HDL=log_pfoa incomecat age &x/solution;
run;
%mend;

%macro fourpredcat(x,ref);
proc glm data=ts;
class incomecat(ref="Under $20,000") &x(ref=&ref);
model HDL=log_pfoa incomecat age &x/solution;
run;
%mend;

%fourpredcon(pregm);
%fourpredcon(fat);
%fourpredcon(cotcat);
%fourpredcat(edu, "Less than high school");
%fourpredcat(racecat, "Non-hispanic White");
%fourpredcat(np, "0");
%fourpredcat(bmicat, "Normal (<25)");
%fourpredcat(act, "Yes");

* (5) 5-predictor model;

%macro fivepredcon(x);
proc glm data=ts;
class incomecat(ref="Under $20,000") edu(ref="Less than high school");
model HDL=log_pfoa incomecat age edu &x/solution;
run;
%mend;

%macro fivepredcat(x,ref);
proc glm data=ts;
class  incomecat(ref="Under $20,000") edu(ref="Less than high school") &x(ref=&ref);
model HDL=log_pfoa incomecat age edu &x/solution;
run;
%mend;

%fivepredcon(pregm);
%fivepredcon(fat);
%fivepredcon(cotcat);
%fivepredcat(racecat, "Non-hispanic White");
%fivepredcat(np, "0");
%fivepredcat(bmicat, "Normal (<25)");
%fivepredcat(act, "Yes");

* (6) 6-predictor model;

%macro sixpredcon(x);
proc glm data=ts;
class incomecat(ref="Under $20,000") edu(ref="Less than high school") racecat(ref="Non-hispanic White");
model HDL=log_pfoa incomecat age edu racecat &x/solution;
run;
%mend;

%macro sixpredcat(x,ref);
proc glm data=ts;
class incomecat(ref="Under $20,000") edu(ref="Less than high school")racecat(ref="Non-hispanic White") &x(ref=&ref);
model HDL=log_pfoa incomecat age edu racecat &x/solution;
run;
%mend;

%sixpredcon(pregm);
%sixpredcon(fat);
%sixpredcon(cotcat);
%sixpredcat(np, "0");
%sixpredcat(bmicat, "Normal (<25)");
%sixpredcat(act, "Yes");

* (7) 7-predictor model;

%macro sevenpredcon(x);
proc glm data=ts;
class incomecat(ref="Under $20,000") edu(ref="Less than high school") racecat(ref="Non-hispanic White")np(ref="0");
model HDL=log_pfoa age incomecat edu racecat np &x/solution;
run;
%mend;

%macro sevenpredcat(x,ref);
proc glm data=ts;
class incomecat(ref="Under $20,000") edu(ref="Less than high school")racecat(ref="Non-hispanic White")np(ref="0") &x(ref=&ref);
model HDL=log_pfoa age incomecat edu racecat np &x/solution;
run;
%mend;

%sevenpredcon(pregm);
%sevenpredcon(fat);
%sevenpredcon(cotcat);
%sevenpredcat(bmicat, "Normal (<25)");
%sevenpredcat(act, "Yes");

* (8) 8-predictor model;

%macro eightpredcon(x);
proc glm data=ts;
class incomecat(ref="Under $20,000") edu(ref="Less than high school") racecat(ref="Non-hispanic White")np(ref="0");
model HDL=log_pfoa age incomecat edu racecat np pregm &x/solution;
run;
%mend;

%macro eightpredcat(x,ref);
proc glm data=ts;
class incomecat(ref="Under $20,000") edu(ref="Less than high school")racecat(ref="Non-hispanic White")np(ref="0") &x(ref=&ref);
model HDL=log_pfoa age incomecat edu racecat np pregm &x/solution;
run;
%mend;

%eightpredcon(fat);
%eightpredcon(cotcat);
%eightpredcat(bmicat, "Normal (<25)");
%eightpredcat(act, "Yes");

* add additional covariates to be consistent with TCHO model;
proc glm data=ts;
class np(ref="0") racecat(ref="Non-hispanic White") incomecat(ref="Under $20,000") bmicat(ref="Normal (<25)") edu(ref="Less than high school");
model hdl=log_pfoa pregm np racecat incomecat bmicat cotcat fat edu age/solution;
run;

****** Model 4: PFOS and HDL;

* (1) only exposure;
proc glm data=ts;
model HDL=log_pfos/solution;
run;

* (2) 2-predictor model;
%macro twopredcon(x);
proc glm data=ts;
model HDL=log_pfos &x/solution;
run;
%mend;

%macro twopredcat(x,ref);
proc glm data=ts;
class &x(ref=&ref);
model HDL=log_pfos &x/solution;
run;
%mend;

%twopredcon(age);
%twopredcon(pregm);
%twopredcon(fat);
%twopredcon(cotcat);
%twopredcat(edu, "Less than high school");
%twopredcat(incomecat, "Under $20,000");
%twopredcat(racecat, "Non-hispanic White");
%twopredcat(np, "0");
%twopredcat(bmicat, "Normal (<25)");
%twopredcat(act, "Yes");

* (3) 3-predictor model;

%macro threepredcon(x);
proc glm data=ts;
class incomecat(ref="Under $20,000");
model HDL=log_pfos incomecat &x/solution;
run;
%mend;

%macro threepredcat(x,ref);
proc glm data=ts;
class incomecat(ref="Under $20,000") &x(ref=&ref);
model HDL=log_pfos incomecat &x/solution;
run;
%mend;

%threepredcon(age);
%threepredcon(pregm);
%threepredcon(fat);
%threepredcon(cotcat);
%threepredcat(edu, "Less than high school");
%threepredcat(racecat, "Non-hispanic White");
%threepredcat(np, "0");
%threepredcat(bmicat, "Normal (<25)");
%threepredcat(act, "Yes");

* (4) 4-predictor model;

%macro fourpredcon(x);
proc glm data=ts;
class incomecat(ref="Under $20,000");
model HDL=log_pfos incomecat pregm &x/solution;
run;
%mend;

%macro fourpredcat(x,ref);
proc glm data=ts;
class incomecat(ref="Under $20,000") &x(ref=&ref);
model HDL=log_pfos incomecat pregm &x/solution;
run;
%mend;

%fourpredcon(Age);
%fourpredcon(fat);
%fourpredcon(cotcat);
%fourpredcat(edu, "Less than high school");
%fourpredcat(racecat, "Non-hispanic White");
%fourpredcat(np, "0");
%fourpredcat(bmicat, "Normal (<25)");
%fourpredcat(act, "Yes");

* (5) 5-predictor model;

%macro fivepredcon(x);
proc glm data=ts;
class incomecat(ref="Under $20,000") np(ref="0");
model HDL=log_pfos incomecat pregm np &x/solution;
run;
%mend;

%macro fivepredcat(x,ref);
proc glm data=ts;
class incomecat(ref="Under $20,000") np(ref="0") &x(ref=&ref);
model HDL=log_pfos incomecat pregm np &x/solution;
run;
%mend;

%fivepredcon(age);
%fivepredcon(fat);
%fivepredcon(cotcat);
%fivepredcat(edu, "Less than high school");
%fivepredcat(racecat, "Non-hispanic White");
%fivepredcat(bmicat, "Normal (<25)");
%fivepredcat(act, "Yes");

* (6) 6-predictor model;

%macro sixpredcon(x);
proc glm data=ts;
class np(ref="0") incomecat(ref="Under $20,000");
model HDL=log_pfos pregm np cotcat incomecat &x/solution;
run;
%mend;

%macro sixpredcat(x,ref);
proc glm data=ts;
class np(ref="0") incomecat(ref="Under $20,000") &x(ref=&ref);
model HDL=log_pfos pregm np cotcat incomecat &x/solution;
run;
%mend;

%sixpredcon(age);
%sixpredcon(fat);
%sixpredcat(edu, "Less than high school");
%sixpredcat(racecat, "Non-hispanic White");
%sixpredcat(bmicat, "Normal (<25)");
%sixpredcat(act, "Yes");

* (7) 7-predictor model;

%macro sevenpredcon(x);
proc glm data=ts;
class np(ref="0") incomecat(ref="Under $20,000");
model HDL=log_pfos pregm np cotcat incomecat fat &x/solution;
run;
%mend;

%macro sevenpredcat(x,ref);
proc glm data=ts;
class np(ref="0") incomecat(ref="Under $20,000") &x(ref=&ref);
model HDL=log_pfos pregm np cotcat incomecat fat &x/solution;
run;
%mend;

%sevenpredcon(age);
%sevenpredcat(edu, "Less than high school");
%sevenpredcat(racecat, "Non-hispanic White");
%sevenpredcat(bmicat, "Normal (<25)");
%sevenpredcat(act, "Yes");

* (8) 8-predictor model;

%macro eightpredcon(x);
proc glm data=ts;
class np(ref="0") incomecat(ref="Under $20,000") bmicat(ref="Normal (<25)");
model HDL=log_pfos pregm np cotcat incomecat fat bmicat &x/solution;
run;
%mend;

%macro eightpredcat(x,ref);
proc glm data=ts;
class np(ref="0") incomecat(ref="Under $20,000") bmicat(ref="Normal (<25)") &x(ref=&ref);
model HDL=log_pfos pregm np cotcat incomecat fat bmicat &x/solution;
run;
%mend;

%eightpredcon(age);
%eightpredcat(edu, "Less than high school");
%eightpredcat(racecat, "Non-hispanic White");
%eightpredcat(act, "Yes");

* (9) 9-predictor model;

%macro ninepredcon(x);
proc glm data=ts;
class np(ref="0") incomecat(ref="Under $20,000") bmicat(ref="Normal (<25)") racecat(ref="Non-hispanic White");
model HDL=log_pfos pregm np cotcat incomecat fat bmicat racecat &x/solution;
run;
%mend;

%macro ninepredcat(x,ref);
proc glm data=ts;
class np(ref="0") incomecat(ref="Under $20,000") bmicat(ref="Normal (<25)") racecat(ref="Non-hispanic White") &x(ref=&ref);
model HDL=log_pfos pregm np cotcat incomecat fat bmicat racecat &x/solution;
run;
%mend;

%ninepredcon(age);
%ninepredcat(edu, "Less than high school");
%ninepredcat(act, "Yes");

* (10) 10-predictor model;

%macro tenpredcon(x);
proc glm data=ts;
class np(ref="0") incomecat(ref="Under $20,000") bmicat(ref="Normal (<25)") racecat(ref="Non-hispanic White") edu(ref="Less than high school");
model HDL=log_pfos pregm np cotcat incomecat fat bmicat racecat edu &x/solution;
run;
%mend;

%macro tenpredcat(x,ref);
proc glm data=ts;
class np(ref="0") incomecat(ref="Under $20,000") bmicat(ref="Normal (<25)") racecat(ref="Non-hispanic White") edu(ref="Less than high school") &x(ref=&ref);
model HDL=log_pfos pregm np cotcat incomecat fat bmicat racecat edu &x/solution;
run;
%mend;

%tenpredcon(age);
%tenpredcat(act, "Yes");

* add additional covariates to be consistent with TCHO model;
proc glm data=ts;
class np(ref="0") racecat(ref="Non-hispanic White") incomecat(ref="Under $20,000") bmicat(ref="Normal (<25)") edu(ref="Less than high school");
model hdl=log_pfos pregm np racecat incomecat bmicat cotcat fat edu age/solution;
run;

***************** FINAL MODELS;

* use macro to simplify coding because all models control for the same covariates;
%macro fm(x, y);
proc glm data=ts;
class np(ref="0") incomecat(ref="Under $20,000") bmicat(ref="Normal (<25)") racecat(ref="Non-hispanic White") edu(ref="Less than high school");
model &y=&x pregm np cotcat incomecat fat bmicat racecat edu age/solution clparm;
run;
%mend;

%fm(log_pfoa, tcho);
%fm(log_pfoa, hdl);
%fm(log_pfoa, ldl);
%fm(log_pfos, tcho);
%fm(log_pfos, hdl);
%fm(log_pfos, ldl);

*-------------------------;
*    Model Diagnostics    ;
*-------------------------;

* to get VIF, we need to use proc reg instead of proc glm;
* to perform proc reg, we need to create indicator variables first;

data diag;
set ts;

incomecat2=0; incomecat3=0;
if incomecat=2 then incomecat2=1;
if incomecat=3 then incomecat3=1;
if incomecat=. then do; incomecat2=.; incomecat3=.; end;

edu2=0; edu3=0;
if edu=2 then edu2=1;
if edu=3 then edu3=1;
if edu=. then do; edu2=.; edu3=.; end;

bmicat2=0; bmicat3=0;
if bmicat=2 then bmicat2=1;
if bmicat=3 then bmicat3=1;
if bmicat=. then do; bmicat2=.; bmicat3=.; end;

racecat1=0; racecat3=0; racecat4=0;
if racecat=1 then racecat1=1;
if racecat=3 then racecat3=1;
if racecat=4 then racecat4=1;
if racecat=. then do; racecat1=.; racecat3=.; racecat4=.; end;

np1=0; np2=0; np3=0;
if np=1 then np1=1;
if np=2 then np2=1;
if np=3 then np3=1;
if np=. then do; np1=.; np2=.; np3=.; end;

run;

proc freq data=diag;
tables incomecat*incomecat2*incomecat3/list;
run;

proc freq data=diag;
tables edu*edu2*edu3/list;
run;

proc freq data=diag;
tables bmicat*bmicat2*bmicat3/list;
run;

proc freq data=diag;
tables racecat*racecat1*racecat3*racecat4/list;
run;

proc freq data=diag;
tables np*np1*np2*np3/list;
run;

* Diagnostics;
%macro dgn(x, y, t, n);
proc reg data=diag;
model &y=&x incomecat2 incomecat3 pregm np1 np2 np3 cotcat fat bmicat2 bmicat3 
      racecat1 racecat3 racecat4 edu2 edu3 age/clb partial pcorr2 influence R vif;
id id;
  output
   out=resid 
   R=residual  
   rstudent=jackres 
   cookd=cooks  
   H =leverage;
run;

data resid;
  set resid;
  outlier_jk = (abs(jackres) > &t);
  outlier_cooks = (cooks > 18/&n);
  outlier_leverage = (leverage > 2*18/&n);
  if sum(outlier_jk, outlier_cooks, outlier_leverage) >=1;
  nm = nmiss(of &x &y incomecat pregm np cotcat fat bmicat racecat edu age);
  if nm > 0 then delete;
  keep id residual cooks leverage jackres outlier_jk outlier_cooks outlier_leverage;
run;

proc print data=resid;
run;
%mend;

%dgn(log_pfoa, tcho, 1.98, 134);
%dgn(log_pfoa, hdl, 1.98, 134);
%dgn(log_pfoa, ldl, 2, 66);
%dgn(log_pfos, tcho, 1.98, 134);
%dgn(log_pfos, hdl, 1.98, 134);
%dgn(log_pfos, ldl, 2, 66);

******* Models for PFOA/PFOS in quartiles;

%macro fm(x, y);
proc glm data=ts;
class &x(ref="Q1") np(ref="0") incomecat(ref="Under $20,000") bmicat(ref="Normal (<25)") racecat(ref="Non-hispanic White") edu(ref="Less than high school");
model &y=&x pregm np cotcat incomecat fat bmicat racecat edu age/solution clparm;
run;
%mend;

%fm(pfoaq, tcho);
%fm(pfoaq, hdl);
%fm(pfoaq, ldl);
%fm(pfosq, tcho);
%fm(pfosq, hdl);
%fm(pfosq, ldl);