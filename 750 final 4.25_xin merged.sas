

libname H "H:\courses documents\EPI750\Final Project";

*import the dataset;
data cdi;
set h.cdi_trend1;
lgpdays = log(pdays);
collabi = collab-1;
run;

proc univariate data=cdi;
var admprevcocdi;
run;

proc freq data=cdi;
tables collab*collabi/list;
run;

***** Possible correlation structures;
* AR(1), TOEP(13) and independent;

****** INITIAL MODEL + COLLINEARITY ASSESSMENT + TEST OF RANDOM EFFECTS;

filename collin "/folders/myfolders/sasuser.v94/collin_2011.sas";
%include collin;

**** full model;
** intercept+slope corr=IND;
proc glimmix data=cdi empirical;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL") medtype(ref="N");
model y=collabi admprevcocdi bedsize medtype test_type
collabi*admprevcocdi collabi*bedsize collabi*medtype collabi*test_type
/dist=poisson link=log offset=lgpdays s cl covb;
random _residual_/subject=organizationid;
random int collabi/subject=organizationid g s vcorr type=un;
covtest 'R=independent G=0' glm; 
covtest 'R=independent G=Specifized' diagr;
covtest 'R=specified G=0' zerog; 
nloptions tech = nrridg;
ods output glimmix.covb=GLIMMIX1;
run;
%collin(covdsn=GLIMMIX1, procdr=GLIMMIX, output=col);

** intercept corr=IND;
proc glimmix data=cdi empirical;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL") medtype(ref="N");
model y=collabi admprevcocdi bedsize medtype test_type
collabi*admprevcocdi collabi*bedsize collabi*medtype collabi*test_type
/dist=poisson link=log offset=lgpdays s cl covb;
random _residual_/subject=organizationid;
random int /subject=organizationid g s vcorr type=un;
covtest 'R=independent G=0' glm; 
covtest 'R=independent G=Specifized' diagr;
covtest 'R=specified G=0' zerog; 
nloptions tech = nrridg;
ods output glimmix.covb=GLIMMIX1;
run;
%collin(covdsn=GLIMMIX1, procdr=GLIMMIX, output=col);

*COVTEST;
Data TestDataSet;
Input covp1 covp2 covp3;
Datalines;
. 0 . /*DIAGG*/
0 0 . /* test if random intercept is significant*/
. 0 0 /* test if random slope is significant*/
;


** intercept+slope corr = ar(1);
proc glimmix data=cdi empirical IC=pq;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL") medtype(ref="N");
model y=collabi admprevcocdi bedsize medtype test_type
collabi*admprevcocdi collabi*bedsize collabi*medtype collabi*test_type
/dist=poisson link=log offset=lgpdays s cl covb;
random _residual_/subject=organizationid type=ar(1);
random int collabi/subject=organizationid g s vcorr ;
covtest 'R=independent G=0' glm; 
covtest 'R=independent G=Specifized' diagr;
covtest 'R=specified G=0' zerog; 
covtest 'Random effects' testdata=TestDataSet;
nloptions tech = nrridg;
ods output glimmix.covb=GLIMMIX1;
run;
%collin(covdsn=GLIMMIX1, procdr=GLIMMIX, output=col);

** intercept corr = ar(1);
proc glimmix data=cdi empirical;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL") medtype(ref="N");
model y=collabi admprevcocdi bedsize medtype test_type
collabi*admprevcocdi collabi*bedsize collabi*medtype collabi*test_type
/dist=poisson link=log offset=lgpdays s cl covb;
random _residual_/subject=organizationid type=ar(1);
random int /subject=organizationid g s vcorr type=un;
covtest 'R=independent G=0' glm; 
covtest 'R=independent G=Specifized' diagr;
covtest 'R=specified G=0' zerog; 
covtest 'Random effects' testdata=TestDataSet;
nloptions tech = nrridg;
ods output glimmix.covb=GLIMMIX1;
run;
%collin(covdsn=GLIMMIX1, procdr=GLIMMIX, output=col);

** (3) intercept_slope corr = toep;
proc glimmix data=cdi empirical IC=pq;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL") medtype(ref="N");
model y=collabi admprevcocdi bedsize medtype test_type
collabi*admprevcocdi collabi*bedsize collabi*medtype collabi*test_type
/dist=poisson link=log offset=lgpdays s cl covb;
random _residual_/subject=organizationid type=toep(13);
random int collabi/subject=organizationid g s vcorr ;
covtest 'R=independent G=0' glm; 
covtest 'R=independent G=Specifized' diagr;
covtest 'R=specified G=0' zerog; 
covtest 'Random effects' testdata=TestDataSet;
nloptions tech = nrridg;
ods output glimmix.covb=GLIMMIX1;
run;
%collin(covdsn=GLIMMIX1, procdr=GLIMMIX, output=col);

** intercept corr = toep;
proc glimmix data=cdi empirical;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL") medtype(ref="N");
model y=collabi admprevcocdi bedsize medtype test_type
collabi*admprevcocdi collabi*bedsize collabi*medtype collabi*test_type
/dist=poisson link=log offset=lgpdays s cl covb;
random _residual_/subject=organizationid type=toep(13);
random int /subject=organizationid g s vcorr type=un;
covtest 'R=independent G=0' glm; 
covtest 'R=independent G=Specifized' diagr;
covtest 'R=specified G=0' zerog; 
covtest 'Random effects' testdata=TestDataSet;
nloptions tech = nrridg;
ods output glimmix.covb=GLIMMIX1;
run;
%collin(covdsn=GLIMMIX1, procdr=GLIMMIX, output=col);

** (3) intercept_slope corr = cs;
proc glimmix data=cdi empirical IC=pq;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL") medtype(ref="N");
model y=collabi admprevcocdi bedsize medtype test_type
collabi*admprevcocdi collabi*bedsize collabi*medtype collabi*test_type
/dist=poisson link=log offset=lgpdays ;
random _residual_/subject=organizationid type=cs;
random int collabi/subject=organizationid ;
covtest 'R=independent G=0' glm; 
covtest 'R=independent G=Specifized' diagr;
covtest 'R=specified G=0' zerog; 
covtest 'Random effects' testdata=TestDataSet;
nloptions tech = nrridg;
ods output glimmix.covb=GLIMMIX1;
run;
%collin(covdsn=GLIMMIX1, procdr=GLIMMIX, output=col);

** intercept corr = cs;
proc glimmix data=cdi empirical;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL") medtype(ref="N");
model y=collabi admprevcocdi bedsize medtype test_type
collabi*admprevcocdi collabi*bedsize collabi*medtype collabi*test_type
/dist=poisson link=log offset=lgpdays s cl covb;
random _residual_/subject=organizationid type=cs;
random int /subject=organizationid g s vcorr type=un;
covtest 'R=independent G=0' glm; 
covtest 'R=independent G=Specifized' diagr;
covtest 'R=specified G=0' zerog; 
covtest 'Random effects' testdata=TestDataSet;
nloptions tech = nrridg;
ods output glimmix.covb=GLIMMIX1;
run;
%collin(covdsn=GLIMMIX1, procdr=GLIMMIX, output=col);

* NO COLLINEARITY (MAX CI = 7.7);
* BOTH RANDOM INTERCEPT AND RANDOM SLOPE ARE SIGNIFICANT;



****** INTERACTION ASSESSMENT;

*eliminate the most insignificant interaction collabi*medtype;

** corr=IND;
proc glimmix data=cdi empirical;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL") medtype(ref="N");
model y=collabi admprevcocdi bedsize medtype test_type
collabi*admprevcocdi collabi*bedsize collabi*test_type
/dist=poisson link=log offset=lgpdays s cl covb;
random _residual_/subject=organizationid;
random int collabi/subject=organizationid g s vcorr type=un;
nloptions tech = nrridg;
run; 

** corr=TOEP;
proc glimmix data=cdi empirical;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL") medtype(ref="N");
model y=collabi admprevcocdi bedsize medtype test_type
collabi*admprevcocdi collabi*bedsize collabi*test_type
/dist=poisson link=log offset=lgpdays s cl covb;
random _residual_/subject=organizationid type=toep(13);
random int collabi/subject=organizationid g s vcorr type=un;
nloptions tech = nrridg;
run; 

** corr=AR(1);
proc glimmix data=cdi empirical;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL") medtype(ref="N");
model y=collabi admprevcocdi bedsize medtype test_type
collabi*admprevcocdi collabi*bedsize collabi*test_type
/dist=poisson link=log offset=lgpdays s cl covb;
random _residual_/subject=organizationid type=ar(1);
random int /subject=organizationid g s vcorr type=un;
nloptions tech = nrridg;
run; 

** corr=cs;
proc glimmix data=cdi empirical;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL") medtype(ref="N");
model y=collabi admprevcocdi bedsize medtype test_type
collabi*admprevcocdi collabi*bedsize collabi*test_type
/dist=poisson link=log offset=lgpdays s cl covb;
random _residual_/subject=organizationid type=cs;
random int /subject=organizationid g s vcorr type=un;
nloptions tech = nrridg;
run; 

*eliminate the most insignificant interaction collabi*bedsize;

** corr=IND;
proc glimmix data=cdi empirical;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL") medtype(ref="N");
model y=collabi admprevcocdi bedsize medtype test_type
collabi*admprevcocdi collabi*test_type
/dist=poisson link=log offset=lgpdays s cl;
random _residual_/subject=organizationid;
random int collabi /subject=organizationid g s type=un;
nloptions tech = nrridg;
run;

** corr=TOEP;
proc glimmix data=cdi empirical;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL") medtype(ref="N");
model y=collabi admprevcocdi bedsize medtype test_type
collabi*admprevcocdi collabi*test_type
/dist=poisson link=log offset=lgpdays s cl;
random _residual_/subject=organizationid type=toep(13);
random int collabi /subject=organizationid g s type=un;
nloptions tech = nrridg;
run;

** corr=AR(1);
proc glimmix data=cdi empirical;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL") medtype(ref="N");
model y=collabi admprevcocdi bedsize medtype test_type
collabi*admprevcocdi collabi*test_type
/dist=poisson link=log offset=lgpdays s cl;
random _residual_/subject=organizationid type=ar(1);
random int collabi /subject=organizationid g s type=un;
nloptions tech = nrridg;
run;

** corr=cs;
proc glimmix data=cdi empirical;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL") medtype(ref="N");
model y=collabi admprevcocdi bedsize medtype test_type
collabi*admprevcocdi collabi*test_type
/dist=poisson link=log offset=lgpdays s cl;
random _residual_/subject=organizationid type=cs;
random int collabi /subject=organizationid g s type=un;
nloptions tech = nrridg;
run;

*eliminate the most insignificant interaction collabi*test_type;

** corr=IND;
proc glimmix data=cdi empirical;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL") medtype(ref="N");
model y=collabi admprevcocdi bedsize medtype test_type 
collabi*admprevcocdi
/dist=poisson link=log offset=lgpdays s cl;
random _residual_/subject=organizationid;
random int collabi /subject=organizationid g s type=un;
nloptions tech = nrridg;
run;

** corr=TOEP;
proc glimmix data=cdi empirical;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL") medtype(ref="N");
model y=collabi admprevcocdi bedsize medtype test_type 
collabi*admprevcocdi
/dist=poisson link=log offset=lgpdays s cl;
random _residual_/subject=organizationid type=toep(13);
random int collabi /subject=organizationid g s type=un;
nloptions tech = nrridg;
run;

** corr=AR(1);
proc glimmix data=cdi empirical;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL") medtype(ref="N");
model y=collabi admprevcocdi bedsize medtype test_type 
collabi*admprevcocdi
/dist=poisson link=log offset=lgpdays s cl;
random _residual_/subject=organizationid type=ar(1);
random int collabi /subject=organizationid g s type=un;
nloptions tech = nrridg;
run;

** corr=cs;
proc glimmix data=cdi empirical;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL") medtype(ref="N");
model y=collabi admprevcocdi bedsize medtype test_type 
collabi*admprevcocdi
/dist=poisson link=log offset=lgpdays s cl;
random _residual_/subject=organizationid type=cs;
random int collabi /subject=organizationid g s type=un;
nloptions tech = nrridg;
run;

********* confounding (using Backward elimination based on p-values);

* model after interaction assessment (GS model);

** corr=IND;
proc glimmix data=cdi empirical;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL") medtype(ref="N");
model y=collabi admprevcocdi bedsize medtype test_type 
collabi*admprevcocdi
/dist=poisson link=log offset=lgpdays s cl;
random _residual_/subject=organizationid;
random int collabi /subject=organizationid g s type=un;
nloptions tech = nrridg;
run;

** corr=TOEP;
proc glimmix data=cdi empirical;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL") medtype(ref="N");
model y=collabi admprevcocdi bedsize medtype test_type 
collabi*admprevcocdi
/dist=poisson link=log offset=lgpdays s cl;
random _residual_/subject=organizationid type=toep(13);
random int collabi /subject=organizationid g s type=un;
nloptions tech = nrridg;
run;

** corr=AR(1);
proc glimmix data=cdi empirical;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL") medtype(ref="N");
model y=collabi admprevcocdi bedsize medtype test_type 
collabi*admprevcocdi
/dist=poisson link=log offset=lgpdays s cl;
random _residual_/subject=organizationid type=ar(1);
random int collabi /subject=organizationid g s type=un;
nloptions tech = nrridg;
run;

** corr=cs;
proc glimmix data=cdi empirical;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL") medtype(ref="N");
model y=collabi admprevcocdi bedsize medtype test_type 
collabi*admprevcocdi
/dist=poisson link=log offset=lgpdays s cl;
random _residual_/subject=organizationid type=cs;
random int collabi /subject=organizationid g s type=un;
nloptions tech = nrridg;
run;

* drop the least significant variable and refit the model;

** corr=IND (medtype, p=0.10);
proc glimmix data=cdi empirical;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL");
model y=collabi admprevcocdi bedsize test_type collabi*admprevcocdi
/dist=poisson link=log offset=lgpdays s cl;
random _residual_/subject=organizationid;
random int collabi /subject=organizationid g s type=un;
nloptions tech = nrridg;
run;
* change = 0.68%;

** corr=TOEP (medtype, p=0.09);
proc glimmix data=cdi empirical;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL");
model y=collabi admprevcocdi bedsize test_type 
collabi*admprevcocdi
/dist=poisson link=log offset=lgpdays s cl;
random _residual_/subject=organizationid type=toep(13);
random int collabi /subject=organizationid g s type=un;
nloptions tech = nrridg;
run;
* change = 0.70%;

** corr=AR(1);
proc glimmix data=cdi empirical;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL");
model y=collabi admprevcocdi bedsize test_type 
collabi*admprevcocdi
/dist=poisson link=log offset=lgpdays s cl;
random _residual_/subject=organizationid type=ar(1);
random int collabi /subject=organizationid g s type=un;
nloptions tech = nrridg;
run;
* change = 0.70%;

** corr=cs;
proc glimmix data=cdi empirical;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL");
model y=collabi admprevcocdi bedsize test_type 
collabi*admprevcocdi
/dist=poisson link=log offset=lgpdays s cl;
random _residual_/subject=organizationid type=cs;
random int collabi /subject=organizationid g s type=un;
nloptions tech = nrridg;
run;

* all the remaining variables are significant;



******* BEST MODELS;

** corr=IND;
proc glimmix data=cdi empirical ic=q;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL");
model y=collabi admprevcocdi bedsize test_type collabi*admprevcocdi
/dist=poisson link=log offset=lgpdays s cl;
random _residual_/subject=organizationid;
random int collabi /subject=organizationid g s type=un;
nloptions tech = nrridg;
run;

** corr=cs;
proc glimmix data=cdi empirical ic=q;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL");
model y=collabi admprevcocdi bedsize test_type collabi*admprevcocdi
/dist=poisson link=log offset=lgpdays s cl;
random _residual_/subject=organizationid type=cs;
random int collabi /subject=organizationid g s ;
covtest 'R=independent G=0' glm; 
covtest 'R=independent G=Specifized' diagr;
covtest 'R=specified G=0' zerog; 
covtest 'Random effects' testdata=TestDataSet;
nloptions tech = nrridg;
run;

** corr=AR(1);
proc glimmix data=cdi empirical ic=q;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL");
model y=collabi admprevcocdi bedsize test_type collabi*admprevcocdi
/dist=poisson link=log offset=lgpdays s cl;
random _residual_/subject=organizationid type=ar(1);
random int collabi /subject=organizationid g s ;
covtest 'R=independent G=0' glm; 
covtest 'R=independent G=Specifized' diagr;
covtest 'R=specified G=0' zerog; 
covtest 'Random effects' testdata=TestDataSet;
nloptions tech = nrridg;
run;

** corr=TOEP --best model;
proc glimmix data=cdi empirical ic=q;
class OrganizationID test_type(ref="EIA") bedsize(ref="SMALL");
model y=collabi admprevcocdi bedsize test_type collabi*admprevcocdi
/dist=poisson link=log offset=lgpdays s cl;
random _residual_/subject=organizationid type=toep(13);
random int collabi /subject=organizationid g s ;
covtest 'R=independent G=0' glm; 
covtest 'R=independent G=Specifized' diagr;
covtest 'R=specified G=0' zerog; 
covtest 'Random effects' testdata=TestDataSet;
nloptions tech = nrridg;
estimate "collab II no interaction" collabi 1 collabi*admprevcocdi 0/ exp cl;
estimate "collab II interaction" collabi 1 collabi*admprevcocdi 1/ exp cl;
run;
