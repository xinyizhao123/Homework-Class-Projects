
 OPTIONS MPRINT SYMBOLGEN mlogic;


* COLLINEARITY DIAGNOSTICS USING THE INFORMATION MATRIX;
* Original MACRO FROM SAS-L BY MATHEW ZACK;
* Modified 26 April 2005 by Jim Singleton to handle covariates included in class statement;
* Modified November 2010 by Kristin Wall and Kevin Delaney to 
 	INCLUDE CODE FOR GENMOD, MIXED and GLIMMIX
 	  and
 	Explicitly name the output dataset containing Collinearity Diagnostics;

*The MACRO contains four named parameters:

 	COVDSN=DATASETNAME is the input dataset containing the Covariance Matrix output from 
 	the LOGISTIC, MIXED, GLIMMIX, PHREG or GENMOD procedures

 	OUTPUT=DATASETNAME is the name you choose to contain the output Collinearity Diagnostics
	
 	PROCDR=SAS procedure that produced the collinearity Matrix output. 
 	Currently, Valid values include:
 	Logistic, Phreg, Genmod, Glimmix and Mixed
 	To maintain consistency with previous versions of the MACRO, this parameter is not required for Logistic or Phreg

 	PARMINFO=Dataset generated with the Statement:
 		ods output genmod.parminfo=parms
 	That contains the names of the variables included in the model;

*IN PROC LOGISTIC OR PROC PHREG SPECIFY THE COVOUT AND THE OUTEST=DATASETNAME ;
*  OPTIONS IN THE PROC STATEMENT.  
* IF USING LOGISTIC OR PHREG Only COVDSN and OUTPUT are required;
*    %COLLIN(COVDSN=DATASETNAME, OUTPUT=DATASETNAME2);


*IF USING PROC LOGISTIC for CONDITIONAL LOGISTIC REGRESSION ie. for Matched data you need to tell SAS to 
 	Drop the intercept column from the COVOUT DATASET: ;
 	/*	proc logistic data=Data1 covout outest=test(DROP=intercept);
		strata ID;
		model outcome(event='1')=Gall hyper;
		run;
		*/
*Doing so will allow you to use the COVOUT DATASET for conditional Logistic regression like any other Logistic 
 	output:
 	 %COLLIN(COVDSN=DATASETNAME, OUTPUT=DATASETNAME2); *In the example above COVDSN=TEST;

*If using PROC SURVEYLOGISTIC add the follwing ODS OUTPUT STATEMENT to your code;
 *ods output surveylogistic.covb=DATASETNAME;  
*Also, add the /covb option to the MODEL statement, e.g. model outcome=exp covars/covb; 
*Then call the Macro as:
 	%Collin(COVDSN=DATASETNAME,PROCDR=SURVEYLOGISTIC,OUTPUT=DATASETNAME2);

* When using this Macro with GLIMMIX:
* Use the /covb option after the model statement and include the line of code:  ods output glimmix.CovB=DATASETNAME;
* Call macro as: %COLLIN(COVDSN=DATASETNAME, PROCDR=GLIMMIX, OUTPUT=);

*When using the Macro with GENMOD:  ;

* IF REPEATED IS NOT USED (UNCLUSTERED DATA -> NO GEE) THEN ;
* ADD COVB TO THE MODEL STATEMENT (MODEL / COVB) and include the following two statements immediately 
 	Before  ;
*ods output genmod.parminfo=parms;
*ods output genmod.covb=covdsn; 

* IF REPEATED IS USED FOR CLUSTERED DATA THEN ;
* ADD COVB TO THE REPEATED STATEMENT (REPEATED / COVB);
 *ods output genmod.parminfo=parms;
 *ods output genmod.geercov=covdsn; 

* When using GENMOD Call the MACRO with PROCDR=GENMOD  and PARMINFO=parms 
 		(from the ODS OUTPUT STATEMENT)
*    %COLLIN(COVDSN=COVDSN, PROCDR=GENMOD, PARMINFO=Parms, OUTPUT=DATASETNAME);

* When using this Macro with MIXED:
* Use the /covb option after the model statement and include the line of code:  ods output mixed.CovB=DATASETNAME;
* Call macro as: %COLLIN(COVDSN=DATASETNAME, PROCDR=MIXED, OUTPUT=);


%MACRO COLLIN(COVDSN=, PROCDR=, PARMINFO=,OUTPUT=);

%* MACRO TO CALCULATE COLLINEARITY DIAGNOSTICS FROM ;
%*  VARIANCE-COVARIANCE MATRIX IN NONLINEAR REGRESSION;

%* REF: DAVIS CE, HYDE JE, BANGDIWALA SI, NELSON JJ.;
%*       AN EXAMPLE OF DEPENDENCIES AMONG VARIABLES IN A;
%*       CONDITIONAL LOGISTIC REGRESSION.  IN: MOOLGAVKAR SH,;
%*       PRENTICE RL, EDS.  MODERN STATISTICAL METHODS IN;
%*       CHRONIC DISEASE EPIDEMIOLOGY.  NEW YORK:;
%*       JOHN WILEY & SONS, INC., 1986:140-7.;



%let Drop=%str();

%* MAKE GENMOD COVARIANCE OUTPUT SIMILAR ENOUGH TO LOGISTIC AND PHREG THAT THIS MACRO WILL
%* WORK.;

%IF %Upcase(&PROCDR)=GENMOD %THEN %DO;

%* FOR SOME INEXPLICABLE REASON, SAS DOES NOT RECORD THE VARIABLE NAMES IN THE OUTPUT;
%* VARIANCE-COVARIANCE DATA SET. THIS NEXT SECTION OF CODE REPLACES THE PARM VARIABLE;
%* WITH THE NAMES OF THE VARIABLES AND RENAMES PARM TO _NAME_ TO CONFORM TO THE OUTPUT;
%* DATA SETS GENERATED BY LOGISTIC AND GENMOD.;

%* IF THERE ARE MORE THAN 9 VARIABLES IN THE MODEL STATEMENT, SAS WILL STOP PROCESSING;
%* ON THE DATA NEXT_2 STEP DECLARING THE BY VARIABLE (PARM) IS NOT IN THE CORRECT SORTED;
%* ORDER. THIS DOESNT HAPPEN FOR LESS THAN NINE VARIABLES. WHEN YOU SORT THE DATA SET;
%* ON PARM, THE SORT DOES NOT TAKE PLACE AS EXPECTED, MESSING UP THE VARIANCE-COVARIANCE;
%* MATRIX. THE PROBLEM IS THAT THE VALUES OF PARM PROGRESS AS PARM1, PARM2, PARM3, ...;
%* PARM9, PARM10, ETC. WHEN YOU SORT ON PARM, PARM10, PARM11 THROUGH PARM19 SORT AFTER;
%* PARM1 AND BEFORE PARM2, DUE TO THE WAY SORTING WORKS ON CHARACTER VARIABLES. THE ONLY;
%* WAY TO FIX THIS IS TO RENAME THE VARIABLES TO PARM01, PARM02, ETC. SO THE SORTING WORKS;
%* CORRECTLY.;


DATA NEXT_1; SET &PARMINFO;
ATTRIB PARNUM FORMAT=$12.;
PARNUM=PARAMETER;
IF PARNUM = 'Prm1' THEN PARNUM = 'Prm01';
IF PARNUM = 'Prm2' THEN PARNUM = 'Prm02';
IF PARNUM = 'Prm3' THEN PARNUM = 'Prm03';
IF PARNUM = 'Prm4' THEN PARNUM = 'Prm04';
IF PARNUM = 'Prm5' THEN PARNUM = 'Prm05';
IF PARNUM = 'Prm6' THEN PARNUM = 'Prm06';
IF PARNUM = 'Prm7' THEN PARNUM = 'Prm07';
IF PARNUM = 'Prm8' THEN PARNUM = 'Prm08';
IF PARNUM = 'Prm9' THEN PARNUM = 'Prm09';

RENAME PARNUM=PARM;

RUN;
PROC SORT;
 BY PARM;
RUN;

DATA NEXT_1A; SET &COVDSN;
ATTRIB PARM FORMAT=$12.;
PARM=ROWNAME;
IF PARM = 'Prm1' THEN PARM = 'Prm01';
IF PARM = 'Prm2' THEN PARM = 'Prm02';
IF PARM = 'Prm3' THEN PARM = 'Prm03';
IF PARM = 'Prm4' THEN PARM = 'Prm04';
IF PARM = 'Prm5' THEN PARM = 'Prm05';
IF PARM = 'Prm6' THEN PARM = 'Prm06';
IF PARM = 'Prm7' THEN PARM = 'Prm07';
IF PARM = 'Prm8' THEN PARM = 'Prm08';
IF PARM = 'Prm9' THEN PARM = 'Prm09';

RUN;
PROC SORT;
 BY PARM;
RUN;

DATA NEXT_2(DROP=EFFECT); MERGE NEXT_1A(IN=IN1A) NEXT_1(IN=IN1); BY PARM; IF IN1A;
PARM=EFFECT;
RENAME PARM=_NAME_;
RUN;

   %* IN SOME OUTPUT VARIANCE-COVARIANCE MATRICES, THERE WILL BE A RECORD FOR;
   %* SCALE. DELETE THIS RECORD.;
   DATA NEXT_3; SET NEXT_2;
   IF _NAME_='SCALE' THEN DELETE;
   RUN;
   %* INSERT A DUMMY RECORD FOR ESTIMATE TO SIMULATE COVARIANCE OUTPUT FROM LOGISTIC
   %*  AND PHREG.;
   DATA NEXT_4;
   _NAME_= 'ESTIMATE';
   OUTPUT;
   RUN;
   DATA NEXT_5; SET NEXT_4 NEXT_3;

   RUN;
proc print; run;

 %END;

%* MAKE MIXED COVARIANCE OUTPUT SIMILAR ENOUGH TO LOGISTIC AND PHREG THAT THIS MACRO WILL WORK.;
%* Use the /covb option after the model statement and inlcude the line of code:  ods output CovB=dataset1;
%* Call macro as: %COLLIN(COVDSN=, PROCDR=MIXED, PARMINFO=dataset1);

%IF %upcase(&PROCDR)=MIXED %THEN %DO;
DATA NEXT_1 (Keep=_NAME_ col:); SET &COVDSN; 
	RENAME EFFECT=_NAME_;
	if Row = 1 	then  RowName = 'Prm01';
	if Row = 2 	then  RowName = 'Prm02';
	if Row = 3 	then  RowName = 'Prm03';
	if Row = 4 	then  RowName = 'Prm04';
	if Row = 5 	then  RowName = 'Prm05';
	if Row = 6 	then  RowName = 'Prm06';
	if Row = 7 	then  RowName = 'Prm07';
	if Row = 8 	then  RowName = 'Prm08';
	if Row = 9 	then  RowName = 'Prm09';		
RUN;

data next_2(Drop=covars);
set next_1;

array cols col:;
covars=dim(cols);
call symput("Numcols",left(covars));
run;


data next_2a;
attrib dummy length=$1000;
retain dummy ;
set next_2;
if sum(of Col1-COL&Numcols)=0 then do;
if dummy=" " then dummy="Drop= COL"||trim(left(put(_N_,8.)));
else if dummy ne " " then do;

dummy=trim(left(dummy))||" COL"||trim(left(put(_N_,8.))); 

end;
Call symput("Drop",dummy);
delete;
end;
run;



DATA NEXT_3; SET NEXT_2a(&drop);
   IF _NAME_='SCALE' THEN DELETE;
   RUN;

   %* INSERT A DUMMY RECORD FOR ESTIMATE TO SIMULATE COVARIANCE OUTPUT FROM LOGISTIC
   %*  AND PHREG.;
   DATA NEXT_4;			ATTRIB _NAME_ FORMAT=$12.;					
   _NAME_= 'ESTIMATE';
   OUTPUT;
   RUN;

   DATA NEXT_5; SET NEXT_4 NEXT_3; ATTRIB _NAME_ FORMAT=$12.;	
   RUN;

proc print; run;

 %END;


%* MAKE GLIMMIX COVARIANCE OUTPUT SIMILAR ENOUGH TO LOGISTIC AND PHREG THAT THIS MACRO WILL WORK.;

%IF %Upcase(&PROCDR)=GLIMMIX %THEN %DO;
DATA NEXT_1 (Keep=_NAME_ COL:); SET &COVDSN; 
	RENAME EFFECT=_NAME_;
	if Row = 1 	then  RowName = 'Prm01';
	if Row = 2 	then  RowName = 'Prm02';
	if Row = 3 	then  RowName = 'Prm03';
	if Row = 4 	then  RowName = 'Prm04';
	if Row = 5 	then  RowName = 'Prm05';
	if Row = 6 	then  RowName = 'Prm06';
	if Row = 7 	then  RowName = 'Prm07';
	if Row = 8 	then  RowName = 'Prm08';
	if Row = 9 	then  RowName = 'Prm09';		
RUN;

data next_2(Drop=covars);
set next_1;

array cols col:;
covars=dim(cols);
call symput("Numcols",left(covars));
run;


data next_2a;
attrib dummy length=$1000;
retain dummy ;
set next_2;
if sum(of Col1-COL&Numcols)=0 then do;
if dummy=" " then dummy="Drop= COL"||trim(left(put(_N_,8.)));
else if dummy ne " " then do;

dummy=trim(left(dummy))||" COL"||trim(left(put(_N_,8.))); 
end;
Call symput("Drop",dummy);
delete;
end;
run;


*Depending on the reference coding used in GENMOD and MIXED the Covariance MATRIX output by the procedure may have 
	Columns and corresponding Rows with all Zeros.
The MACRO Variable DROP (created in NEXT_2A) Isolates and removes these extraneous columns before we get to 
Manipulating the matrix in the IML code below;

DATA NEXT_3; SET NEXT_2a(&drop);
   IF _NAME_='SCALE' THEN DELETE;
   RUN;

   %* INSERT A DUMMY RECORD FOR ESTIMATE TO SIMULATE COVARIANCE OUTPUT FROM LOGISTIC
   %*  AND PHREG.;
   DATA NEXT_4;			ATTRIB _NAME_ FORMAT=$12.;					
   _NAME_= 'ESTIMATE';
   OUTPUT;
   RUN;

   DATA NEXT_5; SET NEXT_4 NEXT_3; ATTRIB _NAME_ FORMAT=$12.;	
   RUN;

proc print; run;

%END;

%* MAKE SURVEYLOGISTIC COVARIANCE OUTPUT SIMILAR ENOUGH TO LOGISTIC AND PHREG THAT THIS MACRO WILL WORK.;

%IF %Upcase(&PROCDR)=SURVEYLOGISTIC %THEN %DO;
DATA NEXT_1 ; SET &covdsn; 
	RENAME Parameter=_NAME_;
		
RUN;


DATA NEXT_3; SET NEXT_1;
   IF _NAME_='SCALE' THEN DELETE;
   RUN;

   %* INSERT A DUMMY RECORD FOR ESTIMATE TO SIMULATE COVARIANCE OUTPUT FROM LOGISTIC
   %*  AND PHREG.;
   DATA NEXT_4;	ATTRIB _NAME_ FORMAT=$32.;					
   _NAME_= 'ESTIMATE';
   OUTPUT;
   RUN;

   DATA NEXT_5; SET NEXT_4 NEXT_3; ATTRIB _NAME_ FORMAT=$32.;	
   RUN;

proc print; run;

%END;


%IF &PROCDR=%str() 
	or %upcase(&PROCDR)=LOGISTIC 
	or %upcase(&PROCDR)=PHREG 
%THEN %DO;
   DATA NEXT_5; SET &COVDSN;
   RUN;

%END; 



proc print data=next_5; run;

%IF (NEXT_5 NE ) %THEN %DO;

OPTION MPRINT;

%LET __STOP=0;

PROC IML;
  USE NEXT_5;
  READ ALL VAR {_NAME_} INTO _VARNAME;

  _NRVNAME=NROW(_VARNAME);


  IF (_NRVNAME>1) THEN DO;
     _VARNAM2=_VARNAME(|2:_NRVNAME, |);
     NMISSING=J(NROW(_VARNAM2),1,.);
     LABELS={"EIGENVAL","CONDINDX","        "};
     _VARNAM2=LABELS//_VARNAM2;
     FREE _VARNAME LABELS;
     READ ALL VAR _NUM_ INTO VARCOV(|COLNAME=_NVNAME|);
     _NRCVC=NCOL(VARCOV);
     LASTVNAM=_NVNAME(|1,_NRCVC|);
     IF (LASTVNAM="_LNLIKE_") THEN VARCOV2=VARCOV(|2:_NRVNAME,1:_NRCVC-1|);
     IF (LASTVNAM^="_LNLIKE_") THEN VARCOV2=VARCOV(|2:_NRVNAME,|);

%* IF COVARIANCE MATRIX IS FROM PROC GENMOD USING THE REPEATED MEASURES DESIGN;
%* THEN THE LOWER DIAGONAL WILL HAVE THE CORRELATIONS AND THE UPPER DIAGONAL WILL HAVE;
%* THE COVARIANCES. THIS NEXT SECTION OF CODE REPLACES THE LOWER DIAGONAL WITH THE UPPER;
%* DIAGONAL TO MAKE A SYMMETRIC COVARIANCE MATRIX. IF THE MATRIX IS SYMMETRICAL ALREADY;
%* THEN THE NEXT SECTION OF CODE WILL NOT AFFECT ANYTHING.;


        VC2_C = NCOL(VARCOV2);
        VC2_R = NROW(VARCOV2);
        DO CL=1 TO VC2_C;
           DO RW=1 TO VC2_R;
              VARCOV2(|RW,CL|) = VARCOV2(|CL,RW|);
           END;
        END;

%* PRINT THE VARIANCE-COVARIANCE MATRIX FOR DIAGNOSTIC PURPOSES;
      PRINT VARCOV2;

     FREE VARCOV _NRCVC LASTVNAM VC2_C VC2_R CL;
     COVBINV=INV(VARCOV2);
     SCALE=INV(SQRT(DIAG(COVBINV)));
     R=SCALE*COVBINV*SCALE;
     FREE COVBINV SCALE;
     CALL EIGEN(MUSQR,V,R);
     FREE R;
     SROOTMUS=SQRT(MUSQR);
     CI=1/(SROOTMUS/MAX(SROOTMUS));
     PHI=(V##2)*DIAG(MUSQR##(-1));
     SUMPHI=PHI(|,+|);
     PI=PHI#(SUMPHI##(-1));
     FREE PHI SUMPHI SROOTMUS V;
     FINAL=(MUSQR||CI||NMISSING||PI`)`;
     FREE PI MUSQR CI NMISSING;
     _NCFINAL=NCOL(FINAL);
     _NRFINAL=NROW(FINAL);
     FINAL2=J(_NRFINAL,_NCFINAL,0);
     _NCFP1=_NCFINAL+1;
     __VDP="VDP";
     DO I=1 TO _NCFINAL;
        FINAL2(|,_NCFP1-I|)=FINAL(|,I|);
        X=CHAR(I,3);
        Y=COMPRESS(CONCAT(__VDP,X));
        IF I=1 THEN _VDPNAME=Y;
           ELSE _VDPNAME=_VDPNAME||Y;
     END;
     FREE FINAL _NRFINAL _NCFINAL I X Y;
     CREATE &output FROM FINAL2(|ROWNAME=_VARNAM2 COLNAME=_VDPNAME|);
     APPEND FROM FINAL2(|ROWNAME=_VARNAM2|);
     FREE _VARNAM2 _VDPNAME FINAL2;
  END;
  IF (_NRVNAME=1) THEN DO;
     X="1";
     CALL SYMPUT("__STOP",LEFT(X));
     PRINT " ";
     PRINT "**********************************************************";
     PRINT "YOU NEED TO SPECIFY THE  COVOUT  OPTION";
     PRINT " IN EITHER PROC LOGISTIC OR PROC PHREG.";
     PRINT " THIS PROGRAM WILL NOT CALCULATE COLLINEARITY DIAGNOSTICS.";
     PRINT "**********************************************************";
     PRINT " ";
  END;
  QUIT;
RUN;

%IF (&__STOP EQ 0) %THEN %DO;
   PROC PRINT DATA=&output LABEL NOOBS;
     ID _VARNAM2;
	 Title7 "Input DATASET &COVDSN, Submitted &sysdate9";
     TITLE8 "COLLINEARITY DIAGNOSTICS FOR NONLINEAR MODELS USING";
     TITLE9 "THE INFORMATION MATRIX:  EIGENVALUES, CONDITION INDEXES,";
     TITLE10 "AND VARIANCE DECOMPOSITION PROPORTIONS (VDP'S)";
	 
     LABEL _VARNAM2="VARIABLE";
   RUN;
%END;

%END;
%ELSE %DO;
   %PUT;
   %PUT "*******************************************************";
   %PUT "WHEN YOU INVOKE THIS MACRO, YOU HAVE TO SPECIFY THE NAME";
   %PUT " OF A SAS DATA SET THAT CONTAINS THE VARIANCE-COVARIANCE";
   %PUT " MATRIX FROM EITHER PROC LOGISTIC OR PROC PHREG.";
   %PUT;
   %PUT "YOU CAN CREATE THIS MATRIX BY INCLUDING THE FOLLOWING OPTIONS";
   %PUT " ON THE PROC STATEMENT:  COVOUT  AND  OUTEST=SASDSN,";
   %PUT " WHERE SASDSN IS THE NAME OF THE SAS DATA SET CONTAINING";
   %PUT " THE VARIANCE-COVARIANCE MATRIX.";
   %PUT "*******************************************************";
   %PUT;
%END;

PROC DATASETS;
DELETE NEXT_1 NEXT_1A NEXT_2 Next_2a NEXT_3 NEXT_4 NEXT_5;
RUN;
QUIT;

title;

%MEND COLLIN;
