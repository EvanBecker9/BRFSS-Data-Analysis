* Evan Becker SAS project


HEALTH CONDITION: 
Label: Ever Told Had Asthma
Section Name: Chronic Health Conditions
Core Section Number: 7
Question Number: 4
Column: 121
Type of Variable: Num
SAS Variable Name: ASTHMA3
Question Prologue:
Question:  (Ever told) (you had) asthma?

RISK FACTOR:
Label: Smoked at Least 100 Cigarettes
Section Name: Tobacco Use
Core Section Number: 12
Question Number: 1
Column: 223
Type of Variable: Num
SAS Variable Name: SMOKE100
Question Prologue:
Question:  Have you smoked at least 100 cigarettes in your entire life?   [Note:  5 packs = 100 cigarettes]


;

LIBNAME proj "C:\Users\Evan\Desktop\SAS CLASS\project sas";


/*
Code to get counts of both states table:

PROC FREQ DATA = proj.sasdata;
TABLE _STATE*SMOKE100;
RUN;
*/

PROC FORMAT;
	*sexvar;
	VALUE sexvarf 
	1 = "Male"
	2 = "Female"
	. = "Missing"
	;
	*race;
	VALUE _RACEGR4f
	1 = "White and Non-Hispanic"
	2 = "Black and Non-Hispanic"
	3-4 = "Other and Non-Hispanic"
	5 = "Hispanic"
	. = "Missing"
	;
	*binge drinking status;
	VALUE _rfbing6f
	1 = "No"
	2 = "Yes"
	. = "Don't know/Refused/Missing"
	;

	VALUE INCOME3f
	1-4 = "Less than $25,000"
	5-6 = "$25,000 to <$50,000"
	7-11 = "$50,000 and greater"
	. = "Missing"
	;
	*age;
	VALUE _ageg5yrf
	1-2 = "18-29 years"
	3-6 = "30-49 years"
	7-10 = "50-69 years"
	11-13 = "70 years and older"
	. = "Missing"
	;
	*_bmi5cat;
	VALUE _bmi5catf
	1 = "<18.5 kg/m2"
	2 = "18.5 to <25 kg/m2"
	3 = "25 to <30 kg/m2"
	4 = ">=30 kg/m2"
	. = "Missing"
	;

	*7 and 9 need to be mapped to .;
	VALUE ASTHMA3f
	1 = "Diagnosed with Asthma"
	2 = "Not Diagnosed with Asthma"
	. = "Missing"
	;

	*7 and 9 need to me mapped to .;
	VALUE SMOKE100f
	1 = "100+ cigarettes"
	2 = "<100 cigarettes"
	. = "Missing"
	;

	VALUE _statef
	4 = "Arizona/(n=10185)"
	40 = "Oklahoma/(n=5775)"
	;
RUN;




*accounting for don't knows or not sures;
DATA BRFSS;
SET proj.SASDATA;
	IF _AGEG5YR = 14 THEN _AGEG5YR = .;
	IF _RACEGR4 = 9 THEN _RACEGR4 = .;
	IF INCOME3 IN (77 99) THEN INCOME3 = .;
	IF _RFBING6 = 9 THEN _RFBING6 = .;
	IF SMOKE100 IN (7 9) THEN SMOKE100 = .;
	IF ASTHMA3 IN (7 9) THEN ASTHMA3 = .;

	BMI = _BMI5 / 100;

KEEP _STATE SEXVAR _AGEG5YR _RACEGR4 INCOME3 _RFBING6 _BMI5 _BMI5CAT SMOKE100 ASTHMA3 BMI;

RUN;

/* 
code to get counts of asthma status for table:

PROC FREQ DATA = BRFSS;
TABLE ASTHMA3*SMOKE100;
FORMAT SMOKE100 SMOKE100f. ASTHMA3 ASTHMA3f.;
RUN;   */







*******************1.) summarizing/comparing variables by state***************************;

%MACRO CATTABLE (VARIABLE, FORMAT, FACTOR);
proc freq data=BRFSS noprint;
table _state*&VARIABLE / chisq sparse outpct out=freqs;
output out=chis chisq;
format &VARIABLE &FORMAT;
run;

data freqs;
length factor $100 response $50;
set freqs;
factor=&FACTOR;
response=put(&VARIABLE, &FORMAT);
if &VARIABLE=. then sort=99; 
drop &VARIABLE;
run;

proc sort data=freqs;
by sort;
run;

data freqs;
merge freqs
      chis (keep=p_pchi);
run;

proc append data=freqs base=factors_bystate force;
run;
%MEND;
%CATTABLE(sexvar, sexvarf., "Sex of Respondent");
%CATTABLE(_ageg5yr, _ageg5yrf., "Age (years)");
%CATTABLE(INCOME3, INCOME3f., "Annual Income");
%CATTABLE(_RACEGR4, _RACEGR4f., "Race/Ethnicity of Respondent");
%CATTABLE(_bmi5cat, _bmi5catf., "BMI Category");
%CATTABLE(_rfbing6, _rfbing6f., "Binge Drinking Status");
%CATTABLE(SMOKE100, SMOKE100f., "100 Cigarettes Status");
%CATTABLE(ASTHMA3, ASTHMA3f., "Asthma");

*sort to get no responses on the bottom;
PROC SORT DATA = factors_bystate;
BY SORT;
RUN;

*code for the table;
title "Count and Percentage of factors among state";
proc report data=factors_bystate missing;
column ("Factor" factor response) _state,(count pct_row) p_pchi; 
define factor   / group " " order=data width=10 ; 
define response / group " "  order=data width=10 RIGHT; 
define _state   / across " " format=_statef.; 
define count    / analysis "Count" ;
define pct_row  / analysis "Percent" format=5.0; 
define p_pchi   / analysis "P-value" format=pvalue6.4;
run;


*delete dataset to not append multiple times;

/*proc datasets;
delete factors_bystate;
quit;*/


*Continous BMI variable comaprison by state;
**** BMI ****;
PROC TTEST DATA=BRFSS;
	CLASS _state;
	VAR BMI;
	ODS OUTPUT ttests=ttest;
RUN;

*unequal variance (p=.0001);
PROC MEANS DATA=BRFSS NOPRINT NWAY; 
	CLASS _state;
	VAR BMI;
	OUTPUT OUT=stats
  	MEDIAN=median
   	P25=p25
   	P75=p75
   	MEAN=mean
   	STD=std ;   
RUN;



DATA stats;
	LENGTH factor $100;
	MERGE stats
          ttest (where=(variances="Unequal")); 
	factor="Body Mass Index (kg/m2)";                
RUN;



PROC APPEND DATA=stats BASE=factors_continuous_bystate FORCE;
RUN;

*table code;
PROC REPORT DATA=factors_continuous_bystate;
COLUMN factor _state,(median p25 p75 mean std) probt;
DEFINE factor / GROUP "Factor";
DEFINE _state  / ACROSS " " FORMAT=_statef.;
DEFINE median / ANALYSIS "Median" FORMAT=5.2;
DEFINE p25    / ANALYSIS "P25" FORMAT=5.2;
DEFINE p75    / ANALYSIS "P75" FORMAT=5.2;
DEFINE mean   / ANALYSIS "Mean" FORMAT=5.2;
DEFINE std    / ANALYSIS "STD" FORMAT=5.2;
DEFINE probt  / ANALYSIS "P-value/(t-test)" FORMAT=pvalue6.4;
RUN;

/*proc datasets;
delete factors_continuous_bystate;
quit;*/






*same comparisons but by asthma status in Arizona;
DATA ARIZONA;
SET BRFSS;
IF _STATE = 4;
IF ASTHMA3 NOT IN(.);
RUN;

%MACRO CATTABLE (VARIABLE, FORMAT, FACTOR);
proc freq data=ARIZONA noprint;
table ASTHMA3*&VARIABLE / chisq sparse outpct out=freqs2;
output out=chis2 chisq;
format &VARIABLE &FORMAT;
run;

data freqs2;
length factor $100 response $50;
set freqs2;
factor=&FACTOR;
response=put(&VARIABLE, &FORMAT);
if &VARIABLE=. then sort=99; 
drop &VARIABLE;
run;

proc sort data=freqs2;
by sort;
run;

data freqs2;
merge freqs2
      chis2 (keep=p_pchi);
run;

proc append data=freqs2 base=factors_byasthma force;
run;
%MEND;
%CATTABLE(sexvar, sexvarf., "Sex of Respondent");
%CATTABLE(_ageg5yr, _ageg5yrf., "Age (years)");
%CATTABLE(INCOME3, INCOME3f., "Annual Income");
%CATTABLE(_RACEGR4, _RACEGR4f., "Race/Ethnicity of Respondent");
%CATTABLE(_bmi5cat, _bmi5catf., "BMI Category");
%CATTABLE(_rfbing6, _rfbing6f., "Binge Drinking Status");
%CATTABLE(SMOKE100, SMOKE100f., "Smoking Status");


PROC SORT DATA = factors_byasthma;
BY SORT;
RUN;

*code for the table;
title "Count and Percentage of factors among asthma response";
proc report data=factors_byasthma missing;
column ("Factor" factor response) ASTHMA3,(count pct_row) p_pchi; 
define factor   / group " " order=data width=10 ; 
define response / group " "  order=data width=10 RIGHT; 
define ASTHMA3   / across " " format=ASTHMA3f.; 
define count    / analysis "Count" ;
define pct_row  / analysis "Percent" format=5.0; 
define p_pchi   / analysis "P-value" format=pvalue6.4;
run;


/*proc datasets;
delete factors_byasthma;
quit;*/





*Continous BMI variable comaprison by asthma status;
**** BMI ****;
PROC TTEST DATA=ARIZONA;
*1.;CLASS ASTHMA3;
	VAR BMI;
*2.;ODS OUTPUT ttests=ttest;
RUN;

*3. unequal variance (p=.0001);
*4.;PROC MEANS DATA=ARIZONA NOPRINT NWAY; 
*5.;CLASS ASTHMA3;
*6.;VAR BMI;
*7.;OUTPUT OUT=stats2
   MEDIAN=median
   P25=p25
   P75=p75
   MEAN=mean
   STD=std ;   
RUN;



DATA stats2;
*8.;LENGTH factor $100;
*9.;MERGE stats2
          ttest (where=(variances="Unequal")); 
*10.;factor="Body Mass Index (kg/m2)";                
RUN;



*11.;PROC APPEND DATA=stats2 BASE=factors_continuous_byasthma FORCE;
RUN;

*table code;
PROC REPORT DATA=factors_continuous_byasthma;
COLUMN factor ASTHMA3,(median p25 p75 mean std) probt;
DEFINE factor / GROUP "Factor";
DEFINE ASTHMA3  / ACROSS " " FORMAT=ASTHMA3f.;
DEFINE median / ANALYSIS "Median" FORMAT=5.2;
DEFINE p25    / ANALYSIS "P25" FORMAT=5.2;
DEFINE p75    / ANALYSIS "P75" FORMAT=5.2;
DEFINE mean   / ANALYSIS "Mean" FORMAT=5.2;
DEFINE std    / ANALYSIS "STD" FORMAT=5.2;
DEFINE probt  / ANALYSIS "P-value/(t-test)" FORMAT=pvalue6.4;
RUN;


/*proc datasets;
delete factors_continuous_byasthma;
quit;*/










******************bar charts********************


percent of people who had asthma by state group;
PROC FREQ DATA = BRFSS NOPRINT;
TABLES _STATE*ASTHMA3/ NOCOL NOPERCENT out = state_data OUTPCT;
FORMAT _STATE _STATEf. ASTHMA3 ASTHMA3f.;
RUN;
DATA state_data;
SET state_data;
ROW = round(PCT_ROW);
RUN;
TITLE "Figure 1: Prevelance of respondents reporting asthma diagnosis by state of residence, 2022 Behavioral Risk Factor Surveillance System (BRFSS), Survey From Arizona and Oklahoma";
PROC SGPLOT DATA = state_data NOAUTOLEGEND;
VBAR _STATE / GROUP=ASTHMA3 RESPONSE=ROW DATALABEL DATALABELATTRS=(SIZE=10) FILLATTRS=(color=salmon);
	WHERE ASTHMA3 = 1;
	YAXIS LABEL = "Prevelance (%) of Respondents Who Have Had Asthma" min = 0 max = 100;
FORMAT _STATE _STATEf.;
RUN;









*for percent of people who had asthma by smoke group;
PROC FREQ DATA = ARIZONA NOPRINT;
TABLES SMOKE100*ASTHMA3/ NOCOL NOPERCENT out = prevelance_data OUTPCT;
FORMAT SMOKE100 SMOKE100f. ASTHMA3 ASTHMA3f.;
RUN;
DATA prevelance_data;
SET prevelance_data;
ROW = round(PCT_ROW);
RUN;

TITLE "Figure 2: Prevelance of respondents reporting asthma diagnosis by smoking group, 2022 Behavioral Risk Factor Surveillance System (BRFSS), Survey From Arizona";
PROC SGPLOT DATA = prevelance_data NOAUTOLEGEND;
VBAR SMOKE100 / GROUP=ASTHMA3 RESPONSE=ROW DATALABEL DATALABELATTRS=(SIZE=10) FILLATTRS=(color=salmon);
	WHERE ASTHMA3 = 1;
	YAXIS LABEL = "Prevelance (%) of Respondents Who Have Had Asthma" min = 0 max = 100;
FORMAT SMOKE100 SMOKE100f.;
RUN;









*percent of asthma stratified by annual income category categorized by smoke group for Arizona;



PROC FREQ DATA = ARIZONA;
	TABLE INCOME3*ASTHMA3*SMOKE100 / NOPERCENT NOROW CHISQ outpct out=data_confounding;
	FORMAT INCOME3 INCOME3f. ASTHMA3 ASTHMA3f. SMOKE100 SMOKE100f.;
RUN;
*Going to use "PCT_COL variable
Less than 25k group does not differ by smoking group for the % of asthma diagnosis (p=.1194)
25k to less than 50k group does not differ by smoking group for the % of asthma diagnosis (p=.1320)
50k+ group differs by smoking group for the % of asthma diagnosis (p=.0413)
;
TITLE "Figure 3: Prevalence of respondents reporting asthma by smoking status, stratified by
annual income, 2022 Behavioral Risk Factor Surveillance System (BRFSS) Survey from Arizona";
PROC SGPANEL DATA = data_confounding NOAUTOLEGEND;
	where SMOKE100 ne . AND ASTHMA3=1;
	STYLEATTRS DATACOLORS=(VIBG STYPK);

	PANELBY INCOME3 / NOVARNAME ONEPANEL NOBORDER COLHEADERPOS=bottom LAYOUT=columnlattice;
	VBAR SMOKE100 / RESPONE=pct_col STAT=mean GROUP=SMOKE100 GROUPDISPLAY=cluster DATALABEL;

	COLAXIS LABEL = " ";
	ROWAXIS LABEL = "Prevalence (%) of Respondents with Asthma" MIN=0 MAX=100;
	FORMAT pct_col 6.0 INCOME3 INCOME3f. SMOKE100 SMOKE100f. ASTHMA3 ASTHMA3f.;
RUN;



*ODS commented out to not make a word doc everytime code is ran
RUN CODE BELOW TO OUTPUT TABLES INTO A WORD DOC;

/*

ODS RTF FILE="C:\Users\Evan\Desktop\SAS CLASS\project sas\ BRFSS Write-up.rtf" 
   BODYTITLE STARTPAGE=NO NOGTITLE STYLE=ocean;
   OPTIONS NOCENTER NODATE NONUMBER;


	title "Table 1: Count and Percentage of factors among state";
proc report data=factors_bystate missing;
column ("Factor" factor response) _state,(count pct_row) p_pchi; 
define factor   / group " " order=data width=10 ; 
define response / group " "  order=data width=10 RIGHT; 
define _state   / across " " format=_statef.; 
define count    / analysis "Count" ;
define pct_row  / analysis "Percent" format=5.0; 
define p_pchi   / analysis "P-value" format=pvalue6.4;
run;



	TITLE "Table 2: BMI Statistics by State";
	PROC REPORT DATA=factors_continuous_bystate;
COLUMN factor _state,(median p25 p75 mean std) probt;
DEFINE factor / GROUP "Factor";
DEFINE _state  / ACROSS " " FORMAT=_statef.;
DEFINE median / ANALYSIS "Median" FORMAT=5.2;
DEFINE p25    / ANALYSIS "P25" FORMAT=5.2;
DEFINE p75    / ANALYSIS "P75" FORMAT=5.2;
DEFINE mean   / ANALYSIS "Mean" FORMAT=5.2;
DEFINE std    / ANALYSIS "STD" FORMAT=5.2;
DEFINE probt  / ANALYSIS "P-value/(t-test)" FORMAT=pvalue6.4;
RUN;


   title "Table 3: Count and Percentage of factors among asthma response";
proc report data=factors_byasthma missing;
column ("Factor" factor response) ASTHMA3,(count pct_row) p_pchi; 
define factor   / group " " order=data width=10 ; 
define response / group " "  order=data width=10 RIGHT; 
define ASTHMA3   / across " " format=ASTHMA3f.; 
define count    / analysis "Count" ;
define pct_row  / analysis "Percent" format=5.0; 
define p_pchi   / analysis "P-value" format=pvalue6.4;
run;


	TITLE "Table 4: BMI Statistics by Asthma Diagnosis";
	PROC REPORT DATA=factors_continuous_byasthma;
COLUMN factor ASTHMA3,(median p25 p75 mean std) probt;
DEFINE factor / GROUP "Factor";
DEFINE ASTHMA3  / ACROSS " " FORMAT=ASTHMA3f.;
DEFINE median / ANALYSIS "Median" FORMAT=5.2;
DEFINE p25    / ANALYSIS "P25" FORMAT=5.2;
DEFINE p75    / ANALYSIS "P75" FORMAT=5.2;
DEFINE mean   / ANALYSIS "Mean" FORMAT=5.2;
DEFINE std    / ANALYSIS "STD" FORMAT=5.2;
DEFINE probt  / ANALYSIS "P-value/(t-test)" FORMAT=pvalue6.4;
RUN;


	TITLE "Figure 1: Prevelance of respondents reporting asthma diagnosis by state of residence, 2022 Behavioral Risk Factor Surveillance System (BRFSS), Survey From Arizona and Oklahoma";
PROC SGPLOT DATA = state_data NOAUTOLEGEND;
VBAR _STATE / GROUP=ASTHMA3 RESPONSE=ROW DATALABEL DATALABELATTRS=(SIZE=10) FILLATTRS=(color=salmon);
	WHERE ASTHMA3 = 1;
	YAXIS LABEL = "Prevelance (%) of Respondents Who Have Had Asthma" min = 0 max = 100;
FORMAT _STATE _STATEf.;
RUN;



	TITLE "Figure 2: Prevelance of respondents reporting asthma diagnosis by smoking group, 2022 Behavioral Risk Factor Surveillance System (BRFSS), Survey From Arizona";
PROC SGPLOT DATA = prevelance_data NOAUTOLEGEND;
VBAR SMOKE100 / GROUP=ASTHMA3 RESPONSE=ROW DATALABEL DATALABELATTRS=(SIZE=10) FILLATTRS=(color=salmon);
	WHERE ASTHMA3 = 1;
	YAXIS LABEL = "Prevelance (%) of Respondents Who Have Had Asthma" min = 0 max = 100;
FORMAT SMOKE100 SMOKE100f.;
RUN;




   TITLE "Figure 3: Prevalence of respondents reporting asthma by smoking status, stratified by
annual income, 2022 Behavioral Risk Factor Surveillance System (BRFSS) Survey from Arizona";
PROC SGPANEL DATA = data_confounding NOAUTOLEGEND;
	where SMOKE100 ne . AND ASTHMA3=1;
	STYLEATTRS DATACOLORS=(VIBG STYPK);

	PANELBY INCOME3 / NOVARNAME ONEPANEL NOBORDER COLHEADERPOS=bottom LAYOUT=columnlattice;
	VBAR SMOKE100 / RESPONE=pct_col STAT=mean GROUP=SMOKE100 GROUPDISPLAY=cluster DATALABEL;

	COLAXIS LABEL = " ";
	ROWAXIS LABEL = "Prevalence (%) of Respondents with Asthma" MIN=0 MAX=100;
	FORMAT pct_col 6.0 INCOME3 INCOME3f. SMOKE100 SMOKE100f. ASTHMA3 ASTHMA3f.;
RUN;


ODS RTF CLOSE;



*/






