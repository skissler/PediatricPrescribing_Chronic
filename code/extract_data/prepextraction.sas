* ============================================================================;
* Setup;
* ============================================================================;

proc datasets lib=work nolist kill; quit; run;

proc printto log="/home/kissler/PediatricPrescribing_Chronic/logs/prepextractionlog.txt" new;
run;

proc printto print="/home/kissler/PediatricPrescribing_Chronic/logs/prepextractionout.txt" new;
run;

libname out "/home/kissler/PediatricPrescribing_Chronic/output/buildfiles/";

proc import datafile = "/home/kissler/PediatricPrescribing_Chronic/output/buildfiles/memb_df.csv"
	out = out.cohort (keep=ENROLID BIRTH_DATE CENSOR_DATE)
	dbms = CSV
	REPLACE;
run;

proc sort data=out.cohort;
	by ENROLID;
run;