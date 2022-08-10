* ============================================================================;
* Setup;
* ============================================================================;

proc datasets lib=work nolist kill; quit; run;

proc printto log="/home/kissler/MarketScanPrescribing/logs/prepextractionlog.txt" new;
run;

proc printto print="/home/kissler/MarketScanPrescribing/logs/prepextractionout.txt" new;
run;

libname out "/home/kissler/MarketScanPrescribing/output/buildfiles/letter/";

proc import datafile = "/home/kissler/MarketScanPrescribing/output/buildfiles/letter/memb_df.csv"
	out = out.cohort (keep=ENROLID BIRTH_DATE CENSOR_DATE)
	dbms = CSV
	REPLACE;
run;

proc sort data=out.cohort;
	by ENROLID;
run;