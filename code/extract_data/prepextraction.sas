* ============================================================================;
* Setup;
* ============================================================================;

proc datasets lib=work nolist kill; quit; run;

proc printto log="/home/kissler/PediatricPrescribing_Chronic/logs/prepextractionlog.txt" new;
run;

proc printto print="/home/kissler/PediatricPrescribing_Chronic/logs/prepextractionout.txt" new;
run;

* Specify data and output libraries;
libname out "/home/kissler/PediatricPrescribing_Chronic/output/buildfiles/";
libname dat08 "/data/markscan_authorized/data/commercial/2008";
libname dat09 "/data/markscan_authorized/data/commercial/2009";
libname dat10 "/data/markscan_authorized/data/commercial/2010";
libname dat11 "/data/markscan_authorized/data/commercial/2011";
libname dat12 "/data/markscan_authorized/data/commercial/2012";
libname dat13 "/data/markscan_authorized/data/commercial/2013";
libname dat14 "/data/markscan_authorized/data/commercial/2014";
libname dat15 "/data/markscan_authorized/data/commercial/2015";
libname dat16 "/data/markscan_authorized/data/commercial/2016";
libname dat17 "/data/markscan_authorized/data/commercial/2017";
libname dat18 "/data/markscan_authorized/data/commercial/2018";

proc import datafile = "/home/kissler/PediatricPrescribing_Chronic/output/buildfiles/memb_df.csv"
	out = out.cohort (keep=ENROLID BIRTH_DATE CENSOR_DATE)
	dbms = CSV
	REPLACE;
run;

proc sort data=out.cohort;
	by ENROLID;
run;