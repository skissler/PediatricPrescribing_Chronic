* ============================================================================;
* Setup;
* ============================================================================;

proc datasets lib=work nolist kill; quit; run;

proc printto log="/home/kissler/MarketScanPrescribing/logs/dlog.txt" new;
run;

proc printto print="/home/kissler/MarketScanPrescribing/logs/dout.txt" new;
run;

libname out "/home/kissler/MarketScanPrescribing/output/buildfiles/letter/";
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

* ============================================================================;
* Extract prescriptions for a given year;
* ============================================================================;

%macro getrx(year=,yeartag=);

	* Import 'd' and reduce to those with valid birth date;
	data d&year. (keep=ENROLID NDCNUM REFILL SVCDATE BIRTH_DATE CENSOR_DATE);
		merge out.cohort (in=inleft)
		dat&year..ccaed&year.&yeartag. (in=inright keep=ENROLID NDCNUM REFILL SVCDATE); *  where=(REFILL=0);
		by ENROLID; 
		IF inleft & inright; 
	run;

	data d&year. (keep=ENROLID DATE CODE REFILL);
		rename SVCDATE=DATE NDCNUM=CODE;
	    set d&year.;
	    where BIRTH_DATE<=SVCDATE<=CENSOR_DATE;
	run;

	proc sort data=d&year. nodupkey;
		by ENROLID DATE CODE;
	run;

%mend;

%getrx(year=08, yeartag=2)
%getrx(year=09, yeartag=1)
%getrx(year=10, yeartag=1)
%getrx(year=11, yeartag=1)
%getrx(year=12, yeartag=1)
%getrx(year=13, yeartag=1)
%getrx(year=14, yeartag=1)
%getrx(year=15, yeartag=1)
%getrx(year=16, yeartag=1)
%getrx(year=17, yeartag=1)
%getrx(year=18, yeartag=1)

data d;
	set d08 
		d09 
		d10 
		d11 
		d12 
		d13 
		d14 
		d15 
		d16 
		d17 
		d18;
run;

proc delete data=d08; run; 
proc delete data=d09; run; 
proc delete data=d10; run; 
proc delete data=d11; run; 
proc delete data=d12; run; 
proc delete data=d13; run; 
proc delete data=d14; run; 
proc delete data=d15; run; 
proc delete data=d16; run; 
proc delete data=d17; run; 
proc delete data=d18; run; 

proc export data=d
	outfile='/home/kissler/MarketScanPrescribing/output/buildfiles/letter/rx_df.csv'  
	dbms=csv
	replace;
run;

* proc delete data=d; * run; 

* proc print data=d (obs=20);
* run;
