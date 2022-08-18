* ============================================================================;
* Setup;
* ============================================================================;

proc datasets lib=work nolist kill; quit; run;

proc printto log="/home/kissler/PediatricPrescribing_Chronic/logs/olog.txt" new;
run;

proc printto print="/home/kissler/PediatricPrescribing_Chronic/logs/oout.txt" new;
run;

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

proc import datafile="/home/kissler/PediatricPrescribing_Chronic/data/vaxcodes.csv"
        out=out.vaxcodes
        dbms=csv
        replace;
run;

proc sort data=out.vaxcodes;
	by CODE;
run;

* proc print data=out.vaxcodes;
* run;

* ============================================================================;
* Extract visits and vaccinations for a given year;
* ============================================================================;

%macro getvisitvax_pre15(year=,yeartag=);

	* Import 'o' and reduce to those with valid birth date;
	data o&year. (keep=DX1 DX2 ENROLID PROC1 PROCTYP SVCDATE BIRTH_DATE CENSOR_DATE);
		merge out.cohort (in=inleft)
		dat&year..ccaeo&year.&yeartag. (in=inright keep=DX1 DX2 ENROLID PROC1 PROCTYP SVCDATE);
		by ENROLID; 
		IF inleft & inright; 
	run;

	data o&year. (keep=DX1 DX2 ENROLID CODE PROCTYP DATE ICD);
		rename SVCDATE=DATE PROC1=CODE;
		length DX1 $30. DX2 $30.;
	    set o&year.(rename=(DX1=DX1_orig DX2=DX2_orig));
	    where BIRTH_DATE<=SVCDATE<=CENSOR_DATE;
	    ICD="9";
	    DX1=DX1_orig;
	    DX2=DX2_orig;
	run;

	data visit_df&year. (keep=ENROLID DATE DX1 DX2 ICD);
		set o&year.;
	run;

	proc sort data=visit_df&year. nodupkey;
		by ENROLID DATE;
	run;

	proc sort data=o&year.;
		by CODE;
	run;

	data vax_df&year. (keep=ENROLID DATE CODE);
		merge out.vaxcodes (in=inleft)
		o&year. (in=inright keep=ENROLID CODE PROCTYP DATE where=(PROCTYP="1"));
		by CODE; 
		IF inleft & inright; 
	run;

	proc sort data=vax_df&year. nodupkey;
		by ENROLID DATE CODE;
	run;

	proc delete data=o&year.; run; 

%mend;

%macro getvisitvax_post15(year=,yeartag=);

	* Import 'o' and reduce to those with valid birth date;
	data o&year. (keep=DX1 DX2 ENROLID PROC1 PROCTYP SVCDATE DXVER BIRTH_DATE CENSOR_DATE);
		merge out.cohort (in=inleft)
		dat&year..ccaeo&year.&yeartag. (in=inright keep=DX1 DX2 ENROLID PROC1 PROCTYP SVCDATE DXVER);
		by ENROLID; 
		IF inleft & inright; 
	run;

	data o&year. (keep=DX1 DX2 ENROLID CODE PROCTYP DATE ICD);
		rename SVCDATE=DATE PROC1=CODE DXVER=ICD;
		length DX1 $30 DX2 $30;
	    set o&year.(rename=(DX1=DX1_orig DX2=DX2_orig));
	    where BIRTH_DATE<=SVCDATE<=CENSOR_DATE;
	    DX1=DX1_orig;
	    DX2=DX2_orig;
	run;

	data visit_df&year. (keep=ENROLID DATE DX1 DX2 ICD);
		set o&year.;
	run;

	proc sort data=visit_df&year. nodupkey;
		by ENROLID DATE;
	run;

	proc sort data=o&year.;
		by CODE;
	run;

	data vax_df&year. (keep=ENROLID DATE CODE);
		merge out.vaxcodes (in=inleft)
		o&year. (in=inright keep=ENROLID CODE PROCTYP DATE where=(PROCTYP="1"));
		by CODE; 
		IF inleft & inright; 
	run;

	proc sort data=vax_df&year. nodupkey;
		by ENROLID DATE CODE;
	run;

	proc delete data=o&year.; run; 

%mend;

%getvisitvax_pre15(year=08, yeartag=2)
%getvisitvax_pre15(year=09, yeartag=1)
%getvisitvax_pre15(year=10, yeartag=1)
%getvisitvax_pre15(year=11, yeartag=1)
%getvisitvax_pre15(year=12, yeartag=1)
%getvisitvax_pre15(year=13, yeartag=1)
%getvisitvax_pre15(year=14, yeartag=1)
%getvisitvax_post15(year=15, yeartag=1)
%getvisitvax_post15(year=16, yeartag=1)
%getvisitvax_post15(year=17, yeartag=1)
%getvisitvax_post15(year=18, yeartag=1)

data visit_df;
	set visit_df08 
		visit_df09 
		visit_df10 
		visit_df11 
		visit_df12 
		visit_df13 
		visit_df14 
		visit_df15 
		visit_df16 
		visit_df17 
		visit_df18;
run;

proc delete data=visit_df08; run; 
proc delete data=visit_df09; run; 
proc delete data=visit_df10; run; 
proc delete data=visit_df11; run; 
proc delete data=visit_df12; run; 
proc delete data=visit_df13; run; 
proc delete data=visit_df14; run; 
proc delete data=visit_df15; run; 
proc delete data=visit_df16; run; 
proc delete data=visit_df17; run; 
proc delete data=visit_df18; run; 

data vax_df;
	set vax_df08 
		vax_df09 
		vax_df10 
		vax_df11 
		vax_df12 
		vax_df13 
		vax_df14 
		vax_df15 
		vax_df16 
		vax_df17 
		vax_df18;
run;

proc delete data=vax_df08; run; 
proc delete data=vax_df09; run; 
proc delete data=vax_df10; run; 
proc delete data=vax_df11; run; 
proc delete data=vax_df12; run; 
proc delete data=vax_df13; run; 
proc delete data=vax_df14; run; 
proc delete data=vax_df15; run; 
proc delete data=vax_df16; run; 
proc delete data=vax_df17; run; 
proc delete data=vax_df18; run; 

proc export data=visit_df
	outfile='/home/kissler/PediatricPrescribing_Chronic/output/buildfiles/visit_df.csv'
	dbms=csv
	replace;
run;

* proc delete data=visit_df; * run; 

proc export data=vax_df
	outfile='/home/kissler/PediatricPrescribing_Chronic/output/buildfiles/vax_df.csv'
	dbms=csv
	replace;
run;

* proc delete data=vax_df; * run; 

* proc print data=out.d (obs=20);
* run;
