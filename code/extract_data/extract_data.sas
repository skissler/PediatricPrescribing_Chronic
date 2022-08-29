* ============================================================================;
* Setup;
* ============================================================================;

* Make sure there's nothing in the 'work' library;
proc datasets lib=work nolist kill; quit; run;

* Set path for log; 
proc printto log="/home/kissler/PediatricPrescribing_Chronic/logs/extract_data_log.txt" new;
run;

* Set path for printing output;
proc printto print="/home/kissler/PediatricPrescribing_Chronic/logs/extract_data_out.txt" new;
run;

* Set path for saving output;
libname out "/home/kissler/PediatricPrescribing_Chronic/output/buildfiles/";

* Specify data libraries;
libname dathome "/data/markscan_authorized/data";
libname dat08 "/data/markscan_authorized_users/kissler/dat08";
libname dat09 "/data/markscan_authorized_users/kissler/dat09";
libname dat10 "/data/markscan_authorized/data/commercial/2010";
libname dat11 "/data/markscan_authorized/data/commercial/2011";
libname dat12 "/data/markscan_authorized/data/commercial/2012";
libname dat13 "/data/markscan_authorized/data/commercial/2013";
libname dat14 "/data/markscan_authorized/data/commercial/2014";
libname dat15 "/data/markscan_authorized/data/commercial/2015";
libname dat16 "/data/markscan_authorized/data/commercial/2016";
libname dat17 "/data/markscan_authorized/data/commercial/2017";
libname dat18 "/data/markscan_authorized/data/commercial/2018";


* Import and process table of days per month --------------------------------; 
proc import datafile="/home/kissler/PediatricPrescribing_Chronic/data/dayspermonth.csv"
        out=dayspermonth
        dbms=csv
        replace;
run;

proc sort data=dayspermonth;
	by DT_MONTH;
run;

* Import and process list of NDC codes to extract -----------------------------; 
proc import datafile="/home/kissler/PediatricPrescribing_Chronic/data/ndc_to_extract.csv"
        out=ndctoextract
        dbms=csv
        replace;
run;

proc sort data=ndctoextract;
	by NDCNUM;
run;

* Import and process table of US states --------------------------------------;
proc import datafile="/home/kissler/PediatricPrescribing_Chronic/data/EGEOLOClist_char.csv"
        out=EGEOLOClist
        dbms=csv
        replace;
run;

* Make sure variable lengths are sufficient to avoid truncation:;
data EGEOLOClist;
	length STATE $30;
    set EGEOLOClist(rename=(STATE=STATE_orig));
    STATE=STATE_orig;
run;

proc sort data=EGEOLOClist;
	by EGEOLOC;
run;

* ============================================================================;
* Define extraction/reduction scripts;
* ============================================================================;

* Extract birth dates;
%macro getbirthdates(year=,yeartag=);

	* Initial import of inpatient services table;
	data cohortBirthdates&year. (keep=ENROLID SVCDATE);
		set dat&year..ccaes&year.&yeartag. (keep=AGE DX1 DX2 ENROLID SVCDATE where=(AGE=0 and (substr(DX1,1,3)="V30" or 
				substr(DX1,1,3)="V31" or 
				substr(DX1,1,3)="V32" or 
				substr(DX1,1,3)="V33" or 
				substr(DX1,1,3)="V34" or 
				substr(DX1,1,3)="V35" or 
				substr(DX1,1,3)="V36" or 
				substr(DX1,1,3)="V37" or 
				substr(DX1,1,3)="Z38" or 
				substr(DX2,1,3)="V30" or 
				substr(DX2,1,3)="V31" or 
				substr(DX2,1,3)="V32" or 
				substr(DX2,1,3)="V33" or 
				substr(DX2,1,3)="V34" or 
				substr(DX2,1,3)="V35" or 
				substr(DX2,1,3)="V36" or 
				substr(DX2,1,3)="V37" or 
				substr(DX2,1,3)="Z38")));
	run;

	* Sort by visit date;
	proc sort data=cohortBirthdates&year.;
		by ENROLID SVCDATE;
	run;

	* For each person, pull out the earliest date (the birth date);
	data cohortBirthdates&year. (keep=ENROLID BIRTH_DATE);
		set cohortBirthdates&year. (rename=(SVCDATE=BIRTH_DATE));
		by ENROLID;
		if first.ENROLID;
	run;

%mend;

* Extract individuals with prescriptions under the age of 5; 
%macro getcohort(year=,yeartag=);

	* Initial import, ensuring we have RX data and age <= 5;
	data cohort&year. (keep=DT_MONTH DT_YEAR DTEND EGEOLOC MSA ENROLID MEMDAYS SEX);
		set dat&year..ccaet&year.&yeartag. (keep=AGE RX DTEND EGEOLOC MSA ENROLID MEMDAYS SEX where=(RX="1" and AGE<=5));
		DT_MONTH=month(DTEND);
		DT_YEAR=year(DTEND);
	run;

	* Restrict to valid states;
	proc sort data=cohort&year.;
		by EGEOLOC;
	run;

	data cohort&year. (keep=DT_MONTH DT_YEAR DTEND STATE MSA ENROLID MEMDAYS SEX BIRTH_DATE);
		merge EGEOLOClist (in=inleft)
		cohort&year. (in=inright);
		by EGEOLOC; 
		IF inleft & inright; 
	run;

	* Restrict to those with a birth date;
	proc sort data=cohort&year.;
		by ENROLID;
	run;

	data cohort&year. (keep=DT_MONTH DT_YEAR DTEND STATE MSA ENROLID MEMDAYS SEX BIRTH_DATE);
		merge cohortBirthdates (in=inleft)
		cohort&year. (in=inright);
		by ENROLID; 
		IF inleft & inright; 
	run;

	* Keep only rows corresponding to full months or the birth month;
	proc sort data=cohort&year.;
		by DT_MONTH;
	run;

	data cohort&year. (keep=DT_MONTH DT_YEAR DTEND STATE MSA ENROLID MEMDAYS SEX BIRTH_DATE NDAYS where=((MEMDAYS>=NDAYS) or (month(BIRTH_DATE)=DT_MONTH and year(BIRTH_DATE)=DT_YEAR)));
		merge dayspermonth (in=inleft)
		cohort&year. (in=inright);
		by DT_MONTH;
		IF inleft & inright;
	run;

	* Keep only relevant columns;
	data cohort&year. (keep=DT_MONTH DT_YEAR DTEND STATE MSA ENROLID SEX BIRTH_DATE);
		set cohort&year.;
	run;

%mend;

* Reduce to individuals who are present for five straight years;
%macro refinecohort();

	* Sort the cohort table;
	proc sort data=cohort;
		by ENROLID DT_YEAR DT_MONTH;
	run;

	* Count months from birth;
	data cohort (keep=DT_MONTH DT_YEAR DTEND STATE MSA ENROLID SEX BIRTH_DATE COUNT);
		set cohort;
		COUNT + 1;
		by ENROLID;
		if first.ENROLID then COUNT = 1;
	run;

	* Append an index column;
	data cohort (keep=DT_MONTH DT_YEAR DTEND STATE MSA ENROLID SEX BIRTH_DATE COUNT BIRTHDIFF);
		set cohort;
		BIRTHDIFF=12*(DT_YEAR-year(BIRTH_DATE))+(DT_MONTH-month(BIRTH_DATE))+1;
	run;

	* keep only rows where index = months from birth, which gives contiguous months from birth;
	data cohort (keep=DTEND STATE MSA ENROLID SEX BIRTH_DATE COUNT BIRTHDIFF where=(COUNT=BIRTHDIFF));
		set cohort;
	run;

	* keep only one row per person (the last);
	proc sort data=cohort;
		by ENROLID COUNT;
	run;

	data cohort (keep=DTEND STATE MSA ENROLID SEX BIRTH_DATE);
		set cohort;
		by ENROLID;
		if last.ENROLID;
	run;

	* Calculate how long each person is followed;
	data cohort (keep=DTEND STATE MSA ENROLID SEX BIRTH_DATE DURATION);
		set cohort;
		DURATION=DTEND-BIRTH_DATE;
	run;

	* Censor people after 5 years (1825 days);
	proc sort data=cohort;
		by DURATION;
	run;

	data cohort (keep=STATE MSA ENROLID SEX BIRTH_DATE DURATION);
		set cohort;
		by DURATION;
		if DURATION>=1825;
	run;

	data cohort (keep=STATE MSA ENROLID SEX BIRTH_DATE CENSOR_DATE);
		set cohort;
		CENSOR_DATE=BIRTH_DATE+1825;
		format CENSOR_DATE mmddyys10.;
	run;

%mend;

* Get prescription data;
%macro getrx(year=,yeartag=);

	proc sort data=cohort;
		by ENROLID;
	run;

	* Import 'd' and reduce to those with valid birth date;
	data d&year. (keep=ENROLID NDCNUM REFILL SVCDATE BIRTH_DATE CENSOR_DATE);
		merge cohort (in=inleft)
		dat&year..ccaed&year.&yeartag. (in=inright keep=ENROLID NDCNUM REFILL SVCDATE); *  where=(REFILL=0);
		by ENROLID; 
		IF inleft & inright; 
	run;

	* Restrict to antibiotic prescriptions;
	proc sort data=d&year.;
		by NDCNUM;
	run;

	data d&year. (keep=ENROLID NDCNUM REFILL SVCDATE BIRTH_DATE CENSOR_DATE);
		merge ndctoextract (keep=NDCNUM in=inleft)
		d&year. (in=inright);
		by NDCNUM; 
		IF inleft & inright; 
	run;

	* Keep only records between the birth date and censor date;
	data d&year. (keep=ENROLID DATE CODE REFILL);
		rename SVCDATE=DATE NDCNUM=CODE;
	    set d&year.;
	    where BIRTH_DATE<=SVCDATE<=CENSOR_DATE;
	run;

	* Get unique records by person, date, and NDC;
	proc sort data=d&year. nodupkey;
		by ENROLID DATE CODE;
	run;

%mend;

* Get visit data;
%macro getvisits_pre15(year=,yeartag=);

	proc sort data=cohort;
		by ENROLID;
	run;

	* Import 'o' and reduce to those with valid birth date;
	data o&year. (keep=DX1 DX2 ENROLID SVCDATE BIRTH_DATE CENSOR_DATE);
		merge cohort (in=inleft)
		dat&year..ccaeo&year.&yeartag. (in=inright keep=DX1 DX2 ENROLID SVCDATE);
		by ENROLID; 
		IF inleft & inright; 
	run;

	* Keep only visits between birth and censor date;
	data o&year. (keep=DX1 DX2 ENROLID DATE ICD);
		rename SVCDATE=DATE;
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

	proc delete data=o&year.; run; 

%mend;

%macro getvisits_post15(year=,yeartag=);

	proc sort data=cohort;
		by ENROLID;
	run;

	* Import 'o' and reduce to those with valid birth date;
	data o&year. (keep=DX1 DX2 ENROLID SVCDATE DXVER BIRTH_DATE CENSOR_DATE);
		merge cohort (in=inleft)
		dat&year..ccaeo&year.&yeartag. (in=inright keep=DX1 DX2 ENROLID SVCDATE DXVER);
		by ENROLID; 
		IF inleft & inright; 
	run;

	* Keep only visits between birth and censor date;
	data o&year. (keep=DX1 DX2 ENROLID DATE ICD);
		rename SVCDATE=DATE DXVER=ICD;
		length DX1 $30 DX2 $30;
	    set o&year.(rename=(DX1=DX1_orig DX2=DX2_orig));
	    where BIRTH_DATE<=SVCDATE<=CENSOR_DATE;
	    DX1=DX1_orig;
	    DX2=DX2_orig;
	run;

	data visit_df&year. (keep=ENROLID DATE DX1 DX2 ICD);
		set o&year.;
	run;

	* Keep just one visit per person per date;
	proc sort data=visit_df&year. nodupkey;
		by ENROLID DATE;
	run;

	proc delete data=o&year.; run; 

%mend;

* ============================================================================;
* Run the extraction;
* ============================================================================;

* Get birthdates --------------------------------------------------------------;
%getbirthdates(year=08, yeartag=2); *1sam;
%getbirthdates(year=09, yeartag=1); *1sam;
%getbirthdates(year=10, yeartag=1); *1sam;
%getbirthdates(year=11, yeartag=1); *1sam;
%getbirthdates(year=12, yeartag=1); *1sam;
%getbirthdates(year=13, yeartag=1); *1sam;
%getbirthdates(year=14, yeartag=1); *1sam;
%getbirthdates(year=15, yeartag=1); *1sam;
%getbirthdates(year=16, yeartag=1); *1sam;
%getbirthdates(year=17, yeartag=1); *1sam;
%getbirthdates(year=18, yeartag=1); *1sam;

* Combine birthdates into a single data table ---------------------------------;
data cohortBirthdates;
	set cohortBirthdates08
		cohortBirthdates09
		cohortBirthdates10
		cohortBirthdates11
		cohortBirthdates12
		cohortBirthdates13
		cohortBirthdates14
		cohortBirthdates15
		cohortBirthdates16
		cohortBirthdates17
		cohortBirthdates18;
run;

* Ensure we've only got one birthdate per person ------------------------------;
proc sort data=cohortBirthdates;
	by ENROLID BIRTH_DATE;
run;
data cohortBirthdates (keep=ENROLID BIRTH_DATE);
	set cohortBirthdates;
	by ENROLID;
	if first.ENROLID;
run;
proc delete data=cohortBirthdates08; run; 
proc delete data=cohortBirthdates09; run; 
proc delete data=cohortBirthdates10; run; 
proc delete data=cohortBirthdates11; run; 
proc delete data=cohortBirthdates12; run; 
proc delete data=cohortBirthdates13; run; 
proc delete data=cohortBirthdates14; run; 
proc delete data=cohortBirthdates15; run; 
proc delete data=cohortBirthdates16; run; 
proc delete data=cohortBirthdates17; run; 
proc delete data=cohortBirthdates18; run; 

* Get yearly cohorts ----------------------------------------------------------;
%getcohort(year=08, yeartag=2); *1sam;
%getcohort(year=09, yeartag=1); *1sam;
%getcohort(year=10, yeartag=1); *1sam;
%getcohort(year=11, yeartag=1); *1sam;
%getcohort(year=12, yeartag=1); *1sam;
%getcohort(year=13, yeartag=1); *1sam;
%getcohort(year=14, yeartag=1); *1sam;
%getcohort(year=15, yeartag=1); *1sam;
%getcohort(year=16, yeartag=1); *1sam;
%getcohort(year=17, yeartag=1); *1sam;
%getcohort(year=18, yeartag=1); *1sam;
proc delete data=cohortBirthdates; run; 

* Combine yearly cohorts into a single data table -----------------------------;
data cohort;
	set cohort08
		cohort09
		cohort10
		cohort11
		cohort12
		cohort13
		cohort14
		cohort15
		cohort16
		cohort17
		cohort18;
run;
proc delete data=cohort08; run; 
proc delete data=cohort09; run; 
proc delete data=cohort10; run; 
proc delete data=cohort11; run; 
proc delete data=cohort12; run; 
proc delete data=cohort13; run; 
proc delete data=cohort14; run; 
proc delete data=cohort15; run; 
proc delete data=cohort16; run; 
proc delete data=cohort17; run; 
proc delete data=cohort18; run; 

* Refine to a cohort of people present for five straight years ----------------;
%refinecohort(); *1sam;

* Get prescription data -------------------------------------------------------;
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

* Combine prescription data into a single data table --------------------------;
data rx_df;
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

* Get visit data --------------------------------------------------------------;
%getvisits_pre15(year=08, yeartag=2)
%getvisits_pre15(year=09, yeartag=1)
%getvisits_pre15(year=10, yeartag=1)
%getvisits_pre15(year=11, yeartag=1)
%getvisits_pre15(year=12, yeartag=1)
%getvisits_pre15(year=13, yeartag=1)
%getvisits_pre15(year=14, yeartag=1)
%getvisits_post15(year=15, yeartag=1)
%getvisits_post15(year=16, yeartag=1)
%getvisits_post15(year=17, yeartag=1)
%getvisits_post15(year=18, yeartag=1)


* Combine visit data into a single data table ---------------------------------;
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


* Save data to output ---------------------------------------------------------;
proc export data=cohort
	outfile='/home/kissler/PediatricPrescribing_Chronic/output/memb_df.csv'
	dbms=csv
	replace;
run;

proc export data=rx_df
	outfile='/home/kissler/PediatricPrescribing_Chronic/output/rx_df.csv'
	dbms=csv
	replace;
run;

proc export data=visit_df
	outfile='/home/kissler/PediatricPrescribing_Chronic/output/visit_df.csv'
	dbms=csv
	replace;
run;

