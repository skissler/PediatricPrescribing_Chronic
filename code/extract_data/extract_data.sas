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
libname dat15 "/data/markscan_authorized/data/commercial/2015";
libname dat16 "/data/markscan_authorized/data/commercial/2016";
libname dat17 "/data/markscan_authorized/data/commercial/2017";
libname dat18 "/data/markscan_authorized/data/commercial/2018";
libname dat19 "/data/markscan_authorized/data/commercial/2019";

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
	data CohortBirthdates&year. (keep=ENROLID SVCDATE);
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
	proc sort data=CohortBirthdates&year.;
		by ENROLID SVCDATE;
	run;

	* For each person, pull out the earliest date (the birth date);
	data CohortBirthdates&year. (keep=ENROLID BIRTH_DATE);
		set CohortBirthdates&year. (rename=(SVCDATE=BIRTH_DATE));
		by ENROLID;
		if first.ENROLID;
	run;

%mend;

* Extract individuals with prescriptions under the age of 5; 
%macro getcohort(year=,yeartag=);

	* Initial import, ensuring we have RX data and age <= 3;
	data Cohort&year. (keep=DT_MONTH DT_YEAR DTEND EGEOLOC MSA ENROLID MEMDAYS SEX);
		set dat&year..ccaet&year.&yeartag. (keep=AGE RX DTEND EGEOLOC MSA ENROLID MEMDAYS SEX where=(RX="1" and AGE<5));
		DT_MONTH=month(DTEND);
		DT_YEAR=year(DTEND);
	run;

	* Restrict to valid states;
	proc sort data=Cohort&year.;
		by EGEOLOC;
	run;

	data Cohort&year. (keep=DT_MONTH DT_YEAR DTEND STATE MSA ENROLID MEMDAYS SEX BIRTH_DATE);
		merge EGEOLOClist (in=inleft)
		Cohort&year. (in=inright);
		by EGEOLOC; 
		IF inleft & inright; 
	run;

	* Restrict to those with a birth date;
	proc sort data=Cohort&year.;
		by ENROLID;
	run;

	data Cohort&year. (keep=DT_MONTH DT_YEAR DTEND STATE MSA ENROLID MEMDAYS SEX BIRTH_DATE);
		merge CohortBirthdates (in=inleft)
		Cohort&year. (in=inright);
		by ENROLID; 
		IF inleft & inright; 
	run;

	* Keep only rows corresponding to full months or the birth month;
	proc sort data=Cohort&year.;
		by DT_MONTH;
	run;

	data Cohort&year. (keep=DT_MONTH DT_YEAR DTEND STATE MSA ENROLID MEMDAYS SEX BIRTH_DATE NDAYS where=((MEMDAYS>=NDAYS) or (month(BIRTH_DATE)=DT_MONTH and year(BIRTH_DATE)=DT_YEAR)));
		merge dayspermonth (in=inleft)
		Cohort&year. (in=inright);
		by DT_MONTH;
		IF inleft & inright;
	run;

	* Keep only relevant columns;
	data Cohort&year. (keep=DT_MONTH DT_YEAR DTEND STATE MSA ENROLID SEX BIRTH_DATE);
		set Cohort&year.;
	run;

%mend;

* Reduce to individuals who are present for five straight years;
%macro refinecohort();

	* Sort the cohort table;
	proc sort data=Cohort;
		by ENROLID DT_YEAR DT_MONTH;
	run;

	* Count months from birth;
	data Cohort (keep=DT_MONTH DT_YEAR DTEND STATE MSA ENROLID SEX BIRTH_DATE COUNT);
		set Cohort;
		COUNT + 1;
		by ENROLID;
		if first.ENROLID then COUNT = 1;
	run;

	* Append an index column;
	data Cohort (keep=DT_MONTH DT_YEAR DTEND STATE MSA ENROLID SEX BIRTH_DATE COUNT BIRTHDIFF);
		set Cohort;
		BIRTHDIFF=12*(DT_YEAR-year(BIRTH_DATE))+(DT_MONTH-month(BIRTH_DATE))+1;
	run;

	* keep only rows where index = months from birth, which gives contiguous months from birth;
	data Cohort (keep=DTEND STATE MSA ENROLID SEX BIRTH_DATE COUNT BIRTHDIFF where=(COUNT=BIRTHDIFF));
		set Cohort;
	run;

	* keep only the last row;
	proc sort data=Cohort;
		by ENROLID COUNT;
	run;

	data Cohort (keep=DTEND STATE MSA ENROLID SEX BIRTH_DATE);
		set Cohort;
		by ENROLID;
		if last.ENROLID;
	run;

	data Cohort (keep=DTEND STATE MSA ENROLID SEX BIRTH_DATE DURATION);
		set Cohort;
		DURATION=DTEND-BIRTH_DATE;
	run;

	proc sort data=Cohort;
		by DURATION;
	run;

	data Cohort (keep=STATE MSA ENROLID SEX BIRTH_DATE DURATION);
		set Cohort;
		by DURATION;
		if DURATION>=720;
	run;

	data Cohort (keep=STATE MSA ENROLID SEX BIRTH_DATE CENSOR_DATE);
		set Cohort;
		CENSOR_DATE=BIRTH_DATE+720;
		format CENSOR_DATE mmddyys10.;
	run;

%mend;

* Get prescription data;
%macro getrx(year=,yeartag=);

	proc sort data=Cohort;
		by ENROLID;
	run;

	* Import 'd' and reduce to those with valid birth date;
	data d&year. (keep=ENROLID NDCNUM REFILL SVCDATE BIRTH_DATE CENSOR_DATE);
		merge Cohort (in=inleft)
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

	proc sort data=Cohort;
		by ENROLID;
	run;

	* Import 'o' and reduce to those with valid birth date;
	data o&year. (keep=DX1 DX2 ENROLID SVCDATE BIRTH_DATE CENSOR_DATE);
		merge Cohort (in=inleft)
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

	proc sort data=Cohort;
		by ENROLID;
	run;

	* Import 'o' and reduce to those with valid birth date;
	data o&year. (keep=DX1 DX2 ENROLID SVCDATE DXVER BIRTH_DATE CENSOR_DATE);
		merge Cohort (in=inleft)
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

	proc sort data=visit_df&year. nodupkey;
		by ENROLID DATE;
	run;

	proc delete data=o&year.; run; 

%mend;

* ============================================================================;
* Run the extraction;
* ============================================================================;

* Get birthdates --------------------------------------------------------------;
%getbirthdates(year=16, yeartag=1sam); *1sam;
%getbirthdates(year=17, yeartag=1sam); *1sam;
%getbirthdates(year=18, yeartag=1sam); *1sam;

* Combine birthdates into a single data table ---------------------------------;
data CohortBirthdates;
	set CohortBirthdates16
		CohortBirthdates17
		CohortBirthdates18;
run;

* Ensure we've only got one birthdate per person ------------------------------;
proc sort data=CohortBirthdates;
	by ENROLID BIRTH_DATE;
run;
data CohortBirthdates (keep=ENROLID BIRTH_DATE);
	set CohortBirthdates;
	by ENROLID;
	if first.ENROLID;
run;
proc delete data=CohortBirthdates16; run; 
proc delete data=CohortBirthdates17; run; 
proc delete data=CohortBirthdates18; run; 

* Get yearly cohorts ----------------------------------------------------------;
%getcohort(year=16, yeartag=1sam); *1sam;
%getcohort(year=17, yeartag=1sam); *1sam;
%getcohort(year=18, yeartag=1sam); *1sam;
proc delete data=CohortBirthdates; run; 

* Combine yearly cohorts into a single data table -----------------------------;
data Cohort;
	set Cohort16
		Cohort17
		Cohort18;
run;
proc delete data=Cohort16; run; 
proc delete data=Cohort17; run; 
proc delete data=Cohort18; run; 

* Refine to a cohort of people present for five straight years ----------------;
%refinecohort(); *1sam;

* Get prescription data -------------------------------------------------------;
%getrx(year=16, yeartag=1sam)
%getrx(year=17, yeartag=1sam)
%getrx(year=18, yeartag=1sam)

* Combine prescription data into a single data table --------------------------;
data rx_df;
	set d16
		d17
		d18;
run;
proc delete data=d16; run; 
proc delete data=d17; run; 
proc delete data=d18; run; 

* Get visit data --------------------------------------------------------------;
%getvisits_post15(year=16, yeartag=1sam)
%getvisits_post15(year=17, yeartag=1sam)
%getvisits_post15(year=18, yeartag=1sam)


* Combine visit data into a single data table ---------------------------------;
data visit_df;
	set visit_df16 
		visit_df17 
		visit_df18;
run;

proc delete data=visit_df16; run; 
proc delete data=visit_df17; run; 
proc delete data=visit_df18; run; 



* Save data to output ---------------------------------------------------------;
proc export data=Cohort
	outfile='/home/kissler/PediatricPrescribing_Chronic/output/Cohort_2022-08-23.csv'
	dbms=csv
	replace;
run;

proc export data=rx_df
	outfile='/home/kissler/PediatricPrescribing_Chronic/output/rx_df_2022-08-23.csv'
	dbms=csv
	replace;
run;

proc export data=visit_df
	outfile='/home/kissler/PediatricPrescribing_Chronic/output/visit_df_2022-08-23.csv'
	dbms=csv
	replace;
run;
































* * Extract cohort of people who are represented for the entire year;
* %macro getcohort(year=,yeartag=);

* 	* Initial import, ensuring we have RX data;
* 	data GeoCohort&year. (keep=AGE DT_MONTH EGEOLOC MSA ENROLID MEMDAYS SEX);
* 		set dat&year..ccaet&year.&yeartag. (keep=AGE DTSTART EGEOLOC MSA ENROLID MEMDAYS RX SEX where=(RX="1"));
* 		DT_MONTH=month(DTSTART);
* 	run;

* 	* Restrict to valid states;
* 	proc sort data=GeoCohort&year.;
* 		by EGEOLOC;
* 	run;

* 	data GeoCohort&year. (keep=AGE DT_MONTH STATE MSA ENROLID MEMDAYS SEX);
* 		merge EGEOLOClist (in=inleft)
* 		GeoCohort&year. (in=inright);
* 		by EGEOLOC; 
* 		IF inleft & inright; 
* 	run;

* 	* Restrict to those with complete coverage through the year; 
* 	proc sort data=GeoCohort&year.;
* 		by DT_MONTH;
* 	run;

* 	data GeoCohort&year. (keep=AGE DT_MONTH STATE MSA ENROLID MEMDAYS SEX NDAYS where=(MEMDAYS>=NDAYS));
* 		merge dayspermonth (in=inleft)
* 		GeoCohort&year. (in=inright);
* 		by DT_MONTH;
* 		IF inleft & inright;
* 	run;

* 	* Sort so that we extract age in January;
* 	proc sort data=GeoCohort&year.;
* 		by ENROLID descending DT_MONTH;
* 	run;	

* 	* Count months of enrollment and only keep those with 12;
* 	* https://stats.idre.ucla.edu/sas/faq/how-can-i-create-an-enumeration-variable-by-groups/;
* 	data GeoCohort&year. (keep=AGE DT_MONTH STATE MSA ENROLID SEX COUNT where=(COUNT=12));
* 		set GeoCohort&year.;
* 		COUNT + 1;
* 		by ENROLID;
* 		if first.ENROLID then COUNT = 1;
* 	run;

* 	* Turn age into age groups; 
* 	data GeoCohort&year. (keep=AGEGRP STATE MSA ENROLID SEX);
* 		set GeoCohort&year.;
* 		* if MSA="" then MSA="00000";
* 		if missing(MSA) then MSA=0;
* 		if AGE>=80 then AGEGRP="80plus";
* 		else if AGE>=75 then AGEGRP="75_79";
* 		else if AGE>=70 then AGEGRP="70_74";
* 		else if AGE>=65 then AGEGRP="65_69";
* 		else if AGE>=60 then AGEGRP="60_64";
* 		else if AGE>=55 then AGEGRP="55_59";
* 		else if AGE>=50 then AGEGRP="50_54";
* 		else if AGE>=45 then AGEGRP="45_49";
* 		else if AGE>=40 then AGEGRP="40_44";
* 		else if AGE>=35 then AGEGRP="35_39";
* 		else if AGE>=30 then AGEGRP="30_34";
* 		else if AGE>=25 then AGEGRP="25_29";
* 		else if AGE>=20 then AGEGRP="20_24";
* 		else if AGE>=15 then AGEGRP="15_19";
* 		else if AGE>=10 then AGEGRP="10_14";
* 		else if AGE>=5 then AGEGRP="05_09";
* 		else if AGE>=0 then AGEGRP="00_04";
* 	run;

* 	* Sort cohort table by ENROLID for joining later;
* 	proc sort data=GeoCohort&year.;
* 		by ENROLID;
* 	run;

* %mend;



