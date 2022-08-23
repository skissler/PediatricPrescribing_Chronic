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
* proc import datafile="/home/kissler/PediatricPrescribing_Chronic/data/ndc_to_extract_geography.csv"
*         out=ndctoextract
*         dbms=csv
*         replace;
* run;

* proc sort data=ndctoextract;
* 	by NDCNUM;
* run;

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

* START CHECKING FROM HERE; 
* ============================================================================;
* Define extraction/reduction scripts;
* ============================================================================;

* Extract birth dates;
%macro getbirthdates(year=,yeartag=);

	* Initial import of inpatient services table;
	data CohortBirthdates&year. (keep=DX1 DX2 ENROLID SVCDATE);
		set dat&year..ccaes&year.&yeartag. (keep=AGE DX1 DX2 ENROLID SVCDATE where=(AGE=0 and (startsWith(DX1, "V30") or 
				startsWith(DX1, "V31") or 
				startsWith(DX1, "V32") or 
				startsWith(DX1, "V33") or 
				startsWith(DX1, "V34") or 
				startsWith(DX1, "V35") or 
				startsWith(DX1, "V36") or 
				startsWith(DX1, "V37") or 
				startsWith(DX1, "Z38") or 
				startsWith(DX2, "V30") or 
				startsWith(DX2, "V31") or 
				startsWith(DX2, "V32") or 
				startsWith(DX2, "V33") or 
				startsWith(DX2, "V34") or 
				startsWith(DX2, "V35") or 
				startsWith(DX2, "V36") or 
				startsWith(DX2, "V37") or 
				startsWith(DX2, "Z38"))));
	run;

	* Sort by visit date;
	proc sort data=CohortBirthdates&year.;
		by SVCDATE;
	run;

	* For each person, pull out the earliest date (the birth date);
	* data CohortBirthdates&year. (keep=ENROLID DX1 DX2 SVCDATE);
	* 	set CohortBirthdates&year.;
	* 	by ENROLID;
	* 	if first.ENROLID;
	* run;

%mend;

* ============================================================================;
* Run the extraction;
* ============================================================================;

%getbirthdates(year=17, yeartag=1sam); *1sam;

proc export data=CohortBirthdates17
	outfile='/home/kissler/PediatricPrescribing_Chronic/output/CohortBirthdates17_2022-02-18.csv'
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



