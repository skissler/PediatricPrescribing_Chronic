#!/usr/bin/env Rscript

# =============================================================================
# Import
# =============================================================================

# --- Full build requires ~???mins --------------------------------------------

library(tidyverse) 
library(data.table) 
library(haven) 
library(lubridate) 
source('code/utils.R')

printtime(msg='Starting script')

# Initialize the output data frame: 
event_df <- setDT(data.frame()) 
	
# =============================================================================
# Find all birth dates in 2008-2018:
# =============================================================================

# --- ~??min ------------------------------------------------------------------

printtime(msg='Starting birth extraction')
for(y in match(c("08","09","10","11","12"), yearlist)){

	# --- ~10min/block --------------------------------------------------------

	# --- Import 's' (inpatient services) table: ------------------------------
	s_df <- read_sas(filenames_s_sam[y],
		col_select=c(
			'AGE',
			'DX1',
			'DX2',
			'ENROLID',
			'SVCDATE'
			), 
		skip=0, n_max=Inf)
	# 2010sam: 2048118 recs

	# --- Reduce 's' (inpatient services) table: ------------------------------
	s_df <- setDT(s_df) # Set as data table
	s_df <- s_df[AGE==0] # Restrict to possible newborns (age = 0)
	# 2010sam: 162285 recs

	s_df <- s_df[
		DX1%like%birthcodes_prefix_icd9 | 
		DX2%like%birthcodes_prefix_icd9 | 
		DX1%like%birthcodes_prefix_icd10 | 
		DX2%like%birthcodes_prefix_icd10 ] # Restrict to birth-related ICDs
	# 2010sam: 96926 recs
	s_df <- s_df[,.(DATE=min(SVCDATE), EVENT='birth', CODE=''), 
		by=.(ENROLID)] 
	# 2010sam: 8369 recs

	# Format like the output table: 
	event_df <- rbind(event_df, s_df) # bind to the output table
	rm(s_df) # clear s_df to save memory

	printtime(msg=paste0('    Finished year ',yearlist[y]))

}

event_df <- event_df[,.(DATE=first(DATE), EVENT=first(EVENT), CODE=first(CODE)),by=.(ENROLID)] # Make sure each ENROLID is only represented once 
# 2010:2018 sample files: 56996 recs
printtime(msg='Finished first block')


write_csv(event_df, file="output/event_df_R.csv")


# =============================================================================
# Restrict eligible set to those with continuous coverage:
# =============================================================================

printtime(msg='Starting censorship definition')
memb_df <- setDT(data.frame())
for(y in match(c("08","09","10","11","12"), yearlist)){

	# --- Import 't' (membership detail) table: ~??min ------------------------
	t_df <- read_sas(filenames_t_sam[y],
		col_select=c(
			'AGE',
			'DTEND',
			'MEMDAYS',
			'EGEOLOC',
			'ENROLID',
			'MSA',
			'SEX',
			'RX'
			), 
		skip=0, n_max=Inf)
	# 2010sam: 13840880 recs
	# 2018sam: 3357203 recs

	printtime(msg='    Finished import')

	# --- Restrict  to ENROLIDs with continuous coverage: ??min ---------------

	# Make data table: 
	t_df <- setDT(t_df) 
	# Restrict to those with a valid birthdate: 
	t_df <- event_df[,.(ENROLID, DATE)][t_df, nomatch=0, on=.(ENROLID)]
	# 2010sam: 47652 recs
	# 2018sam: 180586 recs
	# Restrict to those with prescription data: 
	t_df <- t_df[RX==1]
	# 2010sam: 42425 recs
	# 2018saam: 172668 recs
	t_df <- t_df[,RX:=NULL]
	# Rename DATE column as birthdate: 
	setnames(t_df, 'DATE', 'BIRTH_DATE')
	# Store colums for birth month and birth year
	t_df <- t_df[,BIRTH_MONTH:=month(BIRTH_DATE)]
	t_df <- t_df[,BIRTH_YEAR:=year(BIRTH_DATE)]
	# Restrict to those under 5 (first 5 years of life): 
	t_df <- t_df[AGE<=5]
	# 2010sam: 42425 recs
	# 2018sam: 122574 recs
	# Keep only rows corresponding to full months or the birth month: 
	t_df <- t_df[,DT_MONTH:=month(DTEND)]
	t_df <- t_df[,DT_YEAR:=yearlist_num_long[1]+(y-1)]
	t_df <- dayspermonth[t_df, on=.(DT_MONTH)]
	t_df <- t_df[(MEMDAYS>=NDAYS | 
		((month(BIRTH_DATE)==DT_MONTH) & (year(BIRTH_DATE)==DT_YEAR)))]
	t_df <- t_df[,NDAYS:=NULL]
	# 2010sam: 42371 recs
	# 2018sam: 122542 recs

	# Restrict to those with a valid state
	t_df <- egeoloclist[t_df, nomatch=0, on=.(EGEOLOC)]
	t_df <- t_df[,EGEOLOC:=NULL]
	# 2010sam: 42273 recs
	# 2018sam: 113337 recs

	# Append restricted membership detail to memb_df 
	memb_df <- rbind(memb_df, t_df) 
	rm(t_df)

	printtime(msg=paste0('    Finished year ',yearlist[y]))

	}

memb_df_raw <- memb_df 

write_csv(memb_df, file="output/memb_df_prereduce_R.csv")

# 2010-2018 sample memb_df: 1129099 recs

# Assign an index to each month of membership that counts from birth month:
memb_df <- memb_df[,INDEX:=12*(DT_YEAR-BIRTH_YEAR)+(DT_MONTH-BIRTH_MONTH)]
# Restrict to indices from the month after birth through end of enrollment:
memb_df <- memb_df[INDEX>=min(index_df$INDEX) & INDEX<=max(index_df$INDEX)]


# sam: 1098006 recs
# Find the first missing month for each person: 
memb_df <- memb_df[,CENSORINDEX:=min(setdiff(c(index_df$INDEX,Inf), .SD$INDEX)), by=.(ENROLID)]
# sam: 1098006 recs
# Cut off all months past the censor month:
memb_df <- memb_df[INDEX<CENSORINDEX]

write_csv(memb_df, file="output/cohort_intermediate_R.csv")

# sam: 933316 recs
# Get rid of the censor index column:
memb_df <- memb_df[,CENSORINDEX:=NULL]
# Append the censorship date: 
memb_df <- memb_df[,CENSOR_DATE:=max(DTEND), by=.(ENROLID)]

# Add censorship dates to event_df: 
# Find censorship dates: 
censor_df <- memb_df[,.(DATE=first(CENSOR_DATE), EVENT="censored", CODE=""),by=.(ENROLID)]
# Restrict event_df to people who also have a censorship date: 
event_df <- censor_df[,.(ENROLID)][event_df, nomatch=0, on=.(ENROLID)]
# Bind censorship dates to event_df: 
event_df <- rbind(event_df, censor_df) 
# Remove censorship dates data table: 
rm(censor_df) 

# Generate lookup table for membership attributes: 
memb_df <- memb_df[,.(STATE=first(STATE), MSA=first(MSA), SEX=first(SEX), BIRTH_DATE=first(BIRTH_DATE), CENSOR_DATE=first(CENSOR_DATE)),by=.(ENROLID)]
# sam: 45071 recs
memb_df <- memb_df[,NDAYS:=as.integer(difftime(CENSOR_DATE,BIRTH_DATE,units="days"))]
# sam: 45071 recs

memb_df <- memb_df[NDAYS>=1800]

write_csv(memb_df, file="output/memb_df_R.csv")

printtime(msg='    Finished reduction')

