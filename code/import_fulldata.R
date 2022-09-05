# =============================================================================
# Import
# =============================================================================

# --- Full build requires ~???mins --------------------------------------------

library(tidyverse) 
library(data.table) 
library(haven) 
library(lubridate) 
source('code/utils.R')

# =============================================================================
# Associate prescriptions with visits:
# =============================================================================

printtime(msg='Starting script')

memb_df <- read_csv("output/memb_df.csv", col_types=list(
	col_character(), col_character(), col_character(), col_character(), col_character(), col_character())) %>% 
	mutate(BIRTH_DATE=mdy(BIRTH_DATE)) %>% 
	mutate(CENSOR_DATE=mdy(CENSOR_DATE)) %>% 
	mutate(STATE=case_when(
		STATE=="South Carolin"~"South Carolina",
		STATE=="North Carolin"~"North Carolina",
		TRUE~STATE)) %>% 
	setDT()

rx_df <- read_csv("output/rx_df.csv", col_types=list(
	col_character(), col_character(), col_character(), col_character())) %>% 
	mutate(DATE=mdy(DATE)) %>% 
	setDT() 

visit_df <- read_csv("output/visit_df.csv", col_types=list(
	col_character(), col_character(), col_character(), col_character(), col_character(),col_character(),col_character(),col_character())) %>% 
	mutate(DATE=mdy(DATE)) %>% 
	setDT()

printtime(msg='Imported prescription and visit data')

rx_df <- setorder(rx_df,ENROLID, DATE)[,ID:=1:.N]
visit_df <- setorder(visit_df,ENROLID, DATE)[,ID:=1:.N]

printtime(msg='Added index column to prescripton and visit data')

rx_df <- setnames(rx_df,"DATE","DATE_RX")[,DATE_RX_M7:=DATE_RX-days(7)]
visit_df <- setorder(setnames(visit_df,c("DATE","ID"),c("DATE_VISIT","ID_VISIT")),ENROLID,DATE_VISIT)
rx_df <- rx_df[, c("ASSOC_VISIT_ID") := # Define new column
	visit_df[rx_df, # Join
		.(ID_VISIT), # Get column you need 
		on=.(ENROLID, # Join conditions
			DATE_VISIT<=DATE_RX,
			DATE_VISIT>=DATE_RX_M7),
		mult="last"
	]][,.(ENROLID, DATE_RX, CODE, REFILL, ID, ASSOC_VISIT_ID)]
visit_df <- setnames(visit_df, c("DATE_VISIT","ID_VISIT"),c("DATE","ID"))
rx_df <- setnames(rx_df, "DATE_RX","DATE")

printtime(msg='Finished linking')


# -----------------------------------------------------------------------------
# Keep only people born before 2013: 
memb_df <- memb_df[year(BIRTH_DATE)<=2013]

rx_df <- memb_df[,.(ENROLID)][rx_df, on=.(ENROLID), nomatch=0]
visit_df <- memb_df[,.(ENROLID)][visit_df, on=.(ENROLID), nomatch=0]

printtime(msg='Finished restricting cohorts to continuous enrollment')

# printtime(msg='Saved files')
printtime(msg='Finished script')
