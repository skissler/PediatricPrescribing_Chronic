# =============================================================================
# Import
# =============================================================================

library(tidyverse) 
library(data.table) 
library(lubridate) 
source('code/utils.R')
source('code/utils_private.R')

ccs_map <- setDT(read_csv("data/ccs_map.csv", col_types=list(col_character(),col_character(),col_character())))

# Define CCS priority (get rid of doubles): 
cond_priority <- setDT(data.frame(
	COND=c("Pneumonia",
			"UTI",
			"Intestinal infection",
			"Strep pharyngitis",
			"Sinusitis",
			"URI (other)",
			"SSTI",
			"Otitis media",
			"Asthma",
			"Bacterial infection (other)",
			"Tonsillitis",
			"Bronchitis (acute)",
			"Influenza",
			"Viral infection",
			"Routine child health exam"),
	PRIORITY=1:15))
# Roughly following Fleming-Dutra

# Ensure ccs_map has only 1:1 diagnosis to condition, based on above priority:
ccs_map <- cond_priority[
	ccs_map,on=.(COND)][
	,.(COND,ICD,PRIORITY,MAX_PRIORITY=min(PRIORITY)),by=.(DX)][
	PRIORITY==MAX_PRIORITY][
	,.(COND,DX,ICD,PRIORITY)]	              

# visit_df <- ccs_map[visit_df, on=.(DX=DX1, ICD=ICD)]

visit_df <- ccs_map[ # left-join visit_df on dx2
	ccs_map[ # left-join visit_df on dx1
	visit_df, on=.(DX=DX1, ICD=ICD)][
	,.(COND1=COND,ICD,DX1=DX,PRIORITY1=PRIORITY,DX2,ENROLID,DATE,ID)]
	,on=.(DX=DX2, ICD=ICD)][
	,.(COND1,COND2=COND,DX1,DX2=DX,PRIORITY1,PRIORITY2=PRIORITY,ICD,ENROLID,DATE,ID)][
	is.na(PRIORITY1),PRIORITY1:=as.integer(1e6)][ # set NA cond priority as 1M
	is.na(PRIORITY2),PRIORITY2:=as.integer(1e6)][ # set NA cond priority as 1M
	,COND:=if_else(PRIORITY2<PRIORITY1,COND2,COND1)][ # Prioritize conditions
	,.(ENROLID,DATE,COND,DX1,DX2,ICD,ID)] # Choose output columns
