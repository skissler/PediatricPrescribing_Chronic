# =============================================================================
# Import
# =============================================================================

library(tidyverse) 
library(data.table) 
library(lubridate) 
source('code/utils.R')
source('code/utils_private.R')

alphasig <- 0.05 # for 95% CIs 

# =============================================================================
# Refine CCS data
# =============================================================================

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

# =============================================================================
# Append conditions to visit_df based on diagnosing priority
# =============================================================================

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


# ==============================================================================
# Initial cleaning of the main data files 
# ==============================================================================

# Exclude Hawaii since there are just 5 people who meet inclusion crit.: 
memb_df <- memb_df[STATE!='Hawaii']
rx_df <- memb_df[,.(ENROLID)][rx_df, on=.(ENROLID), nomatch=0]
visit_df <- memb_df[,.(ENROLID)][visit_df, on=.(ENROLID), nomatch=0]

# Add well child visits to conditions: 
visit_df[is.na(COND) & (DX1=="V202" | DX2=="V202") & ICD=="9", COND:="Routine health exam"]
visit_df[is.na(COND) & (DX1=="Z00129" | DX2=="Z00129") & ICD=="0", COND:="Routine health exam"]

# ==============================================================================
# Deal with geographic data
# ==============================================================================

# Import state name abbreviations: 
state_abbrev <- read_csv("data/state_abbrev.csv")

# Import MSA names and population sizes: 
msadat <- setDT(read_csv("data/msadat.csv"))
msadat$MSA <- as.character(msadat$MSA)

# Append MSA names and population sizes to memb_df: 
memb_df <- msadat[memb_df, on=.(MSA)]

# Figure out how many MarketScan people are in each MSA: 
msasizes <- memb_df[MSA!="00000"][
	,.(N=.N, MSA_NAME=first(MSA_NAME), MSA_POP=first(MSA_POP)), by=.(MSA)] %>%
	as_tibble() %>% 
	arrange(desc(N))

# Generate a list of MSAs with at least 100 MarketScan people: 
bigmsalist <- msasizes %>% filter(N>=100) %>% pull(MSA)

load("data/state_boundaries.RData") 

HHS_boundaries_lwr48 <- state_boundaries %>% 
	make_lwr48 %>%
	makeHHS %>%
	group_by(HHS) %>%
	summarise(geometry=st_union(geometry))

# ==============================================================================
# Deal with drug information
# ==============================================================================

# Store NDCs for key drug classes ---------------------------------------------
abx_ndcs <- read_csv("data/ndc_to_extract.csv")
setDT(abx_ndcs)

# ==============================================================================
# Generate ICD and CCS lookup tables 
# ==============================================================================

# Get a list of ICD codes and descriptions for lookup: 
icd9list <- read_csv('data/icd9list.csv') %>% 
	select(`DIAGNOSIS CODE`, `LONG DESCRIPTION`) %>% 
	rename(DX=`DIAGNOSIS CODE`, DESC_LONG=`LONG DESCRIPTION`) %>% 
	mutate(ICD="9")
icd10list <- read_csv('data/icd10list.csv', col_names=c("DX","DESC_LONG")) %>% 
	mutate(ICD="0")
icdlist <- setDT(bind_rows(icd9list, icd10list))

# Get a list of CCS codes and descriptions for lookup: 
ccs9list <- read_csv('data/ccs_dxref2015_formatted_2.csv') %>% 
	select(`ICD-9-CM CODE`, `ICD-9-CM CODE DESCRIPTION`, `CCS CATEGORY`, `CCS CATEGORY DESCRIPTION`) %>% 
	rename(DX=`ICD-9-CM CODE`) %>% 
	rename(DESC_LONG=`ICD-9-CM CODE DESCRIPTION`) %>% 
	rename(CCS_CODE=`CCS CATEGORY`) %>% 
	rename(CCS_DESC=`CCS CATEGORY DESCRIPTION`) %>% 
	mutate(CCS_CODE=as.character(CCS_CODE)) %>% 
	mutate(ICD="9")
ccs10list <- read_csv('data/ccsr_dxref2021.csv') %>% 
	select(`ICD-10-CM Code`, `ICD-10-CM Code Description`, `CCSR Category`, `CCSR Category Description`) %>% 
	rename(DX=`ICD-10-CM Code`) %>%
	rename(DESC_LONG=`ICD-10-CM Code Description`) %>% 
	rename(CCS_CODE=`CCSR Category`) %>% 
	rename(CCS_DESC=`CCSR Category Description`) %>%
	mutate(ICD="0")
ccslist <- setDT(bind_rows(ccs9list, ccs10list))


# ==============================================================================
# Deal with chronic conditions
# ==============================================================================

# Note - if you update these, also update them in make_chronic.R 
chronic_priorities <- tibble(BODY_SYSTEM=c("pulmonary_respiratory",
	"cardiac",
	"renal",
	"genitourinary",
	"gastrointestinal",
	"dermatological",
	"hematological",
	"musculoskeletal",
	"immunological",
	"metabolic",
	"genetic",
	"neurological",
	"malignancy",
	"ophthalmological",
	"otolaryngological",
	"endocrinological",
	"mental_health",
	"otologic",
	"craniofacial")) %>% 
	mutate(PRIORITY=1:n())
setDT(chronic_priorities)

chronicconddf <- read_csv("data/chronicconddf.csv", col_types=list(col_character(),col_character(),col_character()))
setDT(chronicconddf)

# Map to chronic conditions: 
ccmap <- chronic_priorities[,.(BODY_SYSTEM_2=BODY_SYSTEM,PRIORITY_2=PRIORITY)][
	chronic_priorities[,.(BODY_SYSTEM_1=BODY_SYSTEM,PRIORITY_1=PRIORITY)][
	chronicconddf[,.(DX2=DX,BODY_SYSTEM_2=BODY_SYSTEM,ICD)][
	chronicconddf[,.(DX1=DX,BODY_SYSTEM_1=BODY_SYSTEM,ICD)][
	visit_df, on=.(DX1, ICD)], on=.(DX2, ICD)], on=.(BODY_SYSTEM_1)],on=.(BODY_SYSTEM_2)][
	,.(ENROLID, BODY_SYSTEM_1, BODY_SYSTEM_2)][
	!(is.na(BODY_SYSTEM_1) & is.na(BODY_SYSTEM_2))] %>% 
	as_tibble() %>% 
	pivot_longer(c("BODY_SYSTEM_1","BODY_SYSTEM_2")) %>% 
	rename(BODY_SYSTEM=value) %>% 
	select(ENROLID, BODY_SYSTEM) %>% 
	filter(!is.na(BODY_SYSTEM)) %>% 
	group_by(ENROLID, BODY_SYSTEM) %>% 
	slice(1) %>% 
	mutate(CC_FLAG=1) %>% 
	pivot_wider(names_from=BODY_SYSTEM, values_from=CC_FLAG)
setDT(ccmap)

# ==============================================================================
# Set up weights
# ==============================================================================

# cohort_sizes <- memb_df[,.(ENROLID, BIRTH_YEAR=year(BIRTH_DATE))][
# 	,.(NMEMB=.N),by=.(BIRTH_YEAR)]

# cohort_sizes_state <- memb_df[,.(ENROLID, BIRTH_YEAR=year(BIRTH_DATE), STATE)][
# 	,.(NMEMB=.N),by=.(BIRTH_YEAR, STATE)]

# cohort_sizes_HHS <- cohort_sizes_state %>% 
# 	as_tibble() %>% 
# 	makeHHS() %>% 
# 	group_by(BIRTH_YEAR, HHS) %>% 
# 	summarise(NMEMB=sum(NMEMB)) %>% 
# 	setDT()

# Define cohort sizes by different strata: 
cohort_sizes_state_sex <- memb_df[,.(ENROLID, BIRTH_YEAR=year(BIRTH_DATE), STATE, SEX)][
	,.(NMEMB=.N),by=.(BIRTH_YEAR, STATE, SEX)]

cohort_sizes_state_sex_noyear <- memb_df[,.(ENROLID, STATE, SEX)][
	,.(NMEMB=.N),by=.(STATE, SEX)]

cohort_sizes_msa_sex <- memb_df[MSA %in% bigmsalist
	,.(ENROLID, BIRTH_YEAR=year(BIRTH_DATE), MSA, SEX)][
	,.(NMEMB=.N), by=.(BIRTH_YEAR, MSA, SEX)]

cohort_sizes_msa_sex_noyear <- memb_df[MSA %in% bigmsalist
	,.(ENROLID, MSA, SEX)][
	,.(NMEMB=.N), by=.(MSA, SEX)]

yearlist <- unique(cohort_sizes_state_sex$BIRTH_YEAR)

# Get comprehensive lists of states and conditions in the MarketScan data: 
condlist <- sort(unique(as.character(visit_df[!is.na(COND)]$COND)))
statelist <- sort(unique(as.character(memb_df[!is.na(STATE)]$STATE)))

# Import 2010 census population sizes under age 5 (see get_popsizes_under5.R):
popsizes_under5 <- read_csv("data/popsizes_under5.csv", 
	col_types = list(col_character(), col_character(), col_double(), col_double())) 
setDT(popsizes_under5)
popsizes_under5 <- popsizes_under5[STATE!='Hawaii']

# Append birth years to popsizes_under5 for better joining: 
popsizes_under5_birthyear <- Reduce(rbind,lapply(yearlist, function(x){popsizes_under5[,.(STATE,SEX,POPSIZE,BIRTH_YEAR=x)]}))

# Generate weights by birth year: 
newweights <- cohort_sizes_state_sex[
	popsizes_under5_birthyear,on=.(STATE,SEX,BIRTH_YEAR)][
	is.na(NMEMB),NMEMB:=0][
	,WEIGHT:=POPSIZE/NMEMB][
	,POPFRAC:=POPSIZE/sum(POPSIZE),by=.(BIRTH_YEAR)][
	,WEIGHT_INDIV:=POPFRAC/NMEMB][
	,.(BIRTH_YEAR,STATE,SEX,POPFRAC,WEIGHT,WEIGHT_INDIV)]

newweights_noyear <- cohort_sizes_state_sex_noyear[
	popsizes_under5[,.(STATE,SEX,POPSIZE)],on=.(STATE,SEX)][
	is.na(NMEMB),NMEMB:=0][
	,WEIGHT:=POPSIZE/NMEMB][
	,POPFRAC:=POPSIZE/sum(POPSIZE)][
	,WEIGHT_INDIV_NOYEAR:=POPFRAC/NMEMB][
	,.(STATE,SEX,WEIGHT_INDIV_NOYEAR)]

newweights_stategroup_noyear <- cohort_sizes_state_sex_noyear[
	popsizes_under5[,.(STATE,SEX,POPSIZE)],on=.(STATE,SEX)][
	is.na(NMEMB),NMEMB:=0][
	,.(STATE,SEX,NMEMB,POPSIZE,POPFRAC=POPSIZE/sum(POPSIZE)),by=.(STATE)][
	,WEIGHT_INDIV_STATEGROUP_NOYEAR:=POPFRAC/NMEMB][
	,.(STATE,SEX,WEIGHT_INDIV_STATEGROUP_NOYEAR)]

# On average, states have 51.1% boys and 48.9% girls. Use this for weights. 
newweights_msagroup_noyear <- cohort_sizes_msa_sex_noyear[
	SEX==1,POPFRAC:=0.511][
	SEX==2,POPFRAC:=0.489][
	,WEIGHT_INDIV_MSAGROUP_NOYEAR:=POPFRAC/NMEMB][
	,.(MSA,SEX,WEIGHT_INDIV_MSAGROUP_NOYEAR)]

# ==============================================================================
# Combine and refine the main data tables
# ==============================================================================

memb_df <- newweights[
	memb_df[,BIRTH_YEAR:=year(BIRTH_DATE)], on=.(BIRTH_YEAR,STATE,SEX)]
memb_df <- newweights_noyear[memb_df, on=.(STATE,SEX)]
memb_df <- newweights_stategroup_noyear[memb_df, on=.(STATE,SEX)]
memb_df <- newweights_msagroup_noyear[memb_df, on=.(MSA,SEX)]

visit_df <- newweights[
	memb_df[,.(ENROLID, STATE, SEX, MSA, MSA_NAME, MSA_POP, BIRTH_DATE, BIRTH_YEAR)][
	visit_df, 
	on=.(ENROLID)], 
	on=.(BIRTH_YEAR,STATE,SEX)]
visit_df <- newweights_noyear[visit_df, on=.(STATE,SEX)]
visit_df <- newweights_stategroup_noyear[visit_df, on=.(STATE,SEX)]
visit_df <- newweights_msagroup_noyear[visit_df, on=.(MSA,SEX)]
visit_df <- visit_df[,AGE_DAYS:=as.numeric(difftime(DATE,BIRTH_DATE,units="days"))]

rx_df <- newweights[
	memb_df[,.(ENROLID, STATE, SEX, MSA, MSA_NAME, MSA_POP, BIRTH_YEAR, BIRTH_DATE)][
	rx_df, on=.(ENROLID)][
	,.(ENROLID, STATE, SEX, MSA, MSA_NAME, MSA_POP, BIRTH_DATE, BIRTH_YEAR, DATE, CODE, REFILL, ID, ASSOC_VISIT_ID)], 
	on=.(BIRTH_YEAR,STATE,SEX)][
	CODE%in%abx_ndcs$NDCNUM,CLASS:="Antibiotics"]
rx_df <- newweights_noyear[rx_df, on=.(STATE,SEX)]
rx_df <- newweights_stategroup_noyear[rx_df, on=.(STATE,SEX)]
rx_df <- newweights_msagroup_noyear[rx_df, on=.(MSA,SEX)]
rx_df <- rx_df[CLASS=="Antibiotics",ISABX:=1]
rx_df <- rx_df[is.na(ISABX),ISABX:=0]
rx_df <- rx_df[,AGE_DAYS:=as.numeric(difftime(DATE,BIRTH_DATE,units="days"))]


