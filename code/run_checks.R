# Population summary: ----------------------------------------------------------

memb_by_tot <- memb_df %>% 
	as_tibble() %>% 
	summarise(NMEMB=n()) %>% 
	mutate(PCT=round(NMEMB/sum(NMEMB)*100,1)) %>% 
	mutate(CAT="Total") %>% 
	select(CAT, NMEMB, PCT)

memb_by_sex <- memb_df %>% 
	as_tibble() %>% 
	group_by(SEX) %>% 
	summarise(NMEMB=n()) %>% 
	mutate(PCT=round(NMEMB/sum(NMEMB)*100,1)) %>% 
	mutate(CAT=case_when(SEX==1~"Male",TRUE~"Female")) %>% 
	select(CAT, NMEMB, PCT)

memb_by_birthyear <- memb_df %>% 
	as_tibble() %>% 
	group_by(BIRTH_YEAR) %>% 
	summarise(NMEMB=n()) %>% 
	mutate(PCT=round(NMEMB/sum(NMEMB)*100,1)) %>% 
	mutate(CAT=as.character(BIRTH_YEAR)) %>% 
	select(CAT, NMEMB, PCT)

sepdf <- tibble(CAT="---",NMEMB=NA_real_, PCT=NA_real_)

memb_summary <- bind_rows(memb_by_tot, sepdf, memb_by_sex, sepdf, memb_by_birthyear)




# Diagnosis codes in positions 3 and 4: ----------------------------------------

# dxprops <- tibble(
# 	prop_dx1=nrow(visit_df[!is.na(DX1)])/nrow(visit_df),
# 	prop_dx2=nrow(visit_df[!is.na(DX2)])/nrow(visit_df),
# 	prop_dx3=nrow(visit_df[!is.na(DX3)])/nrow(visit_df),
# 	prop_dx4=nrow(visit_df[!is.na(DX4)])/nrow(visit_df)
# )

respcondlist <- c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media")

respconddf <- visit_df %>% 
	as_tibble() %>% 
	select(DX1, DX2, DX3, DX4, ICD) %>% 
	left_join(select(ccs_map,-PRIORITY), by=c("DX1"="DX","ICD"="ICD")) %>% 
	rename(COND1=COND) %>% 
	left_join(select(ccs_map,-PRIORITY), by=c("DX2"="DX","ICD"="ICD")) %>% 
	rename(COND2=COND) %>% 
	left_join(select(ccs_map,-PRIORITY), by=c("DX3"="DX","ICD"="ICD")) %>% 
	rename(COND3=COND) %>% 
	left_join(select(ccs_map,-PRIORITY), by=c("DX4"="DX","ICD"="ICD")) %>% 
	rename(COND4=COND)

# How many visits have a respiratory condition in positions 3 or 4? 

respconddf %>% 
	mutate(haslateresp=case_when(
		((COND3%in%respcondlist) | (COND4%in%respcondlist))~1,
		TRUE~0)) %>% 
	group_by(haslateresp) %>% 
	summarise(NVISITS=n()) %>% 
	mutate(PCT=round(NVISITS/sum(NVISITS)*100,1))

#   haslateresp NVISITS   PCT
#         <dbl>   <int> <dbl>
# 1           0 5546941  99.2
# 2           1   42533   0.8


# Of those with a respiratory condition in positions 3 or 4, how many also have a respiratory condition in 1 or 2? 
respconddf %>% 
	mutate(haslateresp=case_when(
		((COND3%in%respcondlist) | (COND4%in%respcondlist))~1,
		TRUE~0)) %>% 
	filter(haslateresp==1) %>% 
	mutate(hasearlyresp=case_when(
		((COND1%in%respcondlist) | (COND2%in%respcondlist))~1,
		TRUE~0)) %>% 
	group_by(hasearlyresp) %>% 
	summarise(NVISITS=n()) %>% 
	mutate(PCT=round(NVISITS/sum(NVISITS)*100,1))


#   hasearlyresp NVISITS   PCT
#          <dbl>   <int> <dbl>
# 1            0    9910  23.3
# 2            1   32623  76.7





# dxprops_resp <- tibble(
# 	prop_dx1_resp=nrow(visit_df[((!is.na(DX1))&(COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media")))])/nrow(visit_df),
# 	prop_dx2_resp=nrow(visit_df[((!is.na(DX2))&(COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media")))])/nrow(visit_df),
# 	prop_dx3_resp=nrow(visit_df[((!is.na(DX3))&(COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media")))])/nrow(visit_df),
# 	prop_dx4_resp=nrow(visit_df[((!is.na(DX4))&(COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media")))])/nrow(visit_df)
# )

# > dxprops
# # A tibble: 1 × 4
#   prop_dx1 prop_dx2 prop_dx3 prop_dx4
#      <dbl>    <dbl>    <dbl>    <dbl>
# 1    0.997    0.231   0.0871   0.0172

# > dxprops_resp
# # A tibble: 1 × 4
#   prop_dx1_resp prop_dx2_resp prop_dx3_resp prop_dx4_resp
#           <dbl>         <dbl>         <dbl>         <dbl>
# 1         0.256        0.0789        0.0257       0.00376









# Proportion of kids w/ chronic conditions in the top 20% of abx recipients: ---

length(intersect(chronic_ids, superuser_ids))/length(superuser_ids)
# 0.3551236

length(intersect(chronic_ids, memb_df$ENROLID))/nrow(memb_df)
# 0.2020455

# Venue of antibiotic prescribing: ---------------------------------------------

stdplac_table <- read_csv("data/stdplac_table.csv", col_types=list(col_character(),col_character()))

stdplac_summary <- visit_df[ID %in% (rx_df$ASSOC_VISIT_ID)] %>% 
	as_tibble() %>% 
	group_by(STDPLAC) %>% 
	summarise(NVISITS=n()) %>% 
	ungroup() %>% 
	mutate(PCTVISITS=round(NVISITS/sum(NVISITS)*100,1)) %>% 
	arrange(desc(NVISITS)) %>% 
	left_join(stdplac_table, by="STDPLAC")

# STDPLAC NVISITS PCTVISITS STDPLACNAME
# <chr>     <int>     <dbl> <chr>
# 11       711746      87.9 Office
# 22        37329       4.6 Outpatient Hospital-On Campus
# 20        26833       3.3 Urgent Care Facility
# 23        11432       1.4 Emergency Room - Hospital
# 81        10632       1.3 Independent Laboratory
# 12         4063       0.5 Patient Home
# 99         2663       0.3 Other/Unknown
# 24         1670       0.2 Ambulatory Surgical Center
# 21         1058       0.1 Inpatient Hospital
# 72          637       0.1 Rural Health Clinic
# 17          410       0.1 Walk-in Retail Health Clinic
# 95          275       0   Outpatient (NEC)
# 49          208       0   Independent Clinic
# 25          205       0   Birthing Center
# 41          117       0   Ambulance (land)
# 55          105       0   Residential Subst Abuse Facil
# 34           91       0   Hospice
# 50           54       0   Federally Qualified Health Ctr
# 1            42       0   Pharmacy
# 71           42       0   State/Local Public Health Clin
# 62           21       0   Comprehensive Outpt Rehab Fac
# NA           18       0   NA
# 19           10       0   Outpatient Hospital-Off Campus
# 3             7       0   School
# 65            6       0   End-Stage Renal Disease Facil
# 31            5       0   Skilled Nursing Facility
# 53            3       0   Community Mental Health Center
# 14            2       0   Group Home
# 32            2       0   Nursing Facility
# 61            2       0   Comprehensive Inpt Rehab Fac
# 60            1       0   Mass Immunization Center

# Generate a table of antibiotics ----------------------------------------------

abxlist_long <- read_csv(file="data/ndc_to_extract.csv") %>% 
	split(.$THRCLDS) %>% 
	map(~ split(., .$THRDTDS)) %>% 
	map(~ map(., ~ pull(., NDCNUM))) %>% 
	map(~ imap(., ~ tibble(NAME=.y, NDC=.x))) %>% 
	map(~ bind_rows(.)) %>% 
	bind_rows(.id="CLASS") 

abxlist <- abxlist_long %>% 
	group_by(CLASS,NAME) %>% 
	summarise(NDCLIST=Reduce(paste,NDC))

write_csv(abxlist, file="figures/abxlist_long.csv")
write_csv(abxlist, file="figures/abxlist.csv")


# Generate a list of CCS categories we're interested in ------------------------

ccs9_map_full_dx <- read_csv("data/ccs_dxref2015.csv",
	skip=1,
	col_names=c("DX","CCS_CAT","CCS_DESC","ICD_DESC","OPT_CCS_CAT","OPT_CCS_DESC"),
	col_types=list(rep(col_character(),6)),
	quote="\'") %>% 
	select(DX,CCS_DESC,ICD_DESC) %>% 
	mutate(ICD="9")

ccs0_map_full_dx <- read_csv("data/ccsr_dxref2021.csv",
	skip=1,
	col_names=c("DX","ICD_DESC","CCS_CAT","CCS_DESC","INPT_DEFLT","OTPT_DFLT","RATIONALE","8","9"),
	col_types=list(rep(col_character(),9))) %>% 
	select(DX,CCS_DESC,ICD_DESC) %>% 
	mutate(ICD="0")

substrV <- Vectorize(substr)
revV <- Vectorize(rev)

ccs_map_full_dx <- bind_rows(ccs9_map_full_dx, ccs0_map_full_dx) %>% 
	mutate(substrstart=as.numeric(regexpr("\"",ICD_DESC))+1) %>% 
	# mutate(substrstart=case_when(substrstart<0 ~ 0, TRUE~substrstart)) %>% 
	mutate(ICD_DESC=substr(ICD_DESC,substrstart,10000)) %>% 
	mutate(substrend=as.numeric(regexpr("\"",ICD_DESC))) %>% 
	mutate(substrend=case_when(substrend<0~10000, TRUE~substrend-1)) %>% 
	mutate(ICD_DESC=substr(ICD_DESC,1,substrend)) %>% 
	select(-substrstart, -substrend) 

ccs_map_desc <- ccs_map %>% 
	as_tibble() %>% 
	left_join(ccs_map_full_dx, by=c("DX","ICD")) %>% 
	select(COND, DX, ICD, ICD_DESC)

write_csv(ccs_map_desc, file="figures/ccs_map_desc.csv")

# How many MSAs in the final analysis? -----------------------------------------

msadf <- memb_df %>% 
	as_tibble() %>% 
	filter(!is.na(MSA)) %>% 
	filter(MSA!="00000") %>% 
	group_by(MSA) %>% 
	summarise(NMEMB=n()) %>% 
	arrange(desc(NMEMB))


