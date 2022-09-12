
# Calculate cumulative prescriptions by age 5 by MSA for respiratory conditions:
rx_summ_msa_resp <- visit_df[,.(ASSOC_VISIT_ID=ID,COND)][
	rx_df, on=.(ASSOC_VISIT_ID)][
	COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media")][
	MSA %in% bigmsalist
	,.(ENROLID,STATE,MSA,MSA_POP,MSA_NAME,ID,WEIGHT_INDIV_MSAGROUP_NOYEAR)][
	,.(NRX=sum(WEIGHT_INDIV_MSAGROUP_NOYEAR),STATE=first(STATE),MSA_POP=first(MSA_POP), MSA_NAME=first(MSA_NAME)),by=.(MSA)] %>% 
	as_tibble() %>% 
	arrange(MSA) %>%
	makeHHS %>% 
	select(HHS, STATE, MSA, MSA_POP, MSA_NAME, NRX)

# Calculate cumulative prescriptions by age 5 by MSA for non-respiratory conditions:
rx_summ_msa_nonresp <- visit_df[,.(ASSOC_VISIT_ID=ID,COND)][
	rx_df, on=.(ASSOC_VISIT_ID)][
	!(COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media"))][
	MSA %in% bigmsalist
	,.(ENROLID,STATE,MSA,MSA_POP,MSA_NAME,ID,WEIGHT_INDIV_MSAGROUP_NOYEAR)][
	,.(NRX=sum(WEIGHT_INDIV_MSAGROUP_NOYEAR),STATE=first(STATE),MSA_POP=first(MSA_POP), MSA_NAME=first(MSA_NAME)),by=.(MSA)] %>% 
	as_tibble() %>% 
	arrange(MSA) %>%
	makeHHS %>% 
	select(HHS, STATE, MSA, MSA_POP, MSA_NAME, NRX)

write_csv(rx_summ_msa_resp, file="figures/rx_summ_msa_resp.csv")
write_csv(rx_summ_msa_nonresp, file="figures/rx_summ_msa_nonresp.csv")

# Calculate proportion of children who have received a prescriptions by age 5 by MSA for respiratory conditions:
first_rx_msa_resp <- visit_df[,.(ASSOC_VISIT_ID=ID,COND)][
	rx_df, on=.(ASSOC_VISIT_ID)][
	COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media")][
	MSA %in% bigmsalist][
	,AGE_DAYS:=as.numeric(difftime(DATE,BIRTH_DATE,units="days"))][
	,.(AGE_DAYS=min(AGE_DAYS)), by=.(ENROLID)][
		memb_df, on=.(ENROLID)][
	is.na(AGE_DAYS), AGE_DAYS:=Inf] %>% 
	as_tibble()

# Calculate proportion of children who have received a prescriptions by age 5 by MSA for non-respiratory conditions:
first_rx_msa_nonresp <- visit_df[,.(ASSOC_VISIT_ID=ID,COND)][
	rx_df, on=.(ASSOC_VISIT_ID)][
	!(COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media"))][
	MSA %in% bigmsalist][
	,AGE_DAYS:=as.numeric(difftime(DATE,BIRTH_DATE,units="days"))][
	,.(AGE_DAYS=min(AGE_DAYS)), by=.(ENROLID)][
		memb_df, on=.(ENROLID)][
	is.na(AGE_DAYS), AGE_DAYS:=Inf] %>% 
	as_tibble() 

write_csv(first_rx_msa_resp, file="figures/first_rx_msa_resp.csv")
write_csv(first_rx_msa_nonresp, file="figures/first_rx_msa_nonresp.csv")

