# =============================================================================
# Import
# =============================================================================
# Note: the following requires the files that are generated in the 'extract_data' directory. 

library(tidyverse) 
library(broom)
library(purrr) 
library(lubridate) 
library(scales) 
library(sf)
source('code/utils.R')
source('code/utils_private.R')

# Data extraction (only needs to be run once):
system('sas code/extract_data/extract_data.sas')

# Data import:
source('code/import_fulldata.R')
source('code/reduce_under5.R')

# Data cleaning and setup: 
source('code/clean_data.R')

# =============================================================================
# Figure 1A: Cumulative prescriptions
# =============================================================================

rx_df_drug <- rx_df[
	,AGE_DAYS:=as.numeric(difftime(DATE,BIRTH_DATE,units="days"))]

cumrx_df_drug <- setDT(data.frame())
for(t in seq(from=0, to=30*60, by=30)){
	cumrx_df_drug <- rbind(cumrx_df_drug,
		rx_df_drug[AGE_DAYS<=t,.(NOBS=.N, NRX=sum(WEIGHT_INDIV_NOYEAR),AGE_DAYS_ROUNDED=t),by=.(ISABX)])
}
cumrx_df_drug <- cumrx_df_drug[,NMEMB:=nrow(memb_df)]

# cumrx_df_drug <- as_tibble(cumrx_df_drug)
# fig_cumrx_drug <- cumrx_df_drug %>% 
# 	mutate(rawmean=NOBS/NMEMB) %>% 
# 	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
# 	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
# 	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
# 	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
# 	mutate(lwr=NRX-proplwr*NRX) %>% 
# 	mutate(upr=NRX+propupr*NRX) %>% 
# 	mutate(ISABX=factor(ISABX)) %>% 
# 	# select(BIRTH_YEAR, NOBS, NRX, rawmean, lwr, upr) %>% 
# 	ggplot(aes(x=AGE_DAYS_ROUNDED, y=NRX, col=factor(ISABX),fill=factor(ISABX))) + 
# 		geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=0.4) + 
# 		geom_line() + 
# 		scale_x_continuous(breaks=seq(from=0, to=1800, by=360)) + 
# 		theme_classic() + 
# 		labs(x="Days from birth", y=paste0("Cumulative prescriptions"), col="Drug class", fill="Drug class") + 
# 		scale_color_manual(values=c("Gray","Blue"),labels=c("0"="Non-antibiotics","1"="Antibiotics")) + 
# 		scale_fill_manual(values=c("Gray","Blue"),labels=c("0"="Non-antibiotics","1"="Antibiotics")) +
# 		theme(text=element_text(size=16))

rx_df_drug_resp <- visit_df[,.(ASSOC_VISIT_ID=ID,COND)][
	rx_df, on=.(ASSOC_VISIT_ID)][
	COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media")][
	,AGE_DAYS:=as.numeric(difftime(DATE,BIRTH_DATE,units="days"))]

rx_df_drug_nonresp <- visit_df[,.(ASSOC_VISIT_ID=ID,COND)][
	rx_df, on=.(ASSOC_VISIT_ID)][
	!(COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media"))][
	,AGE_DAYS:=as.numeric(difftime(DATE,BIRTH_DATE,units="days"))]

cumrx_df_drug_resp <- setDT(data.frame())
for(t in seq(from=0, to=30*60, by=30)){
	cumrx_df_drug_resp <- rbind(cumrx_df_drug_resp,
		rx_df_drug_resp[AGE_DAYS<=t,.(NOBS=.N, NRX=sum(WEIGHT_INDIV_NOYEAR),AGE_DAYS_ROUNDED=t),by=.(ISABX)])
}
cumrx_df_drug_resp <- cumrx_df_drug_resp[,NMEMB:=nrow(memb_df)]

cumrx_df_drug_nonresp <- setDT(data.frame())
for(t in seq(from=0, to=30*60, by=30)){
	cumrx_df_drug_nonresp <- rbind(cumrx_df_drug_nonresp,
		rx_df_drug_nonresp[AGE_DAYS<=t,.(NOBS=.N, NRX=sum(WEIGHT_INDIV_NOYEAR),AGE_DAYS_ROUNDED=t),by=.(ISABX)])
}
cumrx_df_drug_nonresp <- cumrx_df_drug_nonresp[,NMEMB:=nrow(memb_df)]

fulldat <- cumrx_df_drug %>% 
	mutate(rawmean=NOBS/NMEMB) %>% 
	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
	mutate(lwr=NRX-proplwr*NRX) %>% 
	mutate(upr=NRX+propupr*NRX) %>% 
	mutate(ISABX=factor(ISABX))

respdat <- cumrx_df_drug_resp %>% 
	mutate(rawmean=NOBS/NMEMB) %>% 
	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
	mutate(lwr=NRX-proplwr*NRX) %>% 
	mutate(upr=NRX+propupr*NRX) %>% 
	mutate(ISABX=factor(ISABX))

nonrespdat <- cumrx_df_drug_nonresp %>% 
	mutate(rawmean=NOBS/NMEMB) %>% 
	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
	mutate(lwr=NRX-proplwr*NRX) %>% 
	mutate(upr=NRX+propupr*NRX) %>% 
	mutate(ISABX=factor(ISABX))

combdat_abx <- bind_rows(
	mutate(fulldat,Indication="All conditions"),
	mutate(respdat,Indication="Respiratory conditions"),
	mutate(nonrespdat,Indication="Non-respiratory conditions")
	) %>% 
	filter(ISABX==1) 

fig_cumrx_respnonresp_abx <-
	ggplot(data=combdat_abx, aes(x=AGE_DAYS_ROUNDED, y=NRX, col=Indication, fill=Indication, linetype=Indication)) + 
		geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=0.4) + 
		geom_line() + 
		scale_x_continuous(breaks=seq(from=0, to=1800, by=360)) + 
		theme_classic() + 
		labs(tag="A)", x="Days from birth", y=paste0("Cumulative prescriptions")) + 
		scale_color_manual(values=c("All conditions"="Gray","Respiratory conditions"="Blue","Non-respiratory conditions"="Blue")) + 
		scale_fill_manual(values=c("All conditions"="Gray","Respiratory conditions"="Blue","Non-respiratory conditions"="Blue")) +
		scale_linetype_manual(values=c("All conditions"="solid","Respiratory conditions"="solid","Non-respiratory conditions"="dashed")) +
		theme(text=element_text(size=10))

# ggsave(fig_cumrx_respnonresp_abx, file="figures/cumrx.pdf", width=figwidth, height=figwidth, dpi=figres)
# ggsave(fig_cumrx_respnonresp_abx, file="figures/cumrx.png", width=figwidth, height=figwidth, dpi=figres)

# ggsave(fig_cumrx_respnonresp_abx + theme(legend.position='none'), file="figures/cumrx_nokey.pdf", width=figwidth, height=figwidth, dpi=figres)
# ggsave(fig_cumrx_respnonresp_abx + theme(legend.position='none'), file="figures/cumrx_nokey.png", width=figwidth, height=figwidth, dpi=figres)

# prescriptions by age 5: 
combdat_abx %>% 
	filter(AGE_DAYS_ROUNDED==1800) %>% 
	select(AGE_DAYS_ROUNDED, Indication, NRX, lwr, upr)

# Prescribing rates by age: 
# birth to 6mos (180 days): 
rxrate1 <- combdat_abx %>% 
	filter(Indication=="All conditions") %>% 
	filter(AGE_DAYS_ROUNDED==180) %>% 
	select(NRX,lwr,upr) %>% 
	mutate(NRX=NRX*2, lwr=lwr*2, upr=upr*2)


# 6 mos (180 days) to 2 years (720 days): 
rxrate2 <- combdat_abx %>% 
	filter(Indication=="All conditions") %>% 
	filter(AGE_DAYS_ROUNDED%in%c(180,720)) %>% 
	select(AGE_DAYS_ROUNDED,NRX,lwr,upr) %>% 
	mutate(AGE_DAYS_ROUNDED=AGE_DAYS_ROUNDED-lag(AGE_DAYS_ROUNDED),
		NRX=NRX-lag(NRX),
		lwr=lwr-lag(lwr),
		upr=upr-lag(upr)) %>% 
	filter(!is.na(AGE_DAYS_ROUNDED)) %>% 
	mutate(NRX=NRX/AGE_DAYS_ROUNDED*360,
		lwr=lwr/AGE_DAYS_ROUNDED*360,
		upr=upr/AGE_DAYS_ROUNDED*360) %>% 
	select(-AGE_DAYS_ROUNDED)

# 2 years (720 days) to 5 years (1800 days): 
rxrate3 <- combdat_abx %>% 
	filter(Indication=="All conditions") %>% 
	filter(AGE_DAYS_ROUNDED%in%c(720,1800)) %>% 
	select(AGE_DAYS_ROUNDED,NRX,lwr,upr) %>% 
	mutate(AGE_DAYS_ROUNDED=AGE_DAYS_ROUNDED-lag(AGE_DAYS_ROUNDED),
		NRX=NRX-lag(NRX),
		lwr=lwr-lag(lwr),
		upr=upr-lag(upr)) %>% 
	filter(!is.na(AGE_DAYS_ROUNDED)) %>% 
	mutate(NRX=NRX/AGE_DAYS_ROUNDED*360,
		lwr=lwr/AGE_DAYS_ROUNDED*360,
		upr=upr/AGE_DAYS_ROUNDED*360) %>% 
	select(-AGE_DAYS_ROUNDED)	

# By region: 
temp <- rx_df_drug[ISABX==1][
	,.(NRX=sum(WEIGHT_INDIV_STATEGROUP_NOYEAR)),by=.(STATE)]

# =============================================================================
# Figure 1B: Time to first prescription
# =============================================================================

first_rx_df_drug <- rx_df[
	,AGE_DAYS:=as.numeric(difftime(DATE,BIRTH_DATE,units="days"))][
	,.(WEIGHT_INDIV_NOYEAR=first(WEIGHT_INDIV_NOYEAR), AGE_DAYS=min(AGE_DAYS)), by=.(ENROLID,ISABX)]

first_cumrx_df_drug <- setDT(data.frame())
for(t in seq(from=0, to=30*60, by=30)){
	first_cumrx_df_drug <- rbind(first_cumrx_df_drug,
		first_rx_df_drug[AGE_DAYS<=t,.(NOBS=.N,NRX=sum(WEIGHT_INDIV_NOYEAR),AGE_DAYS_ROUNDED=t),by=.(ISABX)])
}
first_cumrx_df_drug <- first_cumrx_df_drug[,NMEMB:=nrow(memb_df)]

first_cumrx_df_drug <- as_tibble(first_cumrx_df_drug)
fig_first_cumrx_drug <- first_cumrx_df_drug %>% 
	mutate(rawmean=NOBS/NMEMB) %>% 
	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
	mutate(lwr=NRX-proplwr*NRX) %>% 
	mutate(upr=NRX+propupr*NRX) %>% 
	mutate(ISABX=factor(ISABX)) %>% 
	ggplot(aes(x=AGE_DAYS_ROUNDED, y=NRX, col=factor(ISABX),fill=factor(ISABX))) + 
		geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=0.4) + 
		geom_line() + 
		scale_x_continuous(breaks=seq(from=0, to=1800, by=360)) + 
		scale_y_continuous(limits=c(0,1), breaks=seq(from=0, to=1, by=0.2)) + 
		theme_classic() + 
		labs(x="Days from birth", y=paste0("Proportion who have received a prescription"), col="Drug class", fill="Drug class") + 
		scale_color_manual(values=c("Gray","Blue"),labels=c("0"="Non-antibiotics","1"="Antibiotics")) + 
		scale_fill_manual(values=c("Gray","Blue"),labels=c("0"="Non-antibiotics","1"="Antibiotics")) +
		theme(text=element_text(size=16))

# ggsave(fig_first_cumrx_drug, file="figures/letter_binary/first_cumrx_drug.pdf",width=8,height=5)
# ggsave(fig_first_cumrx_drug, file="figures/letter_binary/first_cumrx_drug.png",width=8,height=5)


tab_firstrx_df_drug <- first_cumrx_df_drug %>% 
	mutate(rawmean=NOBS/NMEMB) %>% 
	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
	mutate(lwr=NRX-proplwr*NRX) %>% 
	mutate(upr=NRX+propupr*NRX) %>% 
	select(ISABX, AGE_DAYS_ROUNDED, NRX, lwr, upr) %>% 
	filter(AGE_DAYS_ROUNDED %in% c(30*12*1, 30*12*2, 30*12*5)) %>% 
	arrange(ISABX, AGE_DAYS_ROUNDED) %>% 
	mutate(AGE=case_when(
		AGE_DAYS_ROUNDED==30*12*1~"1 year",
		AGE_DAYS_ROUNDED==30*12*2~"2 years",
		AGE_DAYS_ROUNDED==30*12*5~"5 years")) %>% 
	mutate(NRX=as.character(round(NRX*100,1))) %>% 
	mutate(lwr=paste0("(",as.character(round(lwr*100,1)),",")) %>% 
	mutate(upr=paste0(as.character(round(upr*100,1)),")")) %>% 
	select(ISABX,AGE,NRX,lwr,upr) 

# write_csv(tab_firstrx_df_drug, path="figures/letter_binary/firstrx_df_drug.csv")

# ------------------------------------------------------------------------------
# For respiratory conditions: 

first_rx_df_drug_resp <- visit_df[,.(ASSOC_VISIT_ID=ID,COND)][
	rx_df, on=.(ASSOC_VISIT_ID)][
	COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media")][
	,AGE_DAYS:=as.numeric(difftime(DATE,BIRTH_DATE,units="days"))][
	,.(WEIGHT_INDIV_NOYEAR=first(WEIGHT_INDIV_NOYEAR), AGE_DAYS=min(AGE_DAYS)), by=.(ENROLID,ISABX)]

first_cumrx_df_drug_resp <- setDT(data.frame())
for(t in seq(from=0, to=30*60, by=30)){
	first_cumrx_df_drug_resp <- rbind(first_cumrx_df_drug_resp,
		first_rx_df_drug_resp[AGE_DAYS<=t,.(NOBS=.N,NRX=sum(WEIGHT_INDIV_NOYEAR),AGE_DAYS_ROUNDED=t),by=.(ISABX)])
}
first_cumrx_df_drug_resp <- first_cumrx_df_drug_resp[,NMEMB:=nrow(memb_df)]

first_rx_df_drug_nonresp <- visit_df[,.(ASSOC_VISIT_ID=ID,COND)][
	rx_df, on=.(ASSOC_VISIT_ID)][
	!(COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media"))][
	,AGE_DAYS:=as.numeric(difftime(DATE,BIRTH_DATE,units="days"))][
	,.(WEIGHT_INDIV_NOYEAR=first(WEIGHT_INDIV_NOYEAR), AGE_DAYS=min(AGE_DAYS)), by=.(ENROLID,ISABX)]

first_cumrx_df_drug_nonresp <- setDT(data.frame())
for(t in seq(from=0, to=30*60, by=30)){
	first_cumrx_df_drug_nonresp <- rbind(first_cumrx_df_drug_nonresp,
		first_rx_df_drug_nonresp[AGE_DAYS<=t,.(NOBS=.N,NRX=sum(WEIGHT_INDIV_NOYEAR),AGE_DAYS_ROUNDED=t),by=.(ISABX)])
}
first_cumrx_df_drug_nonresp <- first_cumrx_df_drug_nonresp[,NMEMB:=nrow(memb_df)]

first_cumrx_df_drug_resp <- as_tibble(first_cumrx_df_drug_resp)
fig_first_cumrx_drug_resp <- first_cumrx_df_drug_resp %>% 
	mutate(rawmean=NOBS/NMEMB) %>% 
	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
	mutate(lwr=NRX-proplwr*NRX) %>% 
	mutate(upr=NRX+propupr*NRX) %>% 
	mutate(ISABX=factor(ISABX)) %>% 
	ggplot(aes(x=AGE_DAYS_ROUNDED, y=NRX, col=factor(ISABX),fill=factor(ISABX))) + 
		geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=0.4) + 
		geom_line() + 
		scale_x_continuous(breaks=seq(from=0, to=1800, by=360)) + 
		scale_y_continuous(limits=c(0,1), breaks=seq(from=0, to=1, by=0.2)) + 
		theme_classic() + 
		labs(x="Days from birth", y=paste0("Proportion who have received a prescription"), col="Drug class", fill="Drug class") + 
		scale_color_manual(values=c("Gray","Blue"),labels=c("0"="Non-antibiotics","1"="Antibiotics")) + 
		scale_fill_manual(values=c("Gray","Blue"),labels=c("0"="Non-antibiotics","1"="Antibiotics")) +
		theme(text=element_text(size=16))

# ggsave(fig_first_cumrx_drug_resp, file="figures/letter_binary/first_cumrx_drug_resp.pdf",width=8,height=5)
# ggsave(fig_first_cumrx_drug_resp, file="figures/letter_binary/first_cumrx_drug_resp.png",width=8,height=5)

fulldat <- first_cumrx_df_drug %>% 
	mutate(rawmean=NOBS/NMEMB) %>% 
	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
	mutate(lwr=NRX-proplwr*NRX) %>% 
	mutate(upr=NRX+propupr*NRX) %>% 
	mutate(ISABX=factor(ISABX))

respdat <- first_cumrx_df_drug_resp %>% 
	mutate(rawmean=NOBS/NMEMB) %>% 
	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
	mutate(lwr=NRX-proplwr*NRX) %>% 
	mutate(upr=NRX+propupr*NRX) %>% 
	mutate(ISABX=factor(ISABX))

nonrespdat <- first_cumrx_df_drug_nonresp %>% 
	mutate(rawmean=NOBS/NMEMB) %>% 
	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
	mutate(lwr=NRX-proplwr*NRX) %>% 
	mutate(upr=NRX+propupr*NRX) %>% 
	mutate(ISABX=factor(ISABX))

combdat <- bind_rows(mutate(fulldat,Indication="All conditions"),mutate(respdat,Indication="Respiratory conditions"))

 
fig_first_cumrx_drug_combined <- ggplot(data=combdat, aes(x=AGE_DAYS_ROUNDED, y=NRX, col=factor(ISABX),fill=factor(ISABX), linetype=Indication)) + 
	geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=0.4) + 
	geom_line() + 
	scale_x_continuous(breaks=seq(from=0, to=1800, by=360)) + 
	scale_y_continuous(limits=c(0,1), breaks=seq(from=0, to=1, by=0.2)) + 
	theme_classic() + 
	labs(x="Days from birth", y=paste0("Proportion who have received a prescription"), col="Drug class", fill="Drug class") + 
	scale_color_manual(values=c("Gray","Blue"),labels=c("0"="Non-antibiotics","1"="Antibiotics")) + 
	scale_fill_manual(values=c("Gray","Blue"),labels=c("0"="Non-antibiotics","1"="Antibiotics")) +
	scale_linetype_manual(values=c("solid","dashed")) + 
	theme(text=element_text(size=16))

# Some statistics: 
# first_cumrx_df_drug %>% 
# 	mutate(rawmean=NOBS/NMEMB) %>% 
# 	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
# 	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
# 	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
# 	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
# 	mutate(lwr=NRX-proplwr*NRX) %>% 
# 	mutate(upr=NRX+propupr*NRX) %>% 
# 	filter(AGE_DAYS_ROUNDED %in% c(360, 2*360, 1800)) %>% 
# 	select(CLASS, AGE_DAYS_ROUNDED, NRX, lwr, upr) %>% 
# 	arrange(CLASS, AGE_DAYS_ROUNDED)

combdat_abx <- bind_rows(
	mutate(fulldat,Indication="All conditions"),
	mutate(respdat,Indication="Respiratory conditions"),
	mutate(nonrespdat,Indication="Non-respiratory conditions")
	) %>% 
	filter(ISABX==1) 

fig_first_cumrx_respnonresp_abx <-
	ggplot(data=combdat_abx, aes(x=AGE_DAYS_ROUNDED, y=NRX, col=Indication, fill=Indication, linetype=Indication)) + 
		geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=0.4) + 
		geom_line() + 
		scale_x_continuous(breaks=seq(from=0, to=1800, by=360)) + 
		scale_y_continuous(limits=c(0,1), breaks=seq(from=0, to=1, by=0.2)) + 
		theme_classic() + 
		labs(tag="B)",x="Days from birth", y=paste0("Proportion who have received a prescription")) + 
		scale_color_manual(values=c("All conditions"="Gray","Respiratory conditions"="Blue","Non-respiratory conditions"="Blue")) + 
		scale_fill_manual(values=c("All conditions"="Gray","Respiratory conditions"="Blue","Non-respiratory conditions"="Blue")) +
		scale_linetype_manual(values=c("All conditions"="solid","Respiratory conditions"="solid","Non-respiratory conditions"="dashed")) +
		theme(text=element_text(size=10))

ggsave(fig_first_cumrx_respnonresp_abx, file="figures/firstrx.pdf", width=figwidth, height=figwidth, dpi=figres)
ggsave(fig_first_cumrx_respnonresp_abx, file="figures/firstrx.png", width=figwidth, height=figwidth, dpi=figres)

ggsave(fig_first_cumrx_respnonresp_abx+theme(legend.position="none"), file="figures/firstrx_nokey.pdf", width=figwidth, height=figwidth, dpi=figres)
ggsave(fig_first_cumrx_respnonresp_abx+theme(legend.position="none"), file="figures/firstrx_nokey.png", width=figwidth, height=figwidth, dpi=figres)

combdat_abx %>% 	
	filter(AGE_DAYS_ROUNDED==1800) %>% 
	select(AGE_DAYS_ROUNDED, Indication, NRX, lwr, upr)

# ==============================================================================
# Figure 1C: Prescribing histogram
# ==============================================================================

cumrx_summ_overall <- rx_df_drug[ISABX==1][
	,.(ENROLID,ISRX=1)][
	memb_df[,.(ENROLID)], on=.(ENROLID)][
	is.na(ISRX),ISRX:=0][
	,.(NRX=sum(ISRX)),by=.(ENROLID)] %>% 
	as_tibble() %>% 
	mutate(Indication="All conditions")

cumrx_summ_resp <- rx_df_drug_resp[ISABX==1][
	,.(ENROLID,ISRX=1)][
	memb_df[,.(ENROLID)], on=.(ENROLID)][
	is.na(ISRX),ISRX:=0][
	,.(NRX=sum(ISRX)),by=.(ENROLID)] %>% 
	as_tibble() %>% 
	mutate(Indication="Respiratory infections")

cumrx_summ_nonresp <- rx_df_drug_nonresp[ISABX==1][
	,.(ENROLID,ISRX=1)][
	memb_df[,.(ENROLID)], on=.(ENROLID)][
	is.na(ISRX),ISRX:=0][
	,.(NRX=sum(ISRX)),by=.(ENROLID)] %>% 
	as_tibble() %>% 
	mutate(Indication="Other conditions")

cumrx_summ <- rbind(cumrx_summ_overall, cumrx_summ_resp, cumrx_summ_nonresp)

fig_cumrx_summ <- cumrx_summ %>% 
	ggplot(aes(x=NRX, fill=Indication, col=Indication, lty=Indication)) + 
		# geom_histogram(aes(y=..density..), position="identity", binwidth=1, alpha=0.2) + 
		geom_density(adjust=5, alpha=0.4) + 
		scale_x_continuous(limits=c(0,40)) + 
		scale_color_manual(values=c("All conditions"="black","Respiratory infections"="blue","Other conditions"="blue")) + 
		scale_fill_manual(values=c("All conditions"="black","Respiratory infections"="blue","Other conditions"="blue")) + 
		scale_linetype_manual(values=c("All conditions"="solid","Respiratory infections"="solid","Other conditions"="dashed")) + 
		theme_classic() + 
		theme(text=element_text(size=10)) + 
		labs(x="Number of antibiotic prescriptions", y="Proportion of children (as density)")

ggsave(fig_cumrx_summ, file="figures/cumrx_summ.pdf",width=figwidth, height=figwidth,dpi=figres)
ggsave(fig_cumrx_summ, file="figures/cumrx_summ.png",width=figwidth, height=figwidth,dpi=figres)

ggsave(fig_cumrx_summ + theme(legend.position="none"), file="figures/cumrx_summ_nokey.pdf",width=figwidth, height=figwidth,dpi=figres)
ggsave(fig_cumrx_summ + theme(legend.position="none"), file="figures/cumrx_summ_nokey.png",width=figwidth, height=figwidth,dpi=figres)

# ==============================================================================
# Asymmetry in prescribing
# ==============================================================================

rxrankdf_drug_overall_abx <- rx_df[,.(ENROLID,ISABX,ID)][
	Reduce(rbind, lapply(
		c(0,1),
		function(x){memb_df[,.(ENROLID,WEIGHT_INDIV_NOYEAR,ISABX=x)]})),
	on=.(ENROLID,ISABX)][
	!is.na(ID),NRX:=1][
	is.na(ID),NRX:=0][
	,.(NRX=sum(NRX),WEIGHT_INDIV_NOYEAR=first(WEIGHT_INDIV_NOYEAR)),by=.(ENROLID,ISABX)] %>% 
	as_tibble() %>% 
	shuffledf() %>% 
	arrange(ISABX,desc(NRX)) %>% 
	group_by(ISABX) %>% 
	mutate(TOTRX=sum(NRX)) %>% 
	mutate(PROPRX=NRX/TOTRX) %>% 
	mutate(CUMPROPRX=cumsum(PROPRX)) %>% 
	select(-TOTRX, -PROPRX) %>% 
	mutate(RANK=1:n()) %>% 
	mutate(PRANK=RANK/max(RANK)) %>% 
	mutate(PRANK_WEIGHTED=cumsum(WEIGHT_INDIV_NOYEAR)) %>% 
	ungroup() %>% 
	filter(ISABX==1)

rxrankdf_drug_overall_abx %>% filter(CUMPROPRX>0.5)
rxrankdf_drug_overall_abx %>% filter(PRANK_WEIGHTED>0.2)

rxrankdf_drug_resp_abx <- visit_df[,.(ASSOC_VISIT_ID=ID,COND)][
	rx_df, on=.(ASSOC_VISIT_ID)][
	COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media")][
	,.(ENROLID,ISABX,ID)][
	Reduce(rbind, lapply(
		c(0,1),
		function(x){memb_df[,.(ENROLID,WEIGHT_INDIV_NOYEAR,ISABX=x)]})),
	on=.(ENROLID,ISABX)][
	!is.na(ID),NRX:=1][
	is.na(ID),NRX:=0][
	,.(NRX=sum(NRX),WEIGHT_INDIV_NOYEAR=first(WEIGHT_INDIV_NOYEAR)),by=.(ENROLID,ISABX)] %>% 
	as_tibble() %>% 
	shuffledf() %>% 
	arrange(ISABX,desc(NRX)) %>% 
	group_by(ISABX) %>% 
	mutate(TOTRX=sum(NRX)) %>% 
	mutate(PROPRX=NRX/TOTRX) %>% 
	mutate(CUMPROPRX=cumsum(PROPRX)) %>% 
	select(-TOTRX, -PROPRX) %>% 
	mutate(RANK=1:n()) %>% 
	mutate(PRANK=RANK/max(RANK)) %>% 
	mutate(PRANK_WEIGHTED=cumsum(WEIGHT_INDIV_NOYEAR)) %>% 
	ungroup() %>% 
	filter(ISABX==1)


rxrankdf_drug_nonresp_abx <- visit_df[,.(ASSOC_VISIT_ID=ID,COND)][
	rx_df, on=.(ASSOC_VISIT_ID)][
	!(COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media"))][
	,.(ENROLID,ISABX,ID)][
	Reduce(rbind, lapply(
		c(0,1),
		function(x){memb_df[,.(ENROLID,WEIGHT_INDIV_NOYEAR,ISABX=x)]})),
	on=.(ENROLID,ISABX)][
	!is.na(ID),NRX:=1][
	is.na(ID),NRX:=0][
	,.(NRX=sum(NRX),WEIGHT_INDIV_NOYEAR=first(WEIGHT_INDIV_NOYEAR)),by=.(ENROLID,ISABX)] %>% 
	as_tibble() %>% 
	shuffledf() %>% 
	arrange(ISABX,desc(NRX)) %>% 
	group_by(ISABX) %>% 
	mutate(TOTRX=sum(NRX)) %>% 
	mutate(PROPRX=NRX/TOTRX) %>% 
	mutate(CUMPROPRX=cumsum(PROPRX)) %>% 
	select(-TOTRX, -PROPRX) %>% 
	mutate(RANK=1:n()) %>% 
	mutate(PRANK=RANK/max(RANK)) %>% 
	mutate(PRANK_WEIGHTED=cumsum(WEIGHT_INDIV_NOYEAR)) %>% 
	ungroup() %>% 
	filter(ISABX==1)


rxrankdf_abx_combined <- bind_rows(
	mutate(rxrankdf_drug_overall_abx,COND="All conditions"),
	mutate(rxrankdf_drug_resp_abx,COND="Respiratory conditions"),
	mutate(rxrankdf_drug_nonresp_abx,COND="Non-respiratory conditions"),
	)


fig_rankcurve_respnonresp_abx <- rxrankdf_abx_combined %>% 
	ggplot(aes(x=PRANK_WEIGHTED, y=CUMPROPRX, col=COND, lty=COND)) + 
		# geom_line(stat="smooth", method="loess", span=2) + 
		geom_line() + 
		scale_x_continuous(breaks=seq(from=0,to=1,by=0.1)) + 
		scale_y_continuous(breaks=seq(from=0,to=1,by=0.1)) + 
		labs(x="Proportion of children, ranked", y="Proportion of prescriptions", col="Condition", lty="Condition") + 
		theme_classic() + 
		scale_color_manual(values=c("All conditions"="Gray","Respiratory conditions"="Blue","Non-respiratory conditions"="Blue")) + 
		scale_linetype_manual(values=c("All conditions"="solid","Respiratory conditions"="solid","Non-respiratory conditions"="dashed")) +
		theme(text=element_text(size=10))

ggsave(fig_rankcurve_respnonresp_abx, file="figures/rankcurve.pdf", width=figwidth, height=figwidth, dpi=figres)
ggsave(fig_rankcurve_respnonresp_abx, file="figures/rankcurve.png", width=figwidth, height=figwidth, dpi=figres)

ggsave(fig_rankcurve_respnonresp_abx+theme(legend.position="none"), file="figures/rankcurve_nokey.pdf", width=figwidth, height=figwidth, dpi=figres)
ggsave(fig_rankcurve_respnonresp_abx+theme(legend.position="none"), file="figures/rankcurve_nokey.png", width=figwidth, height=figwidth, dpi=figres)

# ==============================================================================
# Chronic condition analysis
# ==============================================================================

superuserdf <- rxrankdf_drug_overall_abx %>% 
	filter(PRANK_WEIGHTED<=0.2)
setDT(superuserdf)

temp1 <- chronicconddf[,.(DX, BODY_SYSTEM, ICD)][
	visit_df[,.(ENROLID,DX=DX1,ICD)],on=.(DX,ICD)]
temp2 <- chronicconddf[,.(DX, BODY_SYSTEM, ICD)][
	visit_df[,.(ENROLID,DX=DX2,ICD)],on=.(DX,ICD)]
temp <- rbind(temp1, temp2)
tempcount <- temp[!is.na(BODY_SYSTEM)][
	,.(NVISITS=.N),by=.(ICD,DX,BODY_SYSTEM)] %>% 
	as_tibble() %>% 
	arrange(BODY_SYSTEM, desc(NVISITS)) %>% 
	left_join(icd9list, by=c("DX","ICD")) %>% 
	left_join(icd10list, by=c("DX","ICD"), suffix=c(".9",".0")) %>% 
	split(.$BODY_SYSTEM) %>% 
	map(~ select(., -BODY_SYSTEM))

tempcount_clean_9 <- tempcount %>% 
	map(~ filter(., ICD==9)) %>% 
	map(~ select(., -ICD, -DESC_LONG.0)) %>% 
	map(~ mutate(., CUMVISITS=sum(.$NVISITS))) %>% 
	map(~ mutate(., PCTVISITS=round(NVISITS/CUMVISITS*1000)/10)) %>% 
	map(~ select(., PCTVISITS, DX, DESC_LONG.9))

tempcount_clean_10 <- tempcount %>% 
	map(~ filter(., ICD==0)) %>% 
	map(~ select(., -ICD, -DESC_LONG.9)) %>% 
	map(~ mutate(., CUMVISITS=sum(.$NVISITS))) %>% 
	map(~ mutate(., PCTVISITS=round(NVISITS/CUMVISITS*1000)/10)) %>% 
	map(~ select(., PCTVISITS, DX, DESC_LONG.0))

# tempcount_clean_9$pulmonary_respiratory
tempcount_clean_10$pulmonary_respiratory
tempcount_clean_10$otologic
tempcount_clean_10$immunological

#  tempcount$pulmonary_respiratory
# # A tibble: 160 × 5
#    ICD   DX     NVISITS DESC_LONG.9                                  DESC_LONG.0
#    <chr> <chr>    <int> <chr>                                        <chr>
#  1 9     49390    52472 Asthma, unspecified type, unspecified        NA
#  2 9     49392    14651 Asthma, unspecified type, with (acute) exac… NA
#  3 9     49300    13699 Extrinsic asthma, unspecified                NA
#  4 9     7707      8152 Chronic respiratory disease arising in the … NA
#  5 9     32723     6581 Obstructive sleep apnea (adult)(pediatric)   NA

tempcount$otologic
tempcount$immunological$DESC_LONG.0

tempcount_ccs <- temp[!is.na(BODY_SYSTEM)][
	,.(NVISITS=.N),by=.(ICD,DX,BODY_SYSTEM)] %>% 
	as_tibble() %>% 
	arrange(BODY_SYSTEM, desc(NVISITS)) %>% 
	left_join(select(ccs9list,-CCS_CODE), by=c("DX","ICD")) %>% 
	left_join(select(ccs10list,-CCS_CODE), by=c("DX","ICD"), suffix=c(".9",".0")) %>% 
	split(.$BODY_SYSTEM) %>% 
	map(~ select(., -BODY_SYSTEM))

tempcount_ccs$pulmonary_respiratory
tempcount_ccs$otologic

rxrankdf_full <-  rxrankdf_drug_overall_abx %>% 
	mutate(SUPERUSER=case_when(PRANK_WEIGHTED<=0.2~1,TRUE~0)) %>% 
	select(ENROLID, NRX, SUPERUSER) %>% 
	left_join(as_tibble(ccmap), by="ENROLID") %>% 
	replace_na(list(musculoskeletal=0,
	    mental_health=0,
	    immunological=0,
	    otolaryngological=0,
	    pulmonary_respiratory=0,
	    neurological=0,
	    cardiac=0,
	    ophthalmological=0,
	    renal=0,
	    genitourinary=0,
	    otologic=0,
	    metabolic=0,
	    hematological=0,
	    genetic=0,
	    gastrointestinal=0,
	    craniofacial=0,
	    endocrinological=0,
	    malignancy=0,
	    dermatological=0))

superlogisticdf <- rxrankdf_full %>% 
	mutate(SUPERUSER=as.factor(SUPERUSER),
		musculoskeletal=as.factor(musculoskeletal),
		mental_health=as.factor(mental_health),
		immunological=as.factor(immunological),
		otolaryngological=as.factor(otolaryngological),
		pulmonary_respiratory=as.factor(pulmonary_respiratory),
		neurological=as.factor(neurological),
		cardiac=as.factor(cardiac),
		ophthalmological=as.factor(ophthalmological),
		renal=as.factor(renal),
		genitourinary=as.factor(genitourinary),
		otologic=as.factor(otologic),
		metabolic=as.factor(metabolic),
		hematological=as.factor(hematological),
		genetic=as.factor(genetic),
		gastrointestinal=as.factor(gastrointestinal),
		craniofacial=as.factor(craniofacial),
		endocrinological=as.factor(endocrinological),
		malignancy=as.factor(malignancy),
		dermatological=as.factor(dermatological)) %>% 
	(function(x){glm(SUPERUSER ~ musculoskeletal + mental_health + immunological + otolaryngological + pulmonary_respiratory + neurological + cardiac + ophthalmological + renal + genitourinary + otologic + metabolic + hematological + genetic + gastrointestinal + craniofacial + endocrinological + malignancy + dermatological, 
		family="binomial", data=x)})

fig_superlogistic <- superlogisticdf %>% 
	tidy() %>% 
	filter(term!="(Intercept)") %>% 
	ungroup() %>% 
	mutate(SIGNIFICANT=case_when(p.value<(0.05/n())~1, TRUE~0)) %>% 
	ggplot(aes(x=-log(p.value), y=estimate, label=term, alpha=SIGNIFICANT)) + 
		geom_point() + 
		geom_text(hjust=-.1) + 
		theme_classic() +
		scale_x_continuous(limits=c(-50, 1500))  +
		scale_alpha(range=c(0.1,1)) + 
		labs(x="-log(p value)",y="Log-odds difference", alpha="")
# ggsave(fig_superlogistic, file="figures/under5/superlogistic.pdf",width=8,height=5)

ccnames <- tibble(BODY_SYSTEM=c("pulmonary_respiratory",
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
	"craniofacial"),
NAME=c("Pulmonary/Respiratory",
	"Cardiac",
	"Renal",
	"Genitourinary",
	"Gastrointestinal",
	"Dermatological",
	"Hematological",
	"Musculoskeletal",
	"Immunological",
	"Metabolic",
	"Genetic",
	"Neurological",
	"Malignancy",
	"Ophthalmological",
	"Otolaryngological",
	"Endocrinological",
	"Mental health",
	"Otologic",
	"Craniofacial"))

superlogistictab <- superlogisticdf %>% 
	tidy() %>% 
	mutate(estimate=exp(estimate)) %>% 	
	mutate(mlogp=-log(p.value)) %>% 
	arrange(desc(estimate)) %>% 
	filter(term!="(Intercept)") %>% 
	select(term, RelOdds=estimate, p.value) %>% 
	left_join(ccnames %>% 
			rename(term=BODY_SYSTEM) %>% 
			mutate(term=paste0(term,"1")), by="term") %>% 
	select(NAME, RelOdds, p.value) %>% 
	mutate(p.value.lab=case_when(
		p.value<0.0001~"p < 0.0001",
		p.value<0.001~"p < 0.001",
		p.value<0.01~"p < 0.01",
		p.value<0.05~"p < 0.05",
		TRUE~"N.S."
		))

write_csv(superlogistictab, file="figures/superlogistictab.csv")

# ==============================================================================
# Geography
# ==============================================================================

rx_summ_msa_resp <- visit_df[,.(ASSOC_VISIT_ID=ID,COND)][
	rx_df, on=.(ASSOC_VISIT_ID)][
	COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media")][
	MSA %in% bigmsalist
	,.(ENROLID,STATE,MSA,MSA_POP,MSA_NAME,ISABX,ID,WEIGHT_INDIV_MSAGROUP_NOYEAR)][
	,.(NRX=sum(WEIGHT_INDIV_MSAGROUP_NOYEAR),STATE=first(STATE),MSA_POP=first(MSA_POP), MSA_NAME=first(MSA_NAME)),by=.(MSA,ISABX)] %>% 
	as_tibble() %>% 
	arrange(MSA,ISABX) %>%
	makeHHS %>% 
	select(HHS, STATE, MSA, MSA_POP, MSA_NAME, ISABX, NRX) %>% 
	filter(ISABX==1)

rx_summ_msa_nonresp <- visit_df[,.(ASSOC_VISIT_ID=ID,COND)][
	rx_df, on=.(ASSOC_VISIT_ID)][
	!(COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media"))][
	MSA %in% bigmsalist
	,.(ENROLID,STATE,MSA,MSA_POP,MSA_NAME,ISABX,ID,WEIGHT_INDIV_MSAGROUP_NOYEAR)][
	,.(NRX=sum(WEIGHT_INDIV_MSAGROUP_NOYEAR),STATE=first(STATE),MSA_POP=first(MSA_POP), MSA_NAME=first(MSA_NAME)),by=.(MSA,ISABX)] %>% 
	as_tibble() %>% 
	arrange(MSA,ISABX) %>%
	makeHHS %>% 
	select(HHS, STATE, MSA, MSA_POP, MSA_NAME, ISABX, NRX) %>% 
	filter(ISABX==1)

write_csv(rx_summ_msa_resp, path="figures/rx_summ_msa_resp.csv")
write_csv(rx_summ_msa_nonresp, path="figures/rx_summ_msa_nonresp.csv")

first_rx_msa_resp <- visit_df[,.(ASSOC_VISIT_ID=ID,COND)][
	rx_df, on=.(ASSOC_VISIT_ID)][
	COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media")][
	MSA %in% bigmsalist][
	,AGE_DAYS:=as.numeric(difftime(DATE,BIRTH_DATE,units="days"))][
	,.(AGE_DAYS=min(AGE_DAYS)), by=.(ENROLID,ISABX)][
	rbind(
		memb_df[MSA %in% bigmsalist,.(ENROLID, STATE, MSA, MSA_POP, MSA_NAME, WEIGHT_INDIV_MSAGROUP_NOYEAR, ISABX=0)],
		memb_df[MSA %in% bigmsalist,.(ENROLID, STATE, MSA, MSA_POP, MSA_NAME, WEIGHT_INDIV_MSAGROUP_NOYEAR, ISABX=1)]
		), on=.(ENROLID, ISABX)][
	is.na(AGE_DAYS), AGE_DAYS:=Inf] %>% 
	as_tibble() %>% 
	filter(ISABX==1)

first_rx_msa_nonresp <- visit_df[,.(ASSOC_VISIT_ID=ID,COND)][
	rx_df, on=.(ASSOC_VISIT_ID)][
	!(COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media"))][
	MSA %in% bigmsalist][
	,AGE_DAYS:=as.numeric(difftime(DATE,BIRTH_DATE,units="days"))][
	,.(AGE_DAYS=min(AGE_DAYS)), by=.(ENROLID,ISABX)][
	rbind(
		memb_df[MSA %in% bigmsalist,.(ENROLID, STATE, MSA, MSA_POP, MSA_NAME, WEIGHT_INDIV_MSAGROUP_NOYEAR, ISABX=0)],
		memb_df[MSA %in% bigmsalist,.(ENROLID, STATE, MSA, MSA_POP, MSA_NAME, WEIGHT_INDIV_MSAGROUP_NOYEAR, ISABX=1)]
		), on=.(ENROLID, ISABX)][
	is.na(AGE_DAYS), AGE_DAYS:=Inf] %>% 
	as_tibble() %>% 
	filter(ISABX==1)

write_csv(first_rx_msa_resp, path="figures/first_rx_msa_resp.csv")
write_csv(first_rx_msa_nonresp, path="figures/first_rx_msa_nonresp.csv")

