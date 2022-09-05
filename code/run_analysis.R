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
# system('sas code/extract_data/extract_data.sas')
# source('code/get_popsizes_under5.R')
# source('code/make_chronic.R')

# Data import:
source('code/import_fulldata.R')
source('code/reduce_under5.R')

figwidth <- 3.2
figheight <- 3.2*10/16
figres <- 600

# =============================================================================
# Figure 1A: Cumulative prescriptions
# =============================================================================

# Count cumulative prescriptions across all conditions: 
cumrx_df <- setDT(data.frame())
for(t in seq(from=0, to=1825, by=5)){
	cumrx_df <- rbind(cumrx_df,
		rx_df[AGE_DAYS<=t,.(NOBS=.N, NRX=sum(WEIGHT_INDIV_NOYEAR),AGE_DAYS_ROUNDED=t)])
}
cumrx_df <- cumrx_df[,NMEMB:=nrow(memb_df)]

# Count cumulative prescriptions for respiratory conditions: 
rx_df_resp <- visit_df[,.(ASSOC_VISIT_ID=ID,COND)][
	rx_df, on=.(ASSOC_VISIT_ID)][
	COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media")]

cumrx_df_resp <- setDT(data.frame())
for(t in seq(from=0, to=1825, by=5)){
	cumrx_df_resp <- rbind(cumrx_df_resp,
		rx_df_resp[AGE_DAYS<=t,.(NOBS=.N, NRX=sum(WEIGHT_INDIV_NOYEAR),AGE_DAYS_ROUNDED=t)])
}
cumrx_df_resp <- cumrx_df_resp[,NMEMB:=nrow(memb_df)]

# Count cumulative prescriptions for non-respiratory conditions: 
rx_df_nonresp <- visit_df[,.(ASSOC_VISIT_ID=ID,COND)][
	rx_df, on=.(ASSOC_VISIT_ID)][
	!(COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media"))]

cumrx_df_nonresp <- setDT(data.frame())
for(t in seq(from=0, to=1825, by=5)){
	cumrx_df_nonresp <- rbind(cumrx_df_nonresp,
		rx_df_nonresp[AGE_DAYS<=t,.(NOBS=.N, NRX=sum(WEIGHT_INDIV_NOYEAR),AGE_DAYS_ROUNDED=t)])
}
cumrx_df_nonresp <- cumrx_df_nonresp[,NMEMB:=nrow(memb_df)]


# Gather the data into a useful format and append confidence intervals: 
fulldat_cumrx <- cumrx_df %>% 
	mutate(rawmean=NOBS/NMEMB) %>% 
	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
	mutate(lwr=NRX-proplwr*NRX) %>% 
	mutate(upr=NRX+propupr*NRX) 

respdat_cumrx <- cumrx_df_resp %>% 
	mutate(rawmean=NOBS/NMEMB) %>% 
	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
	mutate(lwr=NRX-proplwr*NRX) %>% 
	mutate(upr=NRX+propupr*NRX) 

nonrespdat_cumrx <- cumrx_df_nonresp %>% 
	mutate(rawmean=NOBS/NMEMB) %>% 
	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
	mutate(lwr=NRX-proplwr*NRX) %>% 
	mutate(upr=NRX+propupr*NRX) 

combdat_cumrx <- bind_rows(
	mutate(fulldat_cumrx,Indication="All conditions"),
	mutate(respdat_cumrx,Indication="Respiratory conditions"),
	mutate(nonrespdat_cumrx,Indication="Non-respiratory conditions")
	)

# Plot cumulative prescriptions overall and by respiratory/non-respiratory conditions: 
fig_cumrx_respnonresp <-
	ggplot(data=combdat_cumrx, aes(x=AGE_DAYS_ROUNDED, y=NRX, col=Indication, fill=Indication, linetype=Indication)) + 
		geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=0.4) + 
		geom_line() + 
		scale_x_continuous(breaks=seq(from=0, to=1825, by=365)) + 
		theme_classic() + 
		labs(tag="A)", x="Days from birth", y=paste0("Cumulative prescriptions")) + 
		scale_color_manual(values=c("All conditions"="Gray","Respiratory conditions"="Blue","Non-respiratory conditions"="Blue")) + 
		scale_fill_manual(values=c("All conditions"="Gray","Respiratory conditions"="Blue","Non-respiratory conditions"="Blue")) +
		scale_linetype_manual(values=c("All conditions"="solid","Respiratory conditions"="solid","Non-respiratory conditions"="dashed")) +
		theme(text=element_text(size=10))

ggsave(fig_cumrx_respnonresp, file="figures/cumrx.pdf", width=figwidth, height=figwidth, dpi=figres)
ggsave(fig_cumrx_respnonresp, file="figures/cumrx.png", width=figwidth, height=figwidth, dpi=figres)

ggsave(fig_cumrx_respnonresp + theme(legend.position='none'), file="figures/cumrx_nokey.pdf", width=figwidth, height=figwidth, dpi=figres)
ggsave(fig_cumrx_respnonresp + theme(legend.position='none'), file="figures/cumrx_nokey.png", width=figwidth, height=figwidth, dpi=figres)

# Extract prescriptions by age 5: 
combdat_cumrx %>% 
	filter(AGE_DAYS_ROUNDED==1825) %>% 
	select(AGE_DAYS_ROUNDED, Indication, NRX, lwr, upr)

# Calculate prescribing rates between key ages: 
# 1) Birth to 6mos (180 days): 
rxrate1 <- combdat_cumrx %>% 
	filter(Indication=="All conditions") %>% 
	filter(AGE_DAYS_ROUNDED==180) %>% 
	select(NRX,lwr,upr) %>% 
	mutate(NRX=NRX*2, lwr=lwr*2, upr=upr*2)


# 2) 6 mos (180 days) to 2 years (730 days): 
rxrate2 <- combdat_cumrx %>% 
	filter(Indication=="All conditions") %>% 
	filter(AGE_DAYS_ROUNDED%in%c(180,730)) %>% 
	select(AGE_DAYS_ROUNDED,NRX,lwr,upr) %>% 
	mutate(AGE_DAYS_ROUNDED=AGE_DAYS_ROUNDED-lag(AGE_DAYS_ROUNDED),
		NRX=NRX-lag(NRX),
		lwr=lwr-lag(lwr),
		upr=upr-lag(upr)) %>% 
	filter(!is.na(AGE_DAYS_ROUNDED)) %>% 
	mutate(NRX=NRX/AGE_DAYS_ROUNDED*365,
		lwr=lwr/AGE_DAYS_ROUNDED*365,
		upr=upr/AGE_DAYS_ROUNDED*365) %>% 
	select(-AGE_DAYS_ROUNDED)

# 3) 2 years (730 days) to 5 years (1825 days): 
rxrate3 <- combdat_cumrx %>% 
	filter(Indication=="All conditions") %>% 
	filter(AGE_DAYS_ROUNDED%in%c(730,1825)) %>% 
	select(AGE_DAYS_ROUNDED,NRX,lwr,upr) %>% 
	mutate(AGE_DAYS_ROUNDED=AGE_DAYS_ROUNDED-lag(AGE_DAYS_ROUNDED),
		NRX=NRX-lag(NRX),
		lwr=lwr-lag(lwr),
		upr=upr-lag(upr)) %>% 
	filter(!is.na(AGE_DAYS_ROUNDED)) %>% 
	mutate(NRX=NRX/AGE_DAYS_ROUNDED*365,
		lwr=lwr/AGE_DAYS_ROUNDED*365,
		upr=upr/AGE_DAYS_ROUNDED*365) %>% 
	select(-AGE_DAYS_ROUNDED)	

# =============================================================================
# Figure 1B: Time to first prescription
# =============================================================================

# Get the age of first prescription for any condition: 
firstrx_df <- rx_df[
	,.(WEIGHT_INDIV_NOYEAR=first(WEIGHT_INDIV_NOYEAR), AGE_DAYS=min(AGE_DAYS)), by=.(ENROLID)]

cumfirstrx_df <- setDT(data.frame())
for(t in seq(from=0, to=1825, by=5)){
	cumfirstrx_df <- rbind(cumfirstrx_df,
		firstrx_df[AGE_DAYS<=t,.(NOBS=.N,NRX=sum(WEIGHT_INDIV_NOYEAR),AGE_DAYS_ROUNDED=t)])
}
cumfirstrx_df <- cumfirstrx_df[,NMEMB:=nrow(memb_df)]


# Get the age of first prescription for a respiratory condition: 
firstrx_df_resp <- visit_df[,.(ASSOC_VISIT_ID=ID,COND)][
	rx_df, on=.(ASSOC_VISIT_ID)][
	COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media")][
	,.(WEIGHT_INDIV_NOYEAR=first(WEIGHT_INDIV_NOYEAR), AGE_DAYS=min(AGE_DAYS)), by=.(ENROLID)]

cumfirstrx_df_resp <- setDT(data.frame())
for(t in seq(from=0, to=1825, by=5)){
	cumfirstrx_df_resp <- rbind(cumfirstrx_df_resp,
		firstrx_df_resp[AGE_DAYS<=t,.(NOBS=.N,NRX=sum(WEIGHT_INDIV_NOYEAR),AGE_DAYS_ROUNDED=t)])
}
cumfirstrx_df_resp <- cumfirstrx_df_resp[,NMEMB:=nrow(memb_df)]


# Get the age of first prescription for a non-respiratory condition: 
firstrx_df_nonresp <- visit_df[,.(ASSOC_VISIT_ID=ID,COND)][
	rx_df, on=.(ASSOC_VISIT_ID)][
	!(COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media"))][
	,.(WEIGHT_INDIV_NOYEAR=first(WEIGHT_INDIV_NOYEAR), AGE_DAYS=min(AGE_DAYS)), by=.(ENROLID)]

cumfirstrx_df_nonresp <- setDT(data.frame())
for(t in seq(from=0, to=1825, by=5)){
	cumfirstrx_df_nonresp <- rbind(cumfirstrx_df_nonresp,
		firstrx_df_nonresp[AGE_DAYS<=t,.(NOBS=.N,NRX=sum(WEIGHT_INDIV_NOYEAR),AGE_DAYS_ROUNDED=t)])
}
cumfirstrx_df_nonresp <- cumfirstrx_df_nonresp[,NMEMB:=nrow(memb_df)]


# Gather the data into a useful format and append confidence intervals: 
fulldat_firstcumrx <- cumfirstrx_df %>% 
	mutate(rawmean=NOBS/NMEMB) %>% 
	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
	mutate(lwr=NRX-proplwr*NRX) %>% 
	mutate(upr=NRX+propupr*NRX) 

respdat_firstcumrx <- cumfirstrx_df_resp %>% 
	mutate(rawmean=NOBS/NMEMB) %>% 
	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
	mutate(lwr=NRX-proplwr*NRX) %>% 
	mutate(upr=NRX+propupr*NRX) 

nonrespdat_firstcumrx <- cumfirstrx_df_nonresp %>% 
	mutate(rawmean=NOBS/NMEMB) %>% 
	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
	mutate(lwr=NRX-proplwr*NRX) %>% 
	mutate(upr=NRX+propupr*NRX) 

combdat_firstcumrx <- bind_rows(
	mutate(fulldat_firstcumrx,Indication="All conditions"),
	mutate(respdat_firstcumrx,Indication="Respiratory conditions"),
	mutate(nonrespdat_firstcumrx,Indication="Non-respiratory conditions")
	) 

# Plot age at first prescriptions overall and by respiratory/non-respiratory conditions: 
fig_cumfirstrx_respnonresp <-
	ggplot(data=combdat_firstcumrx, aes(x=AGE_DAYS_ROUNDED, y=NRX, col=Indication, fill=Indication, linetype=Indication)) + 
		geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=0.4) + 
		geom_line() + 
		scale_x_continuous(breaks=seq(from=0, to=1825, by=365)) + 
		scale_y_continuous(limits=c(0,1), breaks=seq(from=0, to=1, by=0.2)) + 
		theme_classic() + 
		labs(tag="B)",x="Days from birth", y=paste0("Proportion who have received a prescription")) + 
		scale_color_manual(values=c("All conditions"="Gray","Respiratory conditions"="Blue","Non-respiratory conditions"="Blue")) + 
		scale_fill_manual(values=c("All conditions"="Gray","Respiratory conditions"="Blue","Non-respiratory conditions"="Blue")) +
		scale_linetype_manual(values=c("All conditions"="solid","Respiratory conditions"="solid","Non-respiratory conditions"="dashed")) +
		theme(text=element_text(size=10))

ggsave(fig_cumfirstrx_respnonresp, file="figures/firstrx.pdf", width=figwidth, height=figwidth, dpi=figres)
ggsave(fig_cumfirstrx_respnonresp, file="figures/firstrx.png", width=figwidth, height=figwidth, dpi=figres)

ggsave(fig_cumfirstrx_respnonresp+theme(legend.position="none"), file="figures/firstrx_nokey.pdf", width=figwidth, height=figwidth, dpi=figres)
ggsave(fig_cumfirstrx_respnonresp+theme(legend.position="none"), file="figures/firstrx_nokey.png", width=figwidth, height=figwidth, dpi=figres)

# Extract proportion of children who have received a prescription by age 5: 
combdat_firstcumrx %>% 	
	filter(AGE_DAYS_ROUNDED==1825) %>% 
	select(AGE_DAYS_ROUNDED, Indication, NRX, lwr, upr)

# ==============================================================================
# Figure 1C: Prescribing histogram
# ==============================================================================

cumrx_summ_overall <- rx_df[
	,.(ENROLID,ISRX=1)][
	memb_df[,.(ENROLID)], on=.(ENROLID)][
	is.na(ISRX),ISRX:=0][
	,.(NRX=sum(ISRX)),by=.(ENROLID)] %>% 
	as_tibble() %>% 
	mutate(Indication="All conditions")

cumrx_summ_resp <- rx_df_resp[
	,.(ENROLID,ISRX=1)][
	memb_df[,.(ENROLID)], on=.(ENROLID)][
	is.na(ISRX),ISRX:=0][
	,.(NRX=sum(ISRX)),by=.(ENROLID)] %>% 
	as_tibble() %>% 
	mutate(Indication="Respiratory infections")

cumrx_summ_nonresp <- rx_df_nonresp[
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

rxrankdf_overall_abx <- rx_df[,.(ENROLID,ISABX,ID)][
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

rxrankdf_overall_abx %>% filter(CUMPROPRX>0.5)
rxrankdf_overall_abx %>% filter(PRANK_WEIGHTED>0.2)

rxrankdf_resp_abx <- visit_df[,.(ASSOC_VISIT_ID=ID,COND)][
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


rxrankdf_nonresp_abx <- visit_df[,.(ASSOC_VISIT_ID=ID,COND)][
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
	mutate(rxrankdf_overall_abx,COND="All conditions"),
	mutate(rxrankdf_resp_abx,COND="Respiratory conditions"),
	mutate(rxrankdf_nonresp_abx,COND="Non-respiratory conditions"),
	)


fig_rankcurve_respnonresp <- rxrankdf_abx_combined %>% 
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

ggsave(fig_rankcurve_respnonresp, file="figures/rankcurve.pdf", width=figwidth, height=figwidth, dpi=figres)
ggsave(fig_rankcurve_respnonresp, file="figures/rankcurve.png", width=figwidth, height=figwidth, dpi=figres)

ggsave(fig_rankcurve_respnonresp+theme(legend.position="none"), file="figures/rankcurve_nokey.pdf", width=figwidth, height=figwidth, dpi=figres)
ggsave(fig_rankcurve_respnonresp+theme(legend.position="none"), file="figures/rankcurve_nokey.png", width=figwidth, height=figwidth, dpi=figres)

# ==============================================================================
# Chronic condition analysis
# ==============================================================================

superuserdf <- rxrankdf_overall_abx %>% 
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

rxrankdf_full <-  rxrankdf_overall_abx %>% 
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

write_csv(rx_summ_msa_resp, file="figures/rx_summ_msa_resp.csv")
write_csv(rx_summ_msa_nonresp, file="figures/rx_summ_msa_nonresp.csv")

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

write_csv(first_rx_msa_resp, file="figures/first_rx_msa_resp.csv")
write_csv(first_rx_msa_nonresp, file="figures/first_rx_msa_nonresp.csv")

