superuser_ids <- rxrankdf_full %>% 
	filter(SUPERUSER==1) %>% 
	pull(ENROLID) %>% 
	unique()

chronic_ids <- ccmap %>% 
	mutate(has_chronic=case_when(
		(pulmonary_respiratory==1 | otologic==1 | immunological==1)~1, 
		TRUE~0)) %>% 
	filter(has_chronic==1) %>% 
	pull(ENROLID) %>% 
	unique()

# =============================================================================
# Cumulative prescriptions for superusers: 
# =============================================================================

memb_df_chronic <- memb_df[ENROLID %in% chronic_ids]
rx_df_chronic <- rx_df[ENROLID %in% chronic_ids]
visit_df_chronic <- visit_df[ENROLID %in% chronic_ids]

memb_df_nonchronic <- memb_df[!(ENROLID %in% chronic_ids)]
rx_df_nonchronic <- rx_df[!(ENROLID %in% chronic_ids)]
visit_df_nonchronic <- visit_df[!(ENROLID %in% chronic_ids)]

# Count cumulative prescriptions for people with chronic conditions: 
rx_df_chronic <- visit_df_chronic[,.(ASSOC_VISIT_ID=ID,COND)][
	rx_df_chronic, on=.(ASSOC_VISIT_ID)]

cumrx_df_chronic <- setDT(data.frame())
for(t in seq(from=0, to=1825, by=5)){
	cumrx_df_chronic <- rbind(cumrx_df_chronic,
		rx_df_chronic[AGE_DAYS<=t,.(NOBS=.N, NRX=.N/nrow(memb_df_chronic),AGE_DAYS_ROUNDED=t)])
}
cumrx_df_chronic <- cumrx_df_chronic[,NMEMB:=nrow(memb_df_chronic)]

# Count cumulative prescriptions for people with nonchronic conditions: 
rx_df_nonchronic <- visit_df_nonchronic[,.(ASSOC_VISIT_ID=ID,COND)][
	rx_df_nonchronic, on=.(ASSOC_VISIT_ID)]

cumrx_df_nonchronic <- setDT(data.frame())
for(t in seq(from=0, to=1825, by=5)){
	cumrx_df_nonchronic <- rbind(cumrx_df_nonchronic,
		rx_df_nonchronic[AGE_DAYS<=t,.(NOBS=.N, NRX=.N/nrow(memb_df_nonchronic),AGE_DAYS_ROUNDED=t)])
}
cumrx_df_nonchronic <- cumrx_df_nonchronic[,NMEMB:=nrow(memb_df_nonchronic)]


part1_cumrx_chronic <- cumrx_df_chronic %>% 
	mutate(rawmean=NOBS/NMEMB) %>% 
	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
	mutate(lwr=NRX-proplwr*NRX) %>% 
	mutate(upr=NRX+propupr*NRX) 

part2_cumrx_chronic <- cumrx_df_nonchronic %>% 
	mutate(rawmean=NOBS/NMEMB) %>% 
	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
	mutate(lwr=NRX-proplwr*NRX) %>% 
	mutate(upr=NRX+propupr*NRX) 

combdat_cumrx_chronic <- bind_rows(
	mutate(part1_cumrx_chronic,Comorbidities="Yes"),
	mutate(part2_cumrx_chronic,Comorbidities="No")
	)

write_csv(combdat_cumrx_chronic, file="underlying_data/combdat_cumrx_chronic.csv")

# Plot cumulative prescriptions overall and by respiratory/non-respiratory conditions: 
fig_cumrx_chronic <-
	ggplot(data=combdat_cumrx_chronic, aes(x=AGE_DAYS_ROUNDED, y=NRX, col=Comorbidities, fill=Comorbidities, linetype=Comorbidities)) + 
		geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=0.4) + 
		geom_line() + 
		scale_x_continuous(breaks=seq(from=0, to=1825, by=365)) + 
		theme_classic() + 
		labs(tag="A)", x="Days from birth", y=paste0("Cumulative antibiotic prescriptions")) + 
		scale_color_manual(values=c("Yes"="Red","No"="Black")) + 
		scale_fill_manual(values=c("Yes"="Red","No"="Black")) +
		scale_linetype_manual(values=c("Yes"="solid","No"="solid")) +
		theme(text=element_text(size=10))

ggsave(fig_cumrx_chronic, file="figures/cumrx_chronic.pdf", width=figwidth, height=figwidth, dpi=figres)
ggsave(fig_cumrx_chronic, file="figures/cumrx_chronic.png", width=figwidth, height=figwidth, dpi=figres)
ggsave(fig_cumrx_chronic, file="figures/cumrx_chronic.tiff", width=figwidth, height=figwidth, dpi=figres)

ggsave(fig_cumrx_chronic + theme(legend.position='none'), file="figures/cumrx_chronic_nokey.pdf", width=figwidth, height=figwidth, dpi=figres)
ggsave(fig_cumrx_chronic + theme(legend.position='none'), file="figures/cumrx_chronic_nokey.png", width=figwidth, height=figwidth, dpi=figres)
ggsave(fig_cumrx_chronic + theme(legend.position='none'), file="figures/cumrx_chronic_nokey.tiff", width=figwidth, height=figwidth, dpi=figres)

# Extract prescriptions by age 5: 
combdat_cumrx_chronic %>% 
	filter(AGE_DAYS_ROUNDED==1825) %>% 
	select(AGE_DAYS_ROUNDED, Comorbidities, NRX, lwr, upr)

# ==============================================================================
# Time to first prescription
# ==============================================================================

# Get the age of first prescription for those with chronic conditions: 
firstrx_df_chronic <- visit_df_chronic[,.(ASSOC_VISIT_ID=ID,COND)][
	rx_df_chronic, on=.(ASSOC_VISIT_ID)][
	,.(WEIGHT_INDIV_NOYEAR=first(WEIGHT_INDIV_NOYEAR), AGE_DAYS=min(AGE_DAYS)), by=.(ENROLID)]

cumfirstrx_df_chronic <- setDT(data.frame())
for(t in seq(from=0, to=1825, by=5)){
	cumfirstrx_df_chronic <- rbind(
		cumfirstrx_df_chronic,
		firstrx_df_chronic[AGE_DAYS<=t,.(NOBS=.N,NRX=.N/nrow(memb_df_chronic),AGE_DAYS_ROUNDED=t)])
}
cumfirstrx_df_chronic <- cumfirstrx_df_chronic[,NMEMB:=nrow(memb_df_chronic)]


# Get the age of first prescription for those without chronic conditions: 
firstrx_df_nonchronic <- visit_df_nonchronic[,.(ASSOC_VISIT_ID=ID,COND)][
	rx_df_nonchronic, on=.(ASSOC_VISIT_ID)][
	,.(WEIGHT_INDIV_NOYEAR=first(WEIGHT_INDIV_NOYEAR), AGE_DAYS=min(AGE_DAYS)), by=.(ENROLID)]

cumfirstrx_df_nonchronic <- setDT(data.frame())
for(t in seq(from=0, to=1825, by=5)){
	cumfirstrx_df_nonchronic <- rbind(
		cumfirstrx_df_nonchronic,
		firstrx_df_nonchronic[AGE_DAYS<=t,.(NOBS=.N,NRX=.N/nrow(memb_df_nonchronic),AGE_DAYS_ROUNDED=t)])
}

cumfirstrx_df_nonchronic <- cumfirstrx_df_nonchronic[,NMEMB:=nrow(memb_df_nonchronic)]

# Gather the data into a useful format and append confidence intervals: 
fulldat_firstcumrx_chronic <- cumfirstrx_df_chronic %>% 
	mutate(rawmean=NOBS/NMEMB) %>% 
	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
	mutate(lwr=NRX-proplwr*NRX) %>% 
	mutate(upr=NRX+propupr*NRX) 

part1_firstcumrx_chronic <- cumfirstrx_df_chronic %>% 
	mutate(rawmean=NOBS/NMEMB) %>% 
	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
	mutate(lwr=NRX-proplwr*NRX) %>% 
	mutate(upr=NRX+propupr*NRX) 

part2_firstcumrx_chronic <- cumfirstrx_df_nonchronic %>% 
	mutate(rawmean=NOBS/NMEMB) %>% 
	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
	mutate(lwr=NRX-proplwr*NRX) %>% 
	mutate(upr=NRX+propupr*NRX) 

combdat_firstcumrx_chronic <- bind_rows(
	mutate(part1_firstcumrx_chronic,Comorbidities="Yes"),
	mutate(part2_firstcumrx_chronic,Comorbidities="No")
	) 

write_csv(combdat_firstcumrx_chronic, file="underlying_data/combdat_firstcumrx_chronic.csv")

# Plot age at first prescriptions overall and by respiratory/non-respiratory conditions: 
fig_cumfirstrx_chronic <-
	ggplot(data=combdat_firstcumrx_chronic, aes(x=AGE_DAYS_ROUNDED, y=NRX, col=Comorbidities, fill=Comorbidities, linetype=Comorbidities)) + 
		geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=0.4) + 
		geom_line() + 
		scale_x_continuous(breaks=seq(from=0, to=1825, by=365)) + 
		scale_y_continuous(limits=c(0,1), breaks=seq(from=0, to=1, by=0.2)) + 
		theme_classic() + 
		labs(tag="B)",x="Days from birth", y=paste0("Proportion who have received\nan antibiotic prescription")) + 
		scale_color_manual(values=c("Yes"="Red","No"="Black")) + 
		scale_fill_manual(values=c("Yes"="Red","No"="Black")) +
		scale_linetype_manual(values=c("Yes"="solid","No"="solid")) +
		theme(text=element_text(size=10))

ggsave(fig_cumfirstrx_chronic, file="figures/cumfirstrx_chronic.pdf", width=figwidth, height=figwidth, dpi=figres)
ggsave(fig_cumfirstrx_chronic, file="figures/cumfirstrx_chronic.png", width=figwidth, height=figwidth, dpi=figres)
ggsave(fig_cumfirstrx_chronic, file="figures/cumfirstrx_chronic.tiff", width=figwidth, height=figwidth, dpi=figres)

ggsave(fig_cumfirstrx_chronic + theme(legend.position='none'), file="figures/cumfirstrx_chronic_nokey.pdf", width=figwidth, height=figwidth, dpi=figres)
ggsave(fig_cumfirstrx_chronic + theme(legend.position='none'), file="figures/cumfirstrx_chronic_nokey.png", width=figwidth, height=figwidth, dpi=figres)
ggsave(fig_cumfirstrx_chronic + theme(legend.position='none'), file="figures/cumfirstrx_chronic_nokey.tiff", width=figwidth, height=figwidth, dpi=figres)

# Extract prescriptions by age 1: 
combdat_firstcumrx_chronic %>% 
	filter(AGE_DAYS_ROUNDED==365) %>% 
	select(AGE_DAYS_ROUNDED, Comorbidities, NRX, lwr, upr)

# Extract prescriptions by age 5: 
combdat_firstcumrx_chronic %>% 
	filter(AGE_DAYS_ROUNDED==1825) %>% 
	select(AGE_DAYS_ROUNDED, Comorbidities, NRX, lwr, upr)



# ==============================================================================
# Chronic/non-Chronic histogram
# ==============================================================================

# Count total prescriptions per person for kids with chronic conditions:
cumrx_summ_chronic <- rx_df_chronic[
	,.(ENROLID,ISRX=1)][
	memb_df_chronic[,.(ENROLID)], on=.(ENROLID)][
	is.na(ISRX),ISRX:=0][
	,.(NRX=sum(ISRX)),by=.(ENROLID)] %>% 
	as_tibble() %>% 
	mutate(Comorbidities="Yes")

# Count total prescriptions per person for kids without chronic conditions:
cumrx_summ_nonchronic <- rx_df_nonchronic[
	,.(ENROLID,ISRX=1)][
	memb_df_nonchronic[,.(ENROLID)], on=.(ENROLID)][
	is.na(ISRX),ISRX:=0][
	,.(NRX=sum(ISRX)),by=.(ENROLID)] %>% 
	as_tibble() %>% 
	mutate(Comorbidities="No")

# Combine counts into a single data frame: 
cumrx_summ_chronic <- rbind(cumrx_summ_chronic, cumrx_summ_nonchronic) %>% 
	ungroup() %>% 
	select(-ENROLID)

write_csv(cumrx_summ_chronic, file="underlying_data/cumrx_summ_chronic.csv")

# Plot cumulative prescriptions by condition group as densities: 
fig_cumrx_summ_chronic <- cumrx_summ_chronic %>% 
	ggplot(aes(x=NRX, fill=Comorbidities, col=Comorbidities, lty=Comorbidities)) + 
		geom_density(adjust=5, alpha=0.4) + 
		scale_x_continuous(limits=c(0,50)) + 
		scale_color_manual(values=c("Yes"="Red","No"="Black")) + 
		scale_fill_manual(values=c("Yes"="Red","No"="Black")) +
		scale_linetype_manual(values=c("Yes"="solid","No"="solid")) +
		theme_classic() + 
		theme(text=element_text(size=10)) + 
		labs(tag="C)", x="Number of antibiotic prescriptions", y="Proportion of children (as density)")


ggsave(fig_cumrx_summ_chronic, file="figures/cumrx_summ_chronic.pdf", width=figwidth, height=figwidth, dpi=figres)
ggsave(fig_cumrx_summ_chronic, file="figures/cumrx_summ_chronic.png", width=figwidth, height=figwidth, dpi=figres)
ggsave(fig_cumrx_summ_chronic, file="figures/cumrx_summ_chronic.tiff", width=figwidth, height=figwidth, dpi=figres)

ggsave(fig_cumrx_summ_chronic + theme(legend.position='none'), file="figures/cumrx_summ_chronic_nokey.pdf", width=figwidth, height=figwidth, dpi=figres)
ggsave(fig_cumrx_summ_chronic + theme(legend.position='none'), file="figures/cumrx_summ_chronic_nokey.png", width=figwidth, height=figwidth, dpi=figres)
ggsave(fig_cumrx_summ_chronic + theme(legend.position='none'), file="figures/cumrx_summ_chronic_nokey.tiff", width=figwidth, height=figwidth, dpi=figres)

