
# Get the age of first prescription for any condition: 
firstrx_df_chronic <- rx_df_chronic[
	,.(WEIGHT_INDIV_NOYEAR=first(WEIGHT_INDIV_NOYEAR), AGE_DAYS=min(AGE_DAYS)), by=.(ENROLID)]

cumfirstrx_df_chronic <- setDT(data.frame())
for(t in seq(from=0, to=1825, by=5)){
	cumfirstrx_df_chronic <- rbind(cumfirstrx_df_chronic,
		firstrx_df_chronic[AGE_DAYS<=t,.(NOBS=.N,NRX=.N/nrow(memb_df_chronic),AGE_DAYS_ROUNDED=t)])
}
cumfirstrx_df_chronic <- cumfirstrx_df_chronic[,NMEMB:=nrow(memb_df_chronic)]


# Get the age of first prescription for a respiratory condition: 
firstrx_df_chronic_resp <- visit_df_chronic[,.(ASSOC_VISIT_ID=ID,COND)][
	rx_df_chronic, on=.(ASSOC_VISIT_ID)][
	COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media")][
	,.(WEIGHT_INDIV_NOYEAR=first(WEIGHT_INDIV_NOYEAR), AGE_DAYS=min(AGE_DAYS)), by=.(ENROLID)]

cumfirstrx_df_chronic_resp <- setDT(data.frame())
for(t in seq(from=0, to=1825, by=5)){
	cumfirstrx_df_chronic_resp <- rbind(cumfirstrx_df_chronic_resp,
		firstrx_df_chronic_resp[AGE_DAYS<=t,.(NOBS=.N,NRX=.N/nrow(memb_df_chronic),AGE_DAYS_ROUNDED=t)])
}
cumfirstrx_df_chronic_resp <- cumfirstrx_df_chronic_resp[,NMEMB:=nrow(memb_df_chronic)]


# Get the age of first prescription for a non-respiratory condition: 
firstrx_df_chronic_nonresp <- visit_df_chronic[,.(ASSOC_VISIT_ID=ID,COND)][
	rx_df_chronic, on=.(ASSOC_VISIT_ID)][
	!(COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media"))][
	,.(WEIGHT_INDIV_NOYEAR=first(WEIGHT_INDIV_NOYEAR), AGE_DAYS=min(AGE_DAYS)), by=.(ENROLID)]

cumfirstrx_df_chronic_nonresp <- setDT(data.frame())
for(t in seq(from=0, to=1825, by=5)){
	cumfirstrx_df_chronic_nonresp <- rbind(cumfirstrx_df_chronic_nonresp,
		firstrx_df_chronic_nonresp[AGE_DAYS<=t,.(NOBS=.N,NRX=.N/nrow(memb_df_chronic),AGE_DAYS_ROUNDED=t)])
}
cumfirstrx_df_chronic_nonresp <- cumfirstrx_df_chronic_nonresp[,NMEMB:=nrow(memb_df_chronic)]


# Gather the data into a useful format and append confidence intervals: 
fulldat_firstcumrx_chronic <- cumfirstrx_df_chronic %>% 
	mutate(rawmean=NOBS/NMEMB) %>% 
	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
	mutate(lwr=NRX-proplwr*NRX) %>% 
	mutate(upr=NRX+propupr*NRX) 

respdat_firstcumrx_chronic <- cumfirstrx_df_chronic_resp %>% 
	mutate(rawmean=NOBS/NMEMB) %>% 
	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
	mutate(lwr=NRX-proplwr*NRX) %>% 
	mutate(upr=NRX+propupr*NRX) 

nonrespdat_firstcumrx_chronic <- cumfirstrx_df_chronic_nonresp %>% 
	mutate(rawmean=NOBS/NMEMB) %>% 
	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
	mutate(lwr=NRX-proplwr*NRX) %>% 
	mutate(upr=NRX+propupr*NRX) 

combdat_firstcumrx_chronic <- bind_rows(
	mutate(fulldat_firstcumrx_chronic,Indication="All conditions"),
	mutate(respdat_firstcumrx_chronic,Indication="Respiratory conditions"),
	mutate(nonrespdat_firstcumrx_chronic,Indication="Non-respiratory conditions")
	) 

# Plot age at first prescriptions overall and by respiratory/non-respiratory conditions: 
fig_cumfirstrx_respnonresp <-
	ggplot(data=combdat_firstcumrx_chronic, aes(x=AGE_DAYS_ROUNDED, y=NRX, col=Indication, fill=Indication, linetype=Indication)) + 
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

# ggsave(fig_cumfirstrx_respnonresp, file="figures/firstrx.pdf", width=figwidth, height=figwidth, dpi=figres)
# ggsave(fig_cumfirstrx_respnonresp, file="figures/firstrx.png", width=figwidth, height=figwidth, dpi=figres)

# ggsave(fig_cumfirstrx_respnonresp+theme(legend.position="none"), file="figures/firstrx_nokey.pdf", width=figwidth, height=figwidth, dpi=figres)
# ggsave(fig_cumfirstrx_respnonresp+theme(legend.position="none"), file="figures/firstrx_nokey.png", width=figwidth, height=figwidth, dpi=figres)

fig_cumfirstrx_comparison_chronic <-
	ggplot(data=bind_rows(mutate(fulldat_firstcumrx,Comorbidities="No"), mutate(fulldat_firstcumrx_chronic,Comorbidities="Yes")), aes(x=AGE_DAYS_ROUNDED, y=NRX, col=Indication, fill=Indication, linetype=Indication)) + 
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