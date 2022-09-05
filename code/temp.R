
# =============================================================================
# Cumulative prescriptions for superusers: 
# =============================================================================

memb_df_superuser <- memb_df[ENROLID %in% superuserdf$ENROLID]
rx_df_superuser <- rx_df[ENROLID %in% superuserdf$ENROLID]
visit_df_superuser <- visit_df[ENROLID %in% superuserdf$ENROLID]

# Count cumulative prescriptions across all conditions: 
cumrx_df_superuser <- setDT(data.frame())
for(t in seq(from=0, to=1825, by=5)){
	cumrx_df_superuser <- rbind(cumrx_df_superuser,
		rx_df_superuser[AGE_DAYS<=t,.(NOBS=.N, NRX=.N/nrow(memb_df_superuser),AGE_DAYS_ROUNDED=t)])
}
cumrx_df_superuser <- cumrx_df_superuser[,NMEMB:=nrow(memb_df_superuser)]

# Count cumulative prescriptions for respiratory conditions: 
rx_df_superuser_resp <- visit_df_superuser[,.(ASSOC_VISIT_ID=ID,COND)][
	rx_df_superuser, on=.(ASSOC_VISIT_ID)][
	COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media")]

cumrx_df_superuser_resp <- setDT(data.frame())
for(t in seq(from=0, to=1825, by=5)){
	cumrx_df_superuser_resp <- rbind(cumrx_df_superuser_resp,
		rx_df_superuser_resp[AGE_DAYS<=t,.(NOBS=.N, NRX=.N/nrow(memb_df_superuser),AGE_DAYS_ROUNDED=t)])
}
cumrx_df_superuser_resp <- cumrx_df_superuser_resp[,NMEMB:=nrow(memb_df_superuser)]

# Count cumulative prescriptions for non-respiratory conditions: 
rx_df_superuser_nonresp <- visit_df_superuser[,.(ASSOC_VISIT_ID=ID,COND)][
	rx_df_superuser, on=.(ASSOC_VISIT_ID)][
	!(COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media"))]

cumrx_df_superuser_nonresp <- setDT(data.frame())
for(t in seq(from=0, to=1825, by=5)){
	cumrx_df_superuser_nonresp <- rbind(cumrx_df_superuser_nonresp,
		rx_df_superuser_nonresp[AGE_DAYS<=t,.(NOBS=.N, NRX=.N/nrow(memb_df_superuser),AGE_DAYS_ROUNDED=t)])
}
cumrx_df_superuser_nonresp <- cumrx_df_superuser_nonresp[,NMEMB:=nrow(memb_df_superuser)]


# Gather the data into a useful format and append confidence intervals: 
fulldat_cumrx_superuser <- cumrx_df_superuser %>% 
	mutate(rawmean=NOBS/NMEMB) %>% 
	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
	mutate(lwr=NRX-proplwr*NRX) %>% 
	mutate(upr=NRX+propupr*NRX) 

respdat_cumrx_superuser <- cumrx_df_superuser_resp %>% 
	mutate(rawmean=NOBS/NMEMB) %>% 
	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
	mutate(lwr=NRX-proplwr*NRX) %>% 
	mutate(upr=NRX+propupr*NRX) 

nonrespdat_cumrx_superuser <- cumrx_df_superuser_nonresp %>% 
	mutate(rawmean=NOBS/NMEMB) %>% 
	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
	mutate(lwr=NRX-proplwr*NRX) %>% 
	mutate(upr=NRX+propupr*NRX) 

combdat_cumrx_superuser <- bind_rows(
	mutate(fulldat_cumrx_superuser,Indication="All conditions"),
	mutate(respdat_cumrx_superuser,Indication="Respiratory conditions"),
	mutate(nonrespdat_cumrx_superuser,Indication="Non-respiratory conditions")
	)

# Plot cumulative prescriptions overall and by respiratory/non-respiratory conditions: 
fig_cumrx_respnonresp_superuser <-
	ggplot(data=combdat_cumrx_superuser, aes(x=AGE_DAYS_ROUNDED, y=NRX, col=Indication, fill=Indication, linetype=Indication)) + 
		geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=0.4) + 
		geom_line() + 
		scale_x_continuous(breaks=seq(from=0, to=1825, by=365)) + 
		theme_classic() + 
		labs(tag="A)", x="Days from birth", y=paste0("Cumulative prescriptions")) + 
		scale_color_manual(values=c("All conditions"="Gray","Respiratory conditions"="Blue","Non-respiratory conditions"="Blue")) + 
		scale_fill_manual(values=c("All conditions"="Gray","Respiratory conditions"="Blue","Non-respiratory conditions"="Blue")) +
		scale_linetype_manual(values=c("All conditions"="solid","Respiratory conditions"="solid","Non-respiratory conditions"="dashed")) +
		theme(text=element_text(size=10))

ggsave(fig_cumrx_respnonresp_superuser, file="figures/cumrx.pdf", width=figwidth, height=figwidth, dpi=figres)
ggsave(fig_cumrx_respnonresp_superuser, file="figures/cumrx.png", width=figwidth, height=figwidth, dpi=figres)

ggsave(fig_cumrx_respnonresp_superuser + theme(legend.position='none'), file="figures/cumrx_nokey.pdf", width=figwidth, height=figwidth, dpi=figres)
ggsave(fig_cumrx_respnonresp_superuser + theme(legend.position='none'), file="figures/cumrx_nokey.png", width=figwidth, height=figwidth, dpi=figres)

# Extract prescriptions by age 5: 
combdat_cumrx_superuser %>% 
	filter(AGE_DAYS_ROUNDED==1825) %>% 
	select(AGE_DAYS_ROUNDED, Indication, NRX, lwr, upr)
