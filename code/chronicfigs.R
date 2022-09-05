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

# Count cumulative prescriptions across all conditions: 
cumrx_df_chronic <- setDT(data.frame())
for(t in seq(from=0, to=1825, by=5)){
	cumrx_df_chronic <- rbind(cumrx_df_chronic,
		rx_df_chronic[AGE_DAYS<=t,.(NOBS=.N, NRX=.N/nrow(memb_df_chronic),AGE_DAYS_ROUNDED=t)])
}
cumrx_df_chronic <- cumrx_df_chronic[,NMEMB:=nrow(memb_df_chronic)]

# Count cumulative prescriptions for respiratory conditions: 
rx_df_chronic_resp <- visit_df_chronic[,.(ASSOC_VISIT_ID=ID,COND)][
	rx_df_chronic, on=.(ASSOC_VISIT_ID)][
	COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media")]

cumrx_df_chronic_resp <- setDT(data.frame())
for(t in seq(from=0, to=1825, by=5)){
	cumrx_df_chronic_resp <- rbind(cumrx_df_chronic_resp,
		rx_df_chronic_resp[AGE_DAYS<=t,.(NOBS=.N, NRX=.N/nrow(memb_df_chronic),AGE_DAYS_ROUNDED=t)])
}
cumrx_df_chronic_resp <- cumrx_df_chronic_resp[,NMEMB:=nrow(memb_df_chronic)]

# Count cumulative prescriptions for non-respiratory conditions: 
rx_df_chronic_nonresp <- visit_df_chronic[,.(ASSOC_VISIT_ID=ID,COND)][
	rx_df_chronic, on=.(ASSOC_VISIT_ID)][
	!(COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media"))]

cumrx_df_chronic_nonresp <- setDT(data.frame())
for(t in seq(from=0, to=1825, by=5)){
	cumrx_df_chronic_nonresp <- rbind(cumrx_df_chronic_nonresp,
		rx_df_chronic_nonresp[AGE_DAYS<=t,.(NOBS=.N, NRX=.N/nrow(memb_df_chronic),AGE_DAYS_ROUNDED=t)])
}
cumrx_df_chronic_nonresp <- cumrx_df_chronic_nonresp[,NMEMB:=nrow(memb_df_chronic)]


# Gather the data into a useful format and append confidence intervals: 
fulldat_cumrx_chronic <- cumrx_df_chronic %>% 
	mutate(rawmean=NOBS/NMEMB) %>% 
	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
	mutate(lwr=NRX-proplwr*NRX) %>% 
	mutate(upr=NRX+propupr*NRX) 

respdat_cumrx_chronic <- cumrx_df_chronic_resp %>% 
	mutate(rawmean=NOBS/NMEMB) %>% 
	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
	mutate(lwr=NRX-proplwr*NRX) %>% 
	mutate(upr=NRX+propupr*NRX) 

nonrespdat_cumrx_chronic <- cumrx_df_chronic_nonresp %>% 
	mutate(rawmean=NOBS/NMEMB) %>% 
	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
	mutate(lwr=NRX-proplwr*NRX) %>% 
	mutate(upr=NRX+propupr*NRX) 

combdat_cumrx_chronic <- bind_rows(
	mutate(fulldat_cumrx_chronic,Indication="All conditions"),
	mutate(respdat_cumrx_chronic,Indication="Respiratory conditions"),
	mutate(nonrespdat_cumrx_chronic,Indication="Non-respiratory conditions")
	)

# Plot cumulative prescriptions overall and by respiratory/non-respiratory conditions: 
fig_cumrx_respnonresp_chronic <-
	ggplot(data=combdat_cumrx_chronic, aes(x=AGE_DAYS_ROUNDED, y=NRX, col=Indication, fill=Indication, linetype=Indication)) + 
		geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=0.4) + 
		geom_line() + 
		scale_x_continuous(breaks=seq(from=0, to=1825, by=365)) + 
		theme_classic() + 
		labs(tag="A)", x="Days from birth", y=paste0("Cumulative prescriptions")) + 
		scale_color_manual(values=c("All conditions"="Gray","Respiratory conditions"="Blue","Non-respiratory conditions"="Blue")) + 
		scale_fill_manual(values=c("All conditions"="Gray","Respiratory conditions"="Blue","Non-respiratory conditions"="Blue")) +
		scale_linetype_manual(values=c("All conditions"="solid","Respiratory conditions"="solid","Non-respiratory conditions"="dashed")) +
		theme(text=element_text(size=10))

# ggsave(fig_cumrx_respnonresp_chronic, file="figures/cumrx.pdf", width=figwidth, height=figwidth, dpi=figres)
# ggsave(fig_cumrx_respnonresp_chronic, file="figures/cumrx.png", width=figwidth, height=figwidth, dpi=figres)

# ggsave(fig_cumrx_respnonresp_chronic + theme(legend.position='none'), file="figures/cumrx_nokey.pdf", width=figwidth, height=figwidth, dpi=figres)
# ggsave(fig_cumrx_respnonresp_chronic + theme(legend.position='none'), file="figures/cumrx_nokey.png", width=figwidth, height=figwidth, dpi=figres)

fig_cumrx_comparison_chronic <-
	ggplot(data=bind_rows(mutate(fulldat_cumrx,CLASS="NONCHRONIC"), mutate(fulldat_cumrx_chronic,class="CHRONIC")), aes(x=AGE_DAYS_ROUNDED, y=NRX, col=CLASS, fill=CLASS, linetype=CLASS)) + 
		geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=0.4) + 
		geom_line() + 
		scale_x_continuous(breaks=seq(from=0, to=1825, by=365)) + 
		theme_classic() + 
		labs(tag="A)", x="Days from birth", y=paste0("Cumulative prescriptions")) + 
		scale_color_manual(values=c("NONCHRONIC"="Gray","CHRONIC"="Red")) + 
		scale_fill_manual(values=c("NONCHRONIC"="Gray","CHRONIC"="Red")) +
		scale_linetype_manual(values=c("NONCHRONIC"="solid","CHRONIC"="solid")) +
		theme(text=element_text(size=10))
