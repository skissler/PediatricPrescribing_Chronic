
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
