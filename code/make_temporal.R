
cumfun <- function(x){
	cumrx_df <- setDT(data.frame())
	x <- setDT(x)
	for(t in seq(from=0, to=1825, by=5)){
		cumrx_df <- rbind(cumrx_df,
			x[AGE_DAYS<=t,.(NOBS=.N, NRX=sum(WEIGHT_INDIV),AGE_DAYS_ROUNDED=t)])
	}
	cumrx_df <- as_tibble(cumrx_df[,NMEMB:=nrow(memb_df)])
	return(cumrx_df)
}

cumrx_df_birthyear <- rx_df %>% 
	as_tibble() %>% 
	split(.$BIRTH_YEAR) %>% 
	map(~ cumfun(.)) %>% 
	bind_rows(.id="BIRTH_YEAR")

# Gather the data into a useful format and append confidence intervals: 
combdat_cumrx_birthyear <- cumrx_df_birthyear %>% 
	group_by(BIRTH_YEAR) %>% 
	mutate(rawmean=NOBS/NMEMB) %>% 
	mutate(rawlwr=qgamma(alphasig/2,NOBS,1)/NMEMB) %>%
	mutate(rawupr=qgamma(1-alphasig/2,NOBS+1,1)/NMEMB) %>%
	mutate(proplwr=(rawmean-rawlwr)/rawmean) %>% 
	mutate(propupr=(rawupr-rawmean)/rawmean) %>% 
	mutate(lwr=NRX-proplwr*NRX) %>% 
	mutate(upr=NRX+propupr*NRX) 

write_csv(combdat_cumrx_birthyear, file="underlying_data/combdat_cumrx_birthyear.csv")

# Plot cumulative prescriptions overall and by respiratory/non-respiratory conditions: 
fig_cumrx_birthyear <-
	ggplot(data=combdat_cumrx_birthyear, aes(x=AGE_DAYS_ROUNDED, y=NRX, col=factor(BIRTH_YEAR), fill=factor(BIRTH_YEAR))) + 
		geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=0.4) + 
		geom_line() + 
		scale_x_continuous(breaks=seq(from=0, to=1825, by=365)) + 
		theme_classic() + 
		labs(x="Days from birth", y=paste0("Cumulative antibiotic prescriptions")) + 
		scale_color_manual(values=c("darkblue","blue","cornflowerblue","pink","orange","darkred")) + 
		scale_fill_manual(values=c("darkblue","blue","cornflowerblue","pink","orange","darkred")) +
		theme(text=element_text(size=10))

ggsave(fig_cumrx_birthyear, file="figures/cumrx_birthyear.pdf", width=figwidth, height=figwidth, dpi=figres)
ggsave(fig_cumrx_birthyear, file="figures/cumrx_birthyear.png", width=figwidth, height=figwidth, dpi=figres)

ggsave(fig_cumrx_birthyear + theme(legend.position='none'), file="figures/cumrx_birthyear_nokey.pdf", width=figwidth, height=figwidth, dpi=figres)
ggsave(fig_cumrx_birthyear + theme(legend.position='none'), file="figures/cumrx_birthyear_nokey.png", width=figwidth, height=figwidth, dpi=figres)

combdat_cumrx_birthyear %>% 
	filter(AGE_DAYS_ROUNDED==1825) %>% 
	select(BIRTH_YEAR, AGE_DAYS_ROUNDED, NRX, lwr, upr)

# BIRTH_YEAR AGE_DAYS_ROUNDED   NRX   lwr   upr
# <chr>                 <dbl> <dbl> <dbl> <dbl>
# 2008                   1825  7.50  7.47  7.54
# 2009                   1825  7.15  7.12  7.18
# 2010                   1825  6.87  6.83  6.90
# 2011                   1825  6.53  6.50  6.57
# 2012                   1825  6.30  6.26  6.33
# 2013                   1825  6.05  6.01  6.09
