# Count total prescriptions per person:
cumrx_summ_overall <- rx_df[
	,.(ENROLID,ISRX=1)][
	memb_df[,.(ENROLID)], on=.(ENROLID)][
	is.na(ISRX),ISRX:=0][
	,.(NRX=sum(ISRX)),by=.(ENROLID)] %>% 
	as_tibble() %>% 
	mutate(Indication="All conditions")

# Count total prescriptions per person for respiratory conditions:
cumrx_summ_resp <- rx_df_resp[
	,.(ENROLID,ISRX=1)][
	memb_df[,.(ENROLID)], on=.(ENROLID)][
	is.na(ISRX),ISRX:=0][
	,.(NRX=sum(ISRX)),by=.(ENROLID)] %>% 
	as_tibble() %>% 
	mutate(Indication="Respiratory infections")

# Count total prescriptions per person for non-respiratory conditions:
cumrx_summ_nonresp <- rx_df_nonresp[
	,.(ENROLID,ISRX=1)][
	memb_df[,.(ENROLID)], on=.(ENROLID)][
	is.na(ISRX),ISRX:=0][
	,.(NRX=sum(ISRX)),by=.(ENROLID)] %>% 
	as_tibble() %>% 
	mutate(Indication="Other conditions")

# Combine counts into a single data frame: 
cumrx_summ <- rbind(cumrx_summ_overall, cumrx_summ_resp, cumrx_summ_nonresp)

# Plot cumulative prescriptions by condition group as densities: 
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
		labs(tag="C)", x="Number of antibiotic prescriptions", y="Proportion of children (as density)")

ggsave(fig_cumrx_summ, file="figures/cumrx_summ.pdf",width=figwidth, height=figwidth,dpi=figres)
ggsave(fig_cumrx_summ, file="figures/cumrx_summ.png",width=figwidth, height=figwidth,dpi=figres)

ggsave(fig_cumrx_summ + theme(legend.position="none"), file="figures/cumrx_summ_nokey.pdf",width=figwidth, height=figwidth,dpi=figres)
ggsave(fig_cumrx_summ + theme(legend.position="none"), file="figures/cumrx_summ_nokey.png",width=figwidth, height=figwidth,dpi=figres)