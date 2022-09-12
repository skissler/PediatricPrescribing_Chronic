# ==============================================================================
# Asymmetry in prescribing
# ==============================================================================

# Count and rank total prescriptions per child, from most to least: 
rxrankdf_overall <- rx_df[,.(ENROLID,ID)][
	memb_df,
	on=.(ENROLID)][
	!is.na(ID),NRX:=1][
	is.na(ID),NRX:=0][
	,.(NRX=sum(NRX),WEIGHT_INDIV_NOYEAR=first(WEIGHT_INDIV_NOYEAR)),by=.(ENROLID)] %>% 
	as_tibble() %>% 
	shuffledf() %>% 
	arrange(desc(NRX)) %>% 
	mutate(TOTRX=sum(NRX)) %>% 
	mutate(PROPRX=NRX/TOTRX) %>% 
	mutate(CUMPROPRX=cumsum(PROPRX)) %>% 
	select(-TOTRX, -PROPRX) %>% 
	mutate(RANK=1:n()) %>% 
	mutate(PRANK=RANK/max(RANK)) %>% 
	mutate(PRANK_WEIGHTED=cumsum(WEIGHT_INDIV_NOYEAR)) %>% 
	ungroup()

# What proportion of children (starting from those who receive the most prescriptions) is responsible for 50% of antibiotic prescriptions? 
rxrankdf_overall %>% filter(CUMPROPRX>0.5)
rxrankdf_overall %>% filter(PRANK_WEIGHTED>0.2)

# Count and rank total prescriptions per child associated with respiratory conditions, from most to least: 
rxrankdf_resp <- visit_df[,.(ASSOC_VISIT_ID=ID,COND)][
	rx_df, on=.(ASSOC_VISIT_ID)][
	COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media")][
	,.(ENROLID,ID)][
	memb_df,
	on=.(ENROLID)][
	!is.na(ID),NRX:=1][
	is.na(ID),NRX:=0][
	,.(NRX=sum(NRX),WEIGHT_INDIV_NOYEAR=first(WEIGHT_INDIV_NOYEAR)),by=.(ENROLID)] %>% 
	as_tibble() %>% 
	shuffledf() %>% 
	arrange(desc(NRX)) %>% 
	mutate(TOTRX=sum(NRX)) %>% 
	mutate(PROPRX=NRX/TOTRX) %>% 
	mutate(CUMPROPRX=cumsum(PROPRX)) %>% 
	select(-TOTRX, -PROPRX) %>% 
	mutate(RANK=1:n()) %>% 
	mutate(PRANK=RANK/max(RANK)) %>% 
	mutate(PRANK_WEIGHTED=cumsum(WEIGHT_INDIV_NOYEAR)) %>% 
	ungroup()

# Count and rank total prescriptions per child associated with non-respiratory conditions, from most to least: 
rxrankdf_nonresp <- visit_df[,.(ASSOC_VISIT_ID=ID,COND)][
	rx_df, on=.(ASSOC_VISIT_ID)][
	!(COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media"))][
	,.(ENROLID,ID)][
	memb_df,
	on=.(ENROLID)][
	!is.na(ID),NRX:=1][
	is.na(ID),NRX:=0][
	,.(NRX=sum(NRX),WEIGHT_INDIV_NOYEAR=first(WEIGHT_INDIV_NOYEAR)),by=.(ENROLID)] %>% 
	as_tibble() %>% 
	shuffledf() %>% 
	arrange(desc(NRX)) %>% 
	mutate(TOTRX=sum(NRX)) %>% 
	mutate(PROPRX=NRX/TOTRX) %>% 
	mutate(CUMPROPRX=cumsum(PROPRX)) %>% 
	select(-TOTRX, -PROPRX) %>% 
	mutate(RANK=1:n()) %>% 
	mutate(PRANK=RANK/max(RANK)) %>% 
	mutate(PRANK_WEIGHTED=cumsum(WEIGHT_INDIV_NOYEAR)) %>% 
	ungroup() 

# Combine the ranking data frames across all condition classes: 
rxrankdf_combined <- bind_rows(
	mutate(rxrankdf_overall,COND="All conditions"),
	mutate(rxrankdf_resp,COND="Respiratory conditions"),
	mutate(rxrankdf_nonresp,COND="Non-respiratory conditions"),
	)

# Plot the Lorenz curves for prescriptions by condition class: 
fig_rankcurve_respnonresp <- rxrankdf_combined %>% 
	ggplot(aes(x=PRANK_WEIGHTED, y=CUMPROPRX, col=COND, lty=COND)) + 
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
