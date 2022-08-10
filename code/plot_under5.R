library(tidyverse) 
library(ggbeeswarm)
library(tidycensus)
library(sf)
library(scales) 
source('code/utils.R')
source('code/utils_private.R')
census_api_key(censuskey)

figwidth <- 3.2
figheight <- 3.2*10/16
figres <- 600

rx_summ_msa_resp <- read_csv("figures/resp/rx_summ_msa_resp.csv")
rx_summ_msa_nonresp <- read_csv("figures/resp/rx_summ_msa_nonresp.csv")

rx_summ_msa <- bind_rows(
	mutate(rx_summ_msa_resp, COND="Respiratory"),
	mutate(rx_summ_msa_nonresp, COND="Non-respiratory")
	)

first_rx_msa_resp <- read_csv("figures/resp/first_rx_msa_resp.csv")
first_rx_msa_nonresp <- read_csv("figures/resp/first_rx_msa_nonresp.csv")

first_rx_msa <- bind_rows(
	mutate(first_rx_msa_resp, COND="Respiratory"),
	mutate(first_rx_msa_nonresp, COND="Non-respiratory")
	)

# ==============================================================================
# Beeswarms: 
# ==============================================================================

# Cumulative prescriptions: ----------------------------------------------------
ypos_msa <- tibble(
	MSA=c(35644, 31084, 16974, 26420, 12060, 42644), 
	MSA_NAME_SHORT=c("New York","Los Angeles","Chicago","Houston","Atlanta","Seattle"),
	ypos=c(4,2,3,5,6,1))

labeldf <- rx_summ_msa %>% 
	filter(MSA %in% c(35644, 31084, 16974, 26420, 12060, 42644)) %>% 
	mutate(xfrom=-(ISABX-2)) %>% 
	left_join(ypos_msa,by=c("MSA"))

bootstrap.weighted.mean <- function(x,w,n){
	dropvec <- sample(1:length(x),size=n,replace=TRUE)
	out <- unlist(lapply(dropvec, function(todrop){weighted.mean(x[-todrop],w[-todrop])}))
	return(out)
}

rx_summ_msa_means <- rx_summ_msa %>% 
	makeHHS() %>% 
	mutate(REGION=case_when(
		HHS%in%c(1,2,3)~"Northeast",
		HHS%in%c(4,6)~"South",
		HHS%in%c(5,7,8)~"Mid/Mountain West",
		HHS%in%c(9,10)~"Pacific West"
		)) %>% 
	split(.$COND) %>% 
	map(~ split(., .$REGION)) %>% 
	map_depth(2, ~ bootstrap.weighted.mean(.$NRX, .$MSA_POP, 10000)) %>% 
	map(~ bind_rows(.)) %>% 
	map(~ pivot_longer(., everything(), names_to="REGION", values_to="NRX")) %>%
	bind_rows(.id="COND") %>% 
	group_by(COND, REGION) %>% 
	summarise(
		MEAN_BS=mean(NRX), 
		LWR_BS=quantile(NRX,0.025), 
		UPR_BS=quantile(NRX,0.975)) %>% 
	mutate(xval=case_when(COND=="Respiratory"~0.1, COND=="Non-respiratory"~2.6)) %>% 
	mutate(xdiff=case_when(COND=="Respiratory"~0.15, COND=="Non-respiratory"~-0.15)) %>% 
	group_by(COND)

rx_summ_msa_backbone <- rx_summ_msa_means %>% 
	group_by(COND) %>% 
	summarise(NRXMIN=min(MEAN_BS), NRXMAX=max(MEAN_BS), xval=first(xval))

fig_rx_summ_msa_pointsize <- rx_summ_msa %>% 
	mutate(REGION=case_when(
		HHS%in%c(1,2,3)~"Northeast",
		HHS%in%c(4,6)~"South",
		HHS%in%c(5,7,8)~"Mid/Mountain West",
		HHS%in%c(9,10)~"Pacific West"
		)) %>% 
	ggplot() + 
		geom_beeswarm(aes(x=factor(COND, levels=c("Respiratory","Non-respiratory")), y=NRX, col=REGION, size=MSA_POP), alpha=0.8, cex=1.5) + 
		scale_color_brewer(type="qual", palette=3) + 
		scale_fill_brewer(type="qual", palette=3) + 
		# geom_rect(data=rx_summ_msa_means, aes(xmin=xval,xmax=xval+xdiff,ymin=LWR_BS,ymax=UPR_BS,fill=REGION), alpha=0.6) + 
		# geom_segment(data=rx_summ_msa_backbone, aes(x=xval,xend=xval,y=NRXMIN, yend=NRXMAX), col="darkgrey", size=0.8, alpha=0.8) + 
		theme_classic() + 
		theme(text=element_text(size=10)) + 
		labs(tag="A)",x=element_blank(), y="Mean antibiotic prescriptions\nper person by age 5")

ggsave(fig_rx_summ_msa_pointsize, file="figures/resp/rx_summ_msa.pdf", width=figwidth, height=figwidth, dpi=figres)
ggsave(fig_rx_summ_msa_pointsize, file="figures/resp/rx_summ_msa.png", width=figwidth, height=figwidth, dpi=figres)

ggsave(fig_rx_summ_msa_pointsize + theme(legend.position="none"), file="figures/resp/rx_summ_msa_nokey.pdf", width=figwidth, height=figwidth, dpi=figres)
ggsave(fig_rx_summ_msa_pointsize + theme(legend.position="none"), file="figures/resp/rx_summ_msa_nokey.png", width=figwidth, height=figwidth, dpi=figres)


# Proportion who have received at least one prescription: ----------------------
first_rx_df <- first_rx_msa %>% 
	filter(AGE_DAYS<=30*12*5) %>% 
	group_by(COND, MSA) %>% 
	summarise(NRX=sum(WEIGHT_INDIV_MSAGROUP_NOYEAR), MSA_POP=first(MSA_POP), MSA_NAME=first(MSA_NAME), STATE=first(STATE))

firstrx_msa_means <- first_rx_df %>% 
	makeHHS() %>% 
	mutate(REGION=case_when(
		HHS%in%c(1,2,3)~"Northeast",
		HHS%in%c(4,6)~"South",
		HHS%in%c(5,7,8)~"Mid/Mountain West",
		HHS%in%c(9,10)~"Pacific West"
		)) %>% 
	split(.$COND) %>% 
	map(~ split(., .$REGION)) %>% 
	map_depth(2, ~ bootstrap.weighted.mean(.$NRX, .$MSA_POP, 10000)) %>% 
	map(~ bind_rows(.)) %>% 
	map(~ pivot_longer(., everything(), names_to="REGION", values_to="NRX")) %>%
	bind_rows(.id="COND") %>% 
	group_by(COND, REGION) %>% 
	summarise(
		MEAN_BS=mean(NRX), 
		LWR_BS=quantile(NRX,0.025), 
		UPR_BS=quantile(NRX,0.975)) %>% 
	mutate(xval=case_when(COND=="Respiratory"~0.1, COND=="Non-respiratory"~2.6)) %>% 
	mutate(xdiff=case_when(COND=="Respiratory"~0.15, COND=="Non-respiratory"~-0.15)) %>% 
	group_by(COND)

firstrx_msa_backbone <- firstrx_msa_means %>% 
	group_by(COND) %>% 
	summarise(NRXMIN=min(MEAN_BS), NRXMAX=max(MEAN_BS), xval=first(xval))

fig_first_rx_msa <- first_rx_df %>% 
	makeHHS() %>% 
	mutate(REGION=case_when(
		HHS%in%c(1,2,3)~"Northeast",
		HHS%in%c(4,6)~"South",
		HHS%in%c(5,7,8)~"Mid/Mountain West",
		HHS%in%c(9,10)~"Pacific West"
		)) %>% 
	ggplot() + 
		geom_beeswarm(aes(x=factor(COND, levels=c("Respiratory","Non-respiratory")), y=NRX, col=REGION, size=MSA_POP), alpha=0.8, cex=2) + 
		scale_color_brewer(type="qual", palette=3) + 
		scale_fill_brewer(type="qual", palette=3) + 
		# geom_rect(data=firstrx_msa_means, aes(xmin=xval,xmax=xval+xdiff,ymin=LWR_BS,ymax=UPR_BS,fill=REGION), alpha=0.6) + 
		# geom_segment(data=firstrx_msa_backbone, aes(x=xval,xend=xval,y=NRXMIN, yend=NRXMAX), col="darkgrey", size=0.8, alpha=0.8) + 
		theme_classic() + 
		theme(text=element_text(size=10)) + 
		labs(tag="B)",x=element_blank(), y="Proportion of children who have received\nat least one prescription by age 5")

ggsave(fig_first_rx_msa, file="figures/resp/first_rx_msa.pdf", width=figwidth, height=figwidth, dpi=figres)
ggsave(fig_first_rx_msa, file="figures/resp/first_rx_msa.png", width=figwidth, height=figwidth, dpi=figres)

ggsave(fig_first_rx_msa + theme(legend.position="none"), file="figures/resp/first_rx_msa_nokey.pdf", width=figwidth, height=figwidth, dpi=figres)
ggsave(fig_first_rx_msa + theme(legend.position="none"), file="figures/resp/first_rx_msa_nokey.png", width=figwidth, height=figwidth, dpi=figres)
