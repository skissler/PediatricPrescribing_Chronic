
# ==============================================================================
# Chronic condition analysis
# ==============================================================================


# Append chronic conditions to the ranked prescribing data frame:
rxrankdf_full <-  rxrankdf_overall %>% 
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

# Run a logistic regression to calculate odds difference in being a super-user by chronic condition:
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

# Plot p/effect for the super-user regression:
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

# Clean up chronic condition names:
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

# Save a table of increase in log-odds of being a superuser by chronic condition:
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
