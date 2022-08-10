library(tidyverse) 
library(icd) 

# See https://www.kpwashingtonresearch.org/our-research/our-scientists/rita-mangione-smith-md-mph/measurement-tools-research-dr-rita-mangione-smith

chronic_priorities <- tibble(BODY_SYSTEM=c("pulmonary_respiratory",
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
	"craniofacial")) %>% 
	mutate(PRIORITY=1:n())

chronicconddf <- bind_rows(
	read_csv("data/pmca_dx_code_lists_i9.csv", col_types=list(col_character(), col_character(), col_character(), col_character(), col_character())) %>%
	mutate(DX=as.character(decimal_to_short(dx))) %>% 
	rename(BODY_SYSTEM=body_system) %>% 
	select("DX","BODY_SYSTEM") %>% 
	mutate(ICD="9"), 
	read_csv("data/pmca_dx_code_lists_i10.csv", col_types=list(col_character(), col_character(), col_character(), col_character(), col_character())) %>%
	mutate(DX=as.character(decimal_to_short(dx))) %>% 
	rename(BODY_SYSTEM=body_system) %>% 
	select("DX","BODY_SYSTEM") %>% 
	mutate(ICD="0")
	) %>% 
	mutate(BODY_SYSTEM=case_when(
		BODY_SYSTEM=="mental health"~"mental_health",
		BODY_SYSTEM=="pulmonary/respiratory"~"pulmonary_respiratory",
		TRUE~BODY_SYSTEM)) %>% 
	left_join(chronic_priorities, by="BODY_SYSTEM") %>% 
	group_by(DX, ICD) %>% 
	arrange(PRIORITY) %>% 
	slice(1) %>% 
	select(-PRIORITY)

write_csv(chronicconddf, file="data/letter/chronicconddf.csv")