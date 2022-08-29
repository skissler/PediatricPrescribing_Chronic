library(tidyverse) 
library(broom)
library(purrr) 
library(lubridate) 
library(scales) 
library(tidycensus)
source('code/utils.R')
source('code/utils_private.R')

census_api_key(censuskey)
popsizes_under5 <- get_acs(
	geography="state", 
	variables=c(M_under_5="B01001_003",F_under_5="B01001_027"),
	year=2010) %>% 
	select(-GEOID, -moe) %>% 
	rename(STATE="NAME") %>% 
	rename(POPSIZE="estimate") %>% 
	mutate(SEX=case_when(substr(variable,1,1)=="M"~1,TRUE~2)) %>% 
	select(STATE, SEX, POPSIZE) %>% 
	filter(STATE!="Puerto Rico") %>%
	mutate(STATE=case_when(STATE=="District of Columbia"~"Washington DC", TRUE~STATE)) # %>% 
	# mutate(WEIGHT=POPSIZE/sum(POPSIZE))

write_csv(popsizes_under5, file="data/popsizes_under5.csv")

