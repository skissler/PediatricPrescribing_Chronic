library(tidyverse) 

msadat <- read_csv("data/msadat_raw_2011.csv") %>% 
	filter(LSAD %in% c("Metropolitan Statistical Area","Metropolitan Division")) %>% 
	select(CBSA, MDIV, LSAD, NAME, ESTIMATESBASE2010) %>% 
	mutate(MSA=case_when(LSAD=="Metropolitan Statistical Area"~CBSA, TRUE~MDIV)) %>% 
	rename(MSA_NAME=NAME) %>% 
	rename(MSA_POP=ESTIMATESBASE2010) %>% 
	select(MSA, MSA_NAME, MSA_POP)

write_csv(msadat, file="data/msadat.csv")