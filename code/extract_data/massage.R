
library(lubridate)

memb_df <- memb_df_raw

# Assign an index to each month of membership that counts from birth month:
memb_df <- memb_df[,INDEX:=12*(DT_YEAR-BIRTH_YEAR)+(DT_MONTH-BIRTH_MONTH)]
# Restrict to indices from the month after birth through end of enrollment:
# memb_df <- memb_df[INDEX>=min(index_df$INDEX) & INDEX<=max(index_df$INDEX)]

# sam: 1098006 recs
# Find the first missing month for each person: 
memb_df <- memb_df[,CENSORINDEX:=min(setdiff(c(0,index_df$INDEX,61,62,63,64,65,66,67,68.69,70,Inf), .SD$INDEX)), by=.(ENROLID)]
# sam: 1098006 recs
# Cut off all months past the censor month:
memb_df <- memb_df[INDEX>=0 & INDEX<CENSORINDEX]

temp <- memb_df %>% 
	as_tibble() %>% 
	select(ENROLID, BIRTH_DATE, STATE, DTEND, INDEX) %>% 
	arrange(ENROLID, INDEX)

print(temp, n=50)

# ==============================================================

sasdf <- read_csv('output/cohort_intermediate_SAS.csv')
sasdf <- sasdf %>% select(ENROLID, BIRTH_DATE, STATE, DTEND, INDEX=BIRTHDIFF) %>% 
 	mutate(BIRTH_DATE=mdy(BIRTH_DATE)) %>% 
 	mutate(DTEND=mdy(DTEND))


temp %>% 
	left_join(sasdf, by=c("ENROLID","BIRTH_DATE","DTEND")) %>% 
	filter(is.na(INDEX.y)) %>% 
	print(n=Inf)
