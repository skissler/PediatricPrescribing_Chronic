

memb_df <- memb_df_raw

# Assign an index to each month of membership that counts from birth month:
memb_df <- memb_df[,INDEX:=12*(DT_YEAR-BIRTH_YEAR)+(DT_MONTH-BIRTH_MONTH)]
# Restrict to indices from the month after birth through end of enrollment:
memb_df <- memb_df[INDEX>=min(index_df$INDEX) & INDEX<=max(index_df$INDEX)]

# sam: 1098006 recs
# Find the first missing month for each person: 
memb_df <- memb_df[,CENSORINDEX:=min(setdiff(c(index_df$INDEX,Inf), .SD$INDEX)), by=.(ENROLID)]
# sam: 1098006 recs
# Cut off all months past the censor month:
memb_df <- memb_df[INDEX<CENSORINDEX]

temp <- memb_df %>% 
	as_tibble() %>% 
	select(DT_MONTH, ENROLID, BIRTH_DATE, STATE, DTEND, DT_YEAR, INDEX) %>% 
	arrange(ENROLID, INDEX)

print(temp, n=50)