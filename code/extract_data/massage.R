

memb_df <- memb_df_raw

temp <- memb_df %>% 
	as_tibble() %>% 
	select(DT_MONTH, ENROLID, BIRTH_DATE, STATE, DTEND, DT_YEAR, INDEX) %>% 
	arrange(ENROLID, INDEX)

print(temp, n=50)