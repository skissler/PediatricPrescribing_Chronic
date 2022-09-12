# Diagnosis codes in positions 3 and 4: ----------------------------------------

dxprops <- tibble(
	prop_dx1=nrow(visit_df[!is.na(DX1)])/nrow(visit_df),
	prop_dx2=nrow(visit_df[!is.na(DX2)])/nrow(visit_df),
	prop_dx3=nrow(visit_df[!is.na(DX3)])/nrow(visit_df),
	prop_dx4=nrow(visit_df[!is.na(DX4)])/nrow(visit_df)
)

dxprops_resp <- tibble(
	prop_dx1_resp=nrow(visit_df[((!is.na(DX1))&(COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media")))])/nrow(visit_df),
	prop_dx2_resp=nrow(visit_df[((!is.na(DX2))&(COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media")))])/nrow(visit_df),
	prop_dx3_resp=nrow(visit_df[((!is.na(DX3))&(COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media")))])/nrow(visit_df),
	prop_dx4_resp=nrow(visit_df[((!is.na(DX4))&(COND %in% c("Sinusitis","Strep pharyngitis","Pneumonia","Influenza","Tonsillitis","Bronchitis (acute)","URI (other)","Otitis media")))])/nrow(visit_df)
)

# Proportion of kids w/ chronic conditions in the top 20% of abx recipients: ---



# Venue of antibiotic prescribing: ---------------------------------------------