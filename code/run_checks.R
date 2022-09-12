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

# > dxprops
# # A tibble: 1 × 4
#   prop_dx1 prop_dx2 prop_dx3 prop_dx4
#      <dbl>    <dbl>    <dbl>    <dbl>
# 1    0.997    0.231   0.0871   0.0172

# > dxprops_resp
# # A tibble: 1 × 4
#   prop_dx1_resp prop_dx2_resp prop_dx3_resp prop_dx4_resp
#           <dbl>         <dbl>         <dbl>         <dbl>
# 1         0.256        0.0789        0.0257       0.00376


# Proportion of kids w/ chronic conditions in the top 20% of abx recipients: ---

length(intersect(chronic_ids, superuser_ids))/length(superuser_ids)
# 0.3551236

length(intersect(chronic_ids, memb_df$ENROLID))/nrow(memb_df)
# 0.2020455

# Venue of antibiotic prescribing: ---------------------------------------------