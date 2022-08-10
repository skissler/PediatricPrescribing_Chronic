# DATA PREPARATION (run once) -------------------------------------------------

# Make ICD maps: 
Rscript code/letter/make_icd_maps.R;

# Extract cohort: 
Rscript code/letter/extract_under5/extract_letter_cohort.R;

# Prepare prescription and visit/vaccination extractions: 
sas code/letter/extract_under5/prepextraction.sas;

# Extract prescriptions: 
sas code/letter/extract_under5/extract_letter_d.sas;

# Extract visits and vaccinations:
sas code/letter/extract_under5/extract_letter_o.sas;

# Make list of chronic conditions: 
source('code/letter/make_chronic.R')

# DATA ANALYSIS (from within R) ------------------------------------------------

# Import data and link prescriptions with visits: 
source('code/letter/import_fulldata.R')

# Reduce data (turn diagnoses into prioritized codes): 
source('code/letter/reduce_under5.R')

# Analyze data and make figures: 
source('code/letter/analyze_under5_letter.R')