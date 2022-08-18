echo "Starting extraction:"
date
Rscript code/extract_data/extract_cohort.R;
echo "Done with cohort extraction:";
date;
sas code/extract_data/prepextraction.sas;
echo "Done with preparation";
date;
sas code/extract_data/extract_d.sas;
echo "Done with prescriptions extraction";
date;
sas code/extract_data/extract_o.sas;
echo "Done with visit/vaccination extraction";
date;