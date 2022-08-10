echo "Starting extraction:"
date
Rscript code/letter/extract_letter/extract_letter_cohort.R;
echo "Done with cohort extraction:";
date;
sas code/letter/extract_letter/prepextraction.sas;
echo "Done with preparation";
date;
sas code/letter/extract_letter/extract_letter_d.sas;
echo "Done with prescriptions extraction";
date;
sas code/letter/extract_letter/extract_letter_o.sas;
echo "Done with visit/vaccination extraction";
date;