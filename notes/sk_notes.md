__Jump to:__

- [9 Aug 2022](#9-Aug-2022)
- [23 Aug 2022](#23-Aug-2022)
- [24 Aug 2022](#24-Aug-2022)
- [25 Aug 2022](#25-Aug-2022)

# 9 Aug 2022
To do: 
- [ ] Generate a clean repository (this one) with the full data and code for the pediatric prescribing manuscript 
- [ ] Address points in response to reviewers 

Note that the relevant code here is currently stored in `MarketScanPrescribing/code/letter/`, with most of the recent code marked by the `resp` suffix. 

What's the minimal dataset we need to make this reproducible? 

Something like: quantiles of antibiotic prescriptions, separated by respiratory or non-respiratory conditions? And by MSA, if we keep that analysis. And we'll need linked chronic conditions... 

What's the unique row label we want? 

Age - MSA - chronic condition / not that chronic condition - resp/non-resp  - number of antibiotics 

Something like that? 

And probably two parallel datasets - one for cumulative prescriptions, one for proportion who have received at least one prescription. 

or, could make the indices: 

Age (1/2/5)- MSA - chronic condition / not that chronic condition - resp/non-resp  - number of antibiotics – and the value is the number of children who fall into that category (i.e. the number of children of these attributes who got 20 antibiotics by age 5, for eample) 

Might need multiple datasets for each of these figures - maybe best to just release the underlying data once we've gotten it extracted and cleaned, for each figure. 


# 23 Aug 2022 

Working now to get a comprehensive set of extraction code for the analysis - rather than the disparate pieces where things currently are. 

---

I've gotten the birth dates extracted at least, in `code/extract_data/extract_data.sas`

Next step is to try to get the full cohort extracted - but the server says it needs maintenance at noon, so I think I'm going to regroup and take care of that later. 

# 24 Aug 2022 

Making progress with the cohort extraction. Now trying to just keep individuals who are present for a full five years after birth. 


# 25 Aug 2022 

Got the cohort extraction done, and the rest should be straightforward. Today I'll focus on the revision plan. 

Just got those done, and pulled all the years of data from the sample files. Going to see now if I can do some analysis on the sample files, and then we'll do the full pull. 

---

Extraction is done, and I've gotten all of the preamble code working. Now to just clean up the figure-making code, and we should be in good shape. 



