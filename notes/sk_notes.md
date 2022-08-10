__Jump to:__
-[9 Aug 2022](#9-Aug-2022)

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


