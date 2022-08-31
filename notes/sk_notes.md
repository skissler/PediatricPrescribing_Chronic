__Jump to:__

- [9 Aug 2022](#9-Aug-2022)
- [23 Aug 2022](#23-Aug-2022)
- [24 Aug 2022](#24-Aug-2022)
- [25 Aug 2022](#25-Aug-2022)
- [29 Aug 2022](#29-Aug-2022)
- [30 Aug 2022](#30-Aug-2022)
- [31 Aug 2022](#31-Aug-2022)

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


# 29 Aug 2022

I'm going for the full extraction now; hopefully things run smoothly. 

While that's running, I'll also work on refining the rest of the code for saving key data files and visualizing the results. 

Alright it looks like everything is running happily on the sample files - I think next steps are just to take care of the revisions, using the full data under the new extraction. A bit more cleaning to do, and I'll want to generate some data files to reproduce the figures exactly - but that can wait until I've gotten the final set of figures put together. 

Now going to work on cleaning the `run_analysis.R` code to facilitate dealing with reviewer comments. Switching to branch `CleanAnalysis`. 

Alright, the reduction code is cleaned and things still seem to be running happily. The full extraction code is still running. Enough for the morning; will return later. 


# 30 Aug 2022 

It looks like the full extraction succeeded from yesterday - it took about four and a half hours, which isn't terrible. Now I'm going to see if the rest of the code works on the newly extracted stuff: 

I've somehow ended up with fewer children in this most recent pull (124,737 rather than 207,814); I'm not sure why that is; I'll double check. Still, it seems like all of the results are pretty much unchanged. 

Damn - it looks like I was missing V39 as one of my birth code prefixes. That'll do it. I'll have to re-run. 

Before doing the full re-run, I also want to see if there are any specific reviewer comments we need to address through further extractions: 

- Diagnoses beyond positions 1 and 2
- Setting of care 
- Weights 

For diagnoses beyond positions 1 and 2, it looks like this occurs in: 
- 2017
- 2016
- 2015
- 2014
- 2013
- 2010

but not 2008, and we don't have the documentation for 2009. I think we can just pull them for 2010 onward. 

Right - I'm starting a new branch, `AdditionalPulls`, to take care of these things. I've merged everything into `main` up to this point, so if things go haywire, just go back there. 

First step is to get diagnosis codes for positions 3 and 4. 

Trying some code for this now. 

have also added some code to get the place of service (`STDPLAC`). 

It looks like the weight columns are just some kind of linking integers? I'm not sure what that refers to. I think I'll just explain this in the response to reviewers as an attempt to translate across scales. 

I've also done a quick update of 2 years from 720 to 730 days, one year from 360 to 365 days, and 5 years from 1800 to 1825 days. That all in run_analysis. Hopefully I haven't broken anything. 

Going to do a re-run of the full extraction now, now that the extra diagnosis columns are being pulled and the service location. 


# 31 Aug 2022 

Going to check on the run I sent off after work yesterday; hopefully we're back up to the same number of kids as we had in the original version of the mansucript. If so, I'll carry on with the formal analysis. 

Still took about 4.5 hours which is good, and the run finished. I forgot to update the output file names (they're still appended with 'sam'), but that's an easy fix. 

Hm.... seems like we only got a handful more kids, 124,810. Not sure what's going on there... 


Trying to see now if it's just because I asked for 1825 days rather than 1800. Unlikely, but we'll see. 

maybe difference in using substring vs `%like%` for the birth codes?

Ok - it looks like the birth date extraction is no different. 

right - somehow I've gotten more lines from SAS at the midpoint of the analysis. How do we end up getting less? Why are the different? maybe the leq. 






