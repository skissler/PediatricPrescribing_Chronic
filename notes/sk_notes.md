__Jump to:__

- [9 Aug 2022](#9-Aug-2022)
- [23 Aug 2022](#23-Aug-2022)
- [24 Aug 2022](#24-Aug-2022)
- [25 Aug 2022](#25-Aug-2022)
- [29 Aug 2022](#29-Aug-2022)
- [30 Aug 2022](#30-Aug-2022)
- [31 Aug 2022](#31-Aug-2022)
- [5 Sep 2022](#5-Sep-2022)
- [12 Sep 2022](#12-Sep-2022)
- [13 Sep 2022](#13-Sep-2022)

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


ok, and the loop gives the same as the first cohort pull function. So the difference must be coming from the reduction steps. where? 


# 5 Sep 2022 

Alright, I'm convinced that the SAS pull is the right one. In the preivous R pull, I kept a few people with coverage prior to their inferred birth, which I should probably throw out since we don't really know what's going on there. I was also keeping some people for whom we didn't have coverage during their birth month, which I think we shouldn't be doing. That gives us somewhat fewer people, but I think the results are similar. I'll do the full pull now and verify the paper's findings now. 

Starting the new extraction now - - started at 1:36pm, so should be finished around 6pm. 

While that's running, I should be able to use the old extractions to refine the code. 

Got the code for Figure 1 cleaned. 

Finished cleaning the ranking code. 

I think I've cleaned the rest, minus the local geographic analysis - now to make sure everything runs happily and to update the manuscript with new figures. 

---

Got some figures made for the repeat of Figure 1 but comparing kids with and without pulmonary/respiratory, otologic, and immunological chronic conditions. They're looking good. Next I need to: 

- [x] Write code to save those figures, in `chronicfigs.R`
- [x] Split out `run_analysis.R` into scripts for each sub-figure 
- [ ] Look at prescriptions by birth year to see if there's been a change (surely there's been a decline) 
- [x] Figure out how to incorporate all of these changes into the manuscript - I think shift current Figure 2 to supplement, then make new Figure 2 after Table 2 the repeat of Figure 1 by chronic/nonchronic, with corresponding updates to the text. 

And, it looks like the new extraction finished right on time, at 5:51. So, I should re-run everything using those new files. 


# 12 Sep 2022

Review to-do: 





## Analysis
- [x] Make plot of trends in prescribing over time
- [x] check how many claims have respiratory codes in positions 3 or 4 (and any code in positions 3 or 4) 
- [x] Among those in the top 20% of antibiotic recipients, what proportion were children with chronic conditions?
- [x] Check venue of antibiotic prescribing and include a bit about this if it's interesting. 
- [x] Generate data files for posting on GitHub 

## Writeup

### Quick wins
- [x] Add note in main text about rationale for using custom weights 
- [x] Note the size of bias in restricting to diagnosis positions 1 and 2
- [x] Note that some antibiotics may have been given for an appropriate, non-respiratory condition but linked to an inappropriate respiratory condition. 
- [x] turn odds increases into odds ratios
- [x] Amend figure 2 legend to NOT refer to all prescriptions, as that has been removed. 
- [x] lessen self-criticism about impact of covid pandemic and argue why findings are still generalizable 
- [x] Note that kids with chronic conditions may differ between private and medicare/medicaid. 
- [x] adjust title to refer to first 5 years of life and to chronic conditions 
- [x] abstract: note that "privatey insured children" in the US receive high volumes of antibiotics. 
- [x] Justify 20% cutoff for high-frequency dispensation
- [x] In addition to lack of vaccines for some of the culprit bugs, other factors such overdiagnosis of bacterial infections and high rates of care seeking for minor self-limited viral illnesses are worth mentioning as contributors to antibiotic overuse
- [x] Consider mentioning in the limitations that this does not include treatments that occur in hospitals, EDs, or offices, so that the results may actually underestimate the true prevalence of antibiotic consumption


### Figure updates
- [ ] Insert version of Figure 1 split by those with/without chronic conditions
- [ ] re-label vertical axis of figure 1 to note that these are antibiotic prescriptions
- [ ] Table 2: add a column with N for each category and what proportion of the top antibiotic recipients each category accounts for. Add footnote to remark that some patients may be in more than one category. 
- [ ] Aggregate and present chronic codes by ICD9 and ICD10
- [x] Remove Tables S5-S7 (or combine S5-S6 and remove non-antibiotic prescriptions) 
- [ ] Give full list of antibiotics included in the Supplement 
- [ ] Include full list of CCS delineations in supplement 
- [ ] Export figures as TIF with 600dpi, 9-10pt font. 
- [ ] Shift regional figures to supplement 


### Comprehensive checks
- [ ] Update results and discussion to account for new results 
- [ ] Update all numbers with re-run 
- [ ] Update vocabulary to ensure we're consistent with prescribed, dispensed, refilled, etc. 
- [ ] discuss/emphasize role of children with chronic conditions in overall antibiotic prescribing trends in the Discussion
- [ ] tighten discussion of links with other studies, maybe remove bit about LMICs. 
- [ ] shorten first 2-3 paragraphs to 1-2 paragraphs? 
- [ ] Consider rewording the statement that "little is known about the extent and variation of antibiotic consumption in this group." Although I agree that this study addresses a gap, a lot is already known about antibiotic prescribing for this group. More specific language about the gap addressed could be helpful.
- [ ] Since studying children with chronic conditions is a focus of the study, it would be helpful to provide more background on the motivation for studying these populations. This could replace some of the background on antibiotic resistance and stewardship.
- [ ] Verify that main text is under 3,000 words



# 13 Sep 2022






