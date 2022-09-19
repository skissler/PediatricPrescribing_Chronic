# =============================================================================
# Import
# =============================================================================
# Note: the following requires the files that are generated in the 'extract_data' directory. 

library(tidyverse) 
library(broom)
library(purrr) 
library(lubridate) 
library(scales) 
library(sf)
source('code/utils.R')
source('code/utils_private.R')

# Data extraction (only needs to be run once):
# system('sas code/extract_data/extract_data.sas')
# source('code/get_popsizes_under5.R')
# source('code/make_chronic.R')
# source('code/get_ndc.R')

# Data import:
source('code/import_fulldata.R')
source('code/reduce_under5.R')

figwidth <- 3.2
figheight <- 3.2*10/16
figres <- 600

# =============================================================================
# Figure 1A: Cumulative prescriptions
# =============================================================================

source('code/make_cumrx.R')

# =============================================================================
# Figure 1B: Time to first prescription
# =============================================================================

source('code/make_cumfirstrx.R')

# ==============================================================================
# Figure 1C: Prescribing histogram
# ==============================================================================

source('code/make_rxhist.R')

# ==============================================================================
# Table 2: Do the chronic condition analysis
# ==============================================================================

source('code/get_rx_asymmetry.R')
source('code/run_chronic_analysis.R')

# ==============================================================================
# Figure 2: Same as figure 1, but for chronic conditions
# ==============================================================================

source('code/make_chronicfigs.R')

# ==============================================================================
# Geographic analysis
# ==============================================================================

source('code/make_geofiles.R')

# ==============================================================================
# Temporal trend analysis
# ==============================================================================

source('code/make_temporal.R')

# ==============================================================================
# Run checks 
# ==============================================================================

source('code/run_checks.R')




