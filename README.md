# Impact of resipratory infection and chronic comorbidities on early pediatric dispensing in the United States

[Stephen M. Kissler](mailto:skissler@hsph.harvard.edu), Bill Wang, Ateev Mehrotra, Michael Barnett, Yonatan H. Grad

## Running the analysis

The scripts `run_analysis_server.R` and `run_analysis_local.R` are run sequentially to build the dataset, perform all calculations, and generate all figures. Note that `run_analysis_server.R` relies on raw MarketScan data files to be installed, which are not included here. However, minimal datasets for reproducing the main figures from the manuscript are saved in the `figures/` directory. 

### Building the dataset

The initial lines of `run_analysis_server.R` call in key packages and load custom scripts (`utils.R` and `utils_private.R`). The `utils_private.R` script contains just one line, defining the census API key for making `tidycensus` calls, formatted as `censuskey <- ####`. 

The next set of lines, initially commented out, are needed to build the datasets and need to only be run once. These rely on the raw MarketScan data. The bulk of the extraction happens in `code/extract_data/extract_data.sas`. 

Once the data files have been built, they can be imported and formatted using `code/import_fulldata.R` and `code/reduce_under5.R`. 


### Generating calculations 

The next collection of scripts in `run_analysis_server.R` walk sequentially through the steps needed to run the analysis presented in the manuscript. The early portions of each script generally need access to the files built from the raw MarketScan data. However, summarized files have been extracted and saved in the `underlying_data/` directory. By loading these files, you can safely skip the first parts of each analysis script (down to where the data file has been written to csv) and instead load the data from `underlying_data/`. This allows the user to reproduce the figures without needed access to the full MarketScan dataset. 

### Output and figures

All output and figures are sent to the `figures/` directory. 
