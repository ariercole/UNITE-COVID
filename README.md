[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5804973.svg)](https://doi.org/10.5281/zenodo.5804973)

# UNITE-COVID data curation notebook (R-Kernel)
## Version 3.0  26/12/2021

This describes the curation process. We use the finalised dataset "UNITE_COVID_19_global_export-1Dec2020 CSV.csv" 
as the source data / master dataframe. The working dataframe is 'working.df' and is updated by the curation script.

"UNITE_curation_dictionary.xlsx" contains curation/explanatory notes.

ESICM UNITE-COVID curation team
- Ari Ercole (University of Cambridge: Cambridge, GB) ORCID: 0000-0001-8350-8093
- Paul W Elbers (Amsterdam UMC: Amsterdam, NL) ORCID: 0000-0003-0447-6893
- Harm-Jan de Grooth (VU University Medical Center: Amsterdam, NL) ORCID: 0000-0002-7499-076X
- Thomas De Corte (Universiteit Gent: Gent, BE) ORCID: 0000-0001-5011-6640
- Massimiliano Greco (Humanitas University, Rozzano, IT) ORCID: 0000-0003-1003-4637

Coagulation free-text curation dictionaries
- Andrea Lavinio (Cambridge University Hospitals NHS Foundation Trust: Cambridge, GB)
- Ari Ercole (University of Cambridge: Cambridge, GB) ORCID: 0000-0001-8350-8093


## Usage
From version 2 onwards, this has been repackaged as a function

Add data file as "./data/UNITE_COVID_19_global_export-1Dec2020.csv"
Dictionaries are in "./dictionaries"

Then source the current pipline and execute the function get_data() which returns the curated dataset as a dataframe

I.e.:
```
source("curation_pipeline_version_2_3.R")
data.df <- get_data()
```


## Dependencies
The pipeline requires the tidyr package

```
install.packages(tidyr)
```
