# ESICM UNITE-COVID data curation pipeline

This describes the curation process. We use the finalised dataset "UNITE_COVID_19_global_export-1Dec2020 CSV.csv" 
as the source data / master dataframe. The working dataframe is 'working.df' and is updated by the curation script.

"UNITE_curation_dictionary.xlsx" contains curation/explanatory notes regarding the variable definitions, missingness 
and any known issues.

The main pipeline is contained within the curation_pipeline_version_XXX.R file. It accepts the finalised dataset above
and produces a curated dataframe 'working.df'.

UNITE-COVID curation team (on behalf of the ESICM UNITE-COVID consortium)
- Ari Ercole (University of Cambridge: Cambridge, GB) ORCID: 0000-0001-8350-8093
- Paul W Elbers (Amsterdam UMC: Amsterdam, NL) ORCID: 0000-0003-0447-6893
- Harm-Jan de Grooth (VU University Medical Center: Amsterdam, NL) ORCID: 0000-0002-7499-076X
- Thomas De Corte (Universiteit Gent: Gent, BE) ORCID: 0000-0001-5011-6640
- Massimiliano Greco (Humanitas University, Rozzano, IT) ORCID: 0000-0003-1003-4637

Version 1.0 published 23/3/2021
