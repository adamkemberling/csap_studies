
# SEAMAP GOM offshore analysis - Repo Outline

# Data Provenance

## Offshore Analyses

Data used for offshore analyses of C. sapidus were taken from the [GSMFC
website](www.seamap.gsmfc.org) in March of 2019 as `.csv` files. Data
cleanup from the raw GSMFC data to the final data put into the offshore
model used in the manuscript is done in the following
    procession:

  - `seamap_analyses/offshore_analyses/R/gsmfc_data_wrangling/Seamap_Data_Wrangle.R`
    : Make necessary joins and drop unnecessary
    columns.

  - `seamap_analyses/offshore_analyses/R/gsmfc_data_wrangling/DistOffshore_DataExplo.R`
    : Perform any data filtering for bad observations and surveys
    outside this project’s research scope. Estimate catch in
    hectares.

  - `seamap_analyses/offshore_analyses/R/offshore_analyses/crabs_pre_hectare.R`
    : The restriction of survey years 2000-2018 and the designation of
    study regions are done here. At this point an “analysis data set”
    has been finalized and no more filtering is done.

## Reproductive Analyses

Biological samples (blue crabs) were collected at sea as part of the
2017 seamap summer/fall and groundfish surveys, recording station ID at
time of collection. Frozen samples were taken back to the lab for
processing, with station ID for each later used to join with full
station information and environmental conditions. Detailed station data
was obtained from the Southeast Fisheries Science Center through
communication with the Pascagoula Laboratory.

# Code Overview

## Offshore Analyses

Final analyses added to the manuscript can be found
    here:

  - `seamap_analyses/offshore_analyses/R/final_analyses/Manuscript_Offshore_Modeling.Rmd`

Sex ratios and carapace width summaries are done here:

  - `seamap_analyses/offshore_analyses/R/sex_ratios/sex_ratios.R`

Exploratory analysis and prototype rmarkdowns can be found here:

  - `seamap_analyses/offshore_analyses/R/IOA_report.Rmd`

  - `seamap_analyses/offshore_analyses/R/crabs_per_hectare.Rmd`

Additional R scripts not central to the final research products can be
found here:

  - `seamap_analyses/offshore_analyses/R/mean_catch_estimates.R`

  - `seamap_analyses/offshore_analyses/R/hurdlemod_jags.R.Rmd`

  - `seamap_analyses/offshore_analyses/R/dlnorm_functions.R`

## Reproductive Analyses

Final analyses added to the manuscript can be found here:

  - `seamap_analyses/reproductive_analyses/R/egg_models_2.Rmd`

Exploratory data analyses were done in the
    following:

  - `seamap_analyses/reproductive_analyses/R/egg_analyses_2.R`

  - `seamap_analyses/reproductive_analyses/R/eggmodels_markdown.Rmd`

  - `seamap_analyses/reproductive_analyses/R/hierarchical_size_at_maturity.R`

The remaining files are legacy code for bayesian regression modeling
with JAGS:

  - `seamap_analyses/reproductive_analyses/R/bayesian_fecundity.R`

  - `seamap_analyses/reproductive_analyses/R/post_summ_function.R`
