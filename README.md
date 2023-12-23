# cnidarian_meta-analysis
Scripts associated with cnidarian meta-analysis on experiments testing effects of warming and CO2, primarily focused on coral reef species

## General scripts
- tidy_ISI_search_cnidarian_exp.R

      combines and cleans ISI search (batches of 500 refs)
  
- tidy_entered_data_cnidarian_meta-analysis.Rmd

      tidys and combines data collected from papers (reference search, study info., experimental info, and species info.(trait data)).
      Importantly, it includes calculations for 3 effect sizes (Hedges g, log ratio, and AE)
      
      Note: raw data is not yet provided for this script, but is provided for transparancy: data produced from this script is in publication in supplamanetal material

## - meta-analysis and RCP projections -  GCB publication - https://onlinelibrary.wiley.com/doi/full/10.1111/gcb.15818

- cnidarian_meta-analysis_statistics.Rmd

      runs meta-analysis statisitc using metafor package 
      estimates effects using RCP projects
      estimates increases in temp and CO2 needed for impacts

## Environmental thermal variability and species diversity affect coral sensitivity to thermal stress
# Data available here - (https://figshare.com/account/home#/projects/190410)

- env_match_data_extract_experiments.Rmd
 
      extract data from global layers for collect location for each experiment
      
- env_match_data_extract_coral_reefs.Rmd
 
      extract data from global layers for each coral reef mapped in globe
 
 
- env_match_stats_1_warming_by_response.Rmd
 
      analysis of individual response catagories
      
- env_match_plots.Rmd
 
      produce all plots of manuscript
      
- latitude_range_corals.Rmd
 
      calculate latitudinal range of coral taxa used in experiment
 
