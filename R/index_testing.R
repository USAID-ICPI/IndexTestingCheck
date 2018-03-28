##   Index Testing Threshold Check
##   COP FY19
##   Aaron Chafetz
##   Purpose: generate output for visuals to show OUs not meeting the 30% threshold set during RPM.
##   Date: 2018-03-27
##   Updated: 2018-03-28

## NOTES
#   - Data source: COP19 Data Packs posted after RPM to PEPFAR Sharepoint (missing CIV)
#   - Data pulled from Allocation tab to include all testing Modalities
#   - Focus - Positive Index testing (Index and IndexMod)

# Dependencies ------------------------------------------------------------

  library("here")
  library("fs")
  library("tidyverse")
  library("readxl")
  library("tibble")

# Folder Setup ------------------------------------------------------------
  
  #folder for Data Packs to sit in (DPs downloaded from - https://drive.google.com/open?id=1KCS8QVYo8uAScZs9MJM7m6bOn-1u7xvw)
    dir_create("RawData")
  #folder for visual outputs
    dir_create("Visuals")

# Inputs ------------------------------------------------------------------
  
  #what OU Data Packs exist there
    ou_list <- dir_ls(here("RawData"), glob = "*.xlsx") %>% 
      basename() %>% 
      str_extract("^[\\w]+\\b")

# Develop HTS Dataset -----------------------------------------------------    
    
  # setup blank input file to start binding onto
    df_hts <- tribble(~operatingunit, ~modality, ~hts_type, ~target, ~share) 
    
  #import all OUs
    source("R", "import_hts.R")
    
    #df_hts <- map(.x = ou_list, .f = ~ import_hts(df_hts, .x))
    
    for (x in ou_list) {
      df_dp <- import_hts(x)
      df_hts <- bind_rows(df_hts, df_dp)
    }

  #clean up OU names
    df_hts <- df_hts %>% 
      mutate(operatingunit = 
               case_when(
                 operatingunit == "DemocraticRepublicoftheCongo" ~ "DRC",
                 operatingunit == "SouthAfrica" ~ "South Africa",
                 operatingunit == "SouthSudan" ~ "South Sudan",
                 TRUE ~ operatingunit 
      ))
  

# Export ------------------------------------------------------------------

    df_hts %>% 
      select(-total) %>% 
      write_csv(here("Output","Testing_Modalities_by_OU.csv"), na = "")
    
    df_index <- df_hts %>% 
      filter(modality %in% c("index", "indexmod"), hts_type == "POS") %>% 
      group_by(operatingunit) %>% 
      summarize_if(is.numeric, ~ sum(.)) %>% 
      ungroup() %>% 
      arrange(desc(share)) %>% 
      write_csv(here("Output","IndexTesting_by_OU.csv"), na = "")
    
