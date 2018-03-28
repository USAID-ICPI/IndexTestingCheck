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


# Import/Combine Function -------------------------------------------------

  #function for importing and binding Data Packs   
    #import_hts <- function(df, opunit){
    import_hts <- function(opunit){  
      
      #print OU importing/binding 
      print(opunit)
      
      #import OU (starting at row 6 with variable names)
      df_dp <- read_excel(Sys.glob(here("RawData", paste0(opunit, "*.xlsx"))), 
                       sheet = "Allocation by SNUxIM", skip = 5)
      
      df_dp <- df_dp %>% 
        #add column for OU name
        mutate(operatingunit = opunit) %>% 
        #limit to OU HTS targets since HTS is our scope
        select(operatingunit, starts_with("D_hts_tst"), -ends_with("_pct"), -D_hts_tst_keypop_fy19) %>% 
        #aggregate before reshaping, smaller df to reshape
        group_by_if(is_character) %>% 
        summarise_if(is.numeric, ~sum(., na.rm = TRUE)) %>% 
        ungroup() %>% 
        
        #reshape so all modalities are one column
        gather(modality, target, -operatingunit) %>% 
        #seperate out Positives vs Total & clean modality from long variable name to just modality
        mutate(hts_type = ifelse(str_detect(modality, "_pos_"), "POS", "TOT"),
               modality = str_remove_all(modality, "D_hts_tst_|pos_|_fy19|_u15|_o15")) %>% 
        
        #aggregate to OU x Prioritization x Modality x HTS_type
        group_by_if(is_character) %>% 
        summarise_if(is.numeric, ~sum(., na.rm = TRUE)) %>% 
        ungroup() %>% 
        #remove any missing/zero lines
        filter(target != 0, !is.na(target)) %>% 
        
        #create each modality's share
        group_by(operatingunit, hts_type) %>% 
        mutate(total = sum(target),
               share = target / sum(target)) %>% 
        ungroup()
      
      #bind out to cumulative dataframe
      #df <- bind_rows(df, df_dp)
    }
 
    
# Develop HTS Dataset -----------------------------------------------------    
    
  # setup blank input file to start binding onto
    df_hts <- tribble(~operatingunit, ~modality, ~hts_type, ~target, ~share) 
    
  #import all OUs
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
    
