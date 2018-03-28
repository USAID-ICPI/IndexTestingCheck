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
