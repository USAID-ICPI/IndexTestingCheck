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
  library("tidyverse")
  library("readxl")
  library("scales")



# Import ------------------------------------------------------------------

  #created from index_testing.R
    df_index <- read_csv(here("Output", "IndexTesting_by_OU.csv"))


# Adjustments -------------------------------------------------------------
    
  #modifications for graphing
    df_index <- df_hts %>% 
      filter(modality %in% c("index", "indexmod"), hts_type == "POS") %>% 
      group_by(operatingunit) %>% 
      summarize_if(is.numeric, ~ sum(.)) %>% 
      ungroup() %>% 
      arrange(desc(share)) %>% 
      mutate(above30 = ifelse(share < .30, 1, 0),
             label_share = paste0(round(share * 100, 0), "%"),
             label_x = row_number(),
             label_y = 0)
    

# Graph -------------------------------------------------------------------

 
  #share
    ggplot(df_index, aes(reorder(operatingunit, desc(share)), share, fill = above30)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      scale_fill_continuous(low = "#99c2eb", high = "#2166ac") +
      #geom_point(aes(label_x, label_y), show.legend=FALSE) +
      #geom_text(aes(label_x, label_y, label = label_share), color = "white", hjust = 2, fontface = "bold") +
      coord_flip() +
      scale_y_continuous(name="Index Target Share of HTS_POS", labels = percent) +
      labs(title = "Index Testing as Share of Total HTS_POS", 
           subtitle = "Fourteen of the 21 OUs have index testing target shares less than the 30% prescribed during RPMs.",
           caption = "Source: COP19 Data Packs") +
      geom_text(aes(label = label_share), color = "white", hjust = 2, fontface = "bold") +
      geom_hline(yintercept = 0.3, color = "#2166ac", linetype = "dashed", size = 1) +
      geom_text(aes(21, .3, label = "30%"), color = "#2166ac", hjust = -.2, fontface = "bold") +
      theme(axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.x=element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank())
  
  
  #targets
    ggplot(df_index, aes(reorder(operatingunit, target), target, fill = above30)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(name="HTS_POS Index Target", labels = comma) +
      labs(x = "",
           title = "OU Breakdown of HTS_POS Index FY19 Targets") +
      
      ggplot(df_index, aes(reorder(operatingunit, target), target)) +
      geom_bar(stat = "identity", fill = above30) +
      coord_flip()    