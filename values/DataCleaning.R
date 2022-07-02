library(readxl)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(writexl)

###This file cleans the textile names, color names, value units, and quantity units 

textile.cleaned <- read_xlsx("C:/Users/Alexander Gong/Downloads/WICVOC_Cleaned.xlsx")

textile.final <- textile.cleaned %>%
  filter(textile_name != "-") %>%
  mutate(textile_name = str_replace_all(textile_name, "Chintz", "chintz")) %>%
  mutate(textile_name = str_replace_all(textile_name, "adathaies", "adaties")) %>%
  mutate(textile_name = str_replace_all(textile_name, "aichuabannys", "atchiabanijs")) %>%
  mutate(textile_name = str_replace_all(textile_name, "allejaes", "alacha")) %>%
  mutate(textile_name = str_replace_all(textile_name, "sail cloth", "sailcloth")) %>%
  mutate(textile_name = str_replace_all(textile_name, "sail clothing", "sailcloth")) %>%
  mutate(textile_name = str_replace_all(textile_name, "lingetten (Carolees?)", "carolees")) %>%
  mutate(textile_name = str_replace_all(textile_name, "ginghams", "gingham")) %>%
  mutate(textile_name = str_replace_all(textile_name, "guinees", "guinea-stuffs")) %>%
  mutate(textile_name = str_replace_all(textile_name, "lakens and silks", "lacken")) %>%
  mutate(textile_name = str_replace_all(textile_name, "zegelgaren", "zeilgaren")) %>%
  mutate(textile_name = str_replace_all(textile_name, "zeil", "zeilgaren"))%>%
  mutate(textile_color_arch = tolower(textile_color_arch)) %>%
  mutate(textile_color_arch = str_replace_all(textile_color_arch, "blue / azure", "blue"))%>%
  mutate(textile_color_arch = str_replace_all(textile_color_arch, "brown blue", "brown-blue"))%>%
  mutate(textile_color_arch = str_replace_all(textile_color_arch, "gold, silver", "gold-silver"))%>%
  mutate(textile_color_arch = str_replace_all(textile_color_arch, "green and blue", "green-blue"))%>%
  mutate(textile_color_arch = str_replace_all(textile_color_arch, "green and white", "green-white"))%>%
  mutate(textile_color_arch = str_replace_all(textile_color_arch, "red, gold", "red-gold"))%>%
  mutate(textile_color_arch = str_replace_all(textile_color_arch, "black, gold", "black-gold")) %>%
  mutate(textile_quality_arch = tolower(textile_quality_arch)) 

textile.halfps <- textile.final %>%
  filter(textile_unit == "half ps") %>%
  mutate(textile_quantity = as.numeric(textile_quantity) * 0.5) %>%
  mutate(textile_unit = "ps")

textile.schock <- textile.final %>%
  filter(textile_unit == "schock") %>%
  mutate(textile_quantity = as.numeric(textile_quantity) * 4) %>%
  mutate(textile_unit = "ps")

textile.final <- textile.final %>%
  filter(textile_unit != "half ps") %>%
  filter(textile_unit != "schock")

textile.final <- rbind(textile.final, textile.halfps)
textile.final <- rbind(textile.final, textile.schock)



write_xlsx(textile.final, "C:/Users/Alexander Gong/Documents/Courses/Data_Science/assignment 4/final_data.xlsx")

  
 

 

  


  
  
 