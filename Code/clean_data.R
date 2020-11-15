# Packages ----------------------------------------------------------------

library(tidyverse)

# Data --------------------------------------------------------------------

og_dat <- readxl::read_excel(
  here::here("Data", "SAS ready Final Data File, Marbling Trial.xlsx"),
  skip = 1, .name_repair = janitor::make_clean_names)



all_dat <- og_dat %>%
  
  select(group,
         sex,
         hump_height,
         ph = p_h,
         contains("lean"),
         cook_temp = off_temp,
         panel_day = panelday,
         wbsf,
         overall = overall_sensory_experience,
         tenderness = sensory_tenderness,
         juiciness,
         buttery,
         meaty = meaty_brothy,
         metallic = metallic_bloody_serumy,
         liver_organy,
         grassy
         ) %>%
  
  mutate(eq_cat = case_when(overall > 7.5 ~ "Positive",
                            TRUE ~ "Negative"),
         tender_cat = case_when(wbsf > 3.9 ~ "Tough",
                                TRUE ~ "Tender")) %>%
  
  mutate(tender_cat = factor(tender_cat, levels = c("Tough", "Tender")),
         group = factor(group, levels = c("TR", "SL", "SM", "MT",
                                                  "MD", "SA", "MA")))