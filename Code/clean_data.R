# Packages ----------------------------------------------------------------

library(tidyverse)

# Data --------------------------------------------------------------------

og_dat <- readxl::read_excel(here::here("Data", "SAS ready Final Data File, Marbling Trial.xlsx"),
                             skip = 1, .name_repair = janitor::make_clean_names)


# All Data ------------------------------------------------------------


all_dat <- og_dat %>%
  select(location, group, sex, astamp, contains("mem"),
         me_high, hcw, contains("ft"), contains("rea"), contains("pyg"),
         side1yg, side2yg, hump_height, p_h, wbsf, off_temp,
         cook_day:overall_sensory_experience) %>%
  rename(a_stamp = astamp,
         ms_1 = mems1,
         ms_2 = mems2,
         ms_high = me_high,
         ft_1 = side1ft,
         ft_2 = side2ft,
         rea_1 = side1rea,
         rea_2 = side2rea,
         yg_1 = side1yg,
         yg_2 = side2yg,
         ph = p_h,
         cook_temp = off_temp,
         panel_day = panelday) %>%
  mutate(eq_cat = case_when(overall_sensory_experience > 7.5 ~ "Positive",
                            TRUE ~ "Negative"),
         tender_cat = case_when(wbsf > 3.9 ~ "Tough",
                                TRUE ~ "Tender")) %>%
  mutate(tender_cat = factor(tender_cat, levels = c("Tough", "Tender"))) %>%
  mutate_at(vars(location, panel_day, eq_cat, tender_cat),
            ~ factor(.))


# Subset Data -------------------------------------------------------------

# data before new marbling groups
subset_dat <- all_dat %>%

  select(group = group, marbling_score = ms_high, shear_force = wbsf,
         juiciness, tenderness = sensory_tenderness, buttery,
         meaty = meaty_brothy, metallic = metallic_bloody_serumy,
         overall_sensory_experience, contains("cat")) %>%

  mutate(group = factor(group, levels = c("TR", "SL", "SM", "MT",
                                          "MD", "SA", "MA"))) %>%

  arrange(marbling_score)

# data with new marbling groups and n = 50 per group
set.seed(12)

pr_dat <- subset_dat %>%
  filter(group == "SA" | group == "MA") %>%
  sample_n(50) %>%
  mutate(group = "Prime")

md_dat <- subset_dat %>%
  filter(group == "MD") %>%
  sample_n(50) %>%
  mutate(group = "Moderate (Upper 1/3rd)")

mt_dat <- subset_dat %>%
  filter(group == "MT") %>%
  sample_n(50) %>%
  mutate(group = "Modest (Middle 1/3rd)")

top_choice_dat <- anti_join(subset_dat, md_dat) %>%
  anti_join(., mt_dat) %>%
  filter(group == "MD" | group == "MT") %>%
  sample_n(50) %>%
  mutate(group = "Top Choice (Upper 2/3rd)")

sm50_dat <- subset_dat %>%
  filter(group == "SM") %>%
  mutate(group = case_when(marbling_score >= 450 ~ "Small 50",
                           TRUE ~ "Small 00")) %>%
  filter(group == "Small 50") %>%
  sample_n(50)

sm00_dat <- subset_dat %>%
  filter(group == "SM") %>%
  mutate(group = case_when(marbling_score >= 450 ~ "Small 50",
                           TRUE ~ "Small 00")) %>%
  filter(group == "Small 00") %>%
  sample_n(50)

subset_dat <- bind_rows(pr_dat, top_choice_dat, md_dat, mt_dat, sm50_dat, sm00_dat) %>%
  mutate(group = factor(group, levels = c("Small 00", "Small 50", "Modest (Middle 1/3rd)",
                                          "Top Choice (Upper 2/3rd)", "Moderate (Upper 1/3rd)", "Prime")))

remove(pr_dat, md_dat, mt_dat, top_choice_dat, sm50_dat, sm00_dat)
