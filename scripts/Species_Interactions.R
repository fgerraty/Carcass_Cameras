##########################################################################
# Carcass Cameras ########################################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Species Interactions ###################################################
#-------------------------------------------------------------------------

carcass_camera_data <- read_csv("data/processed/carcass_camera_data.csv")

competitive_interactions <- carcass_camera_data %>% 
  filter(event_type == "competition")


#Plot number of competitive interactions per day (TL photos only) by carcass age

plot_df <- competitive_interactions %>% 
  filter(timelapse == TRUE) %>% 
  group_by(ccam_num, carcass_age) %>% 
  summarize(n_competitive_interactions = n(),
            n_days = length(unique(day_num)),
            n_competive_interactions_per_day = n_competitive_interactions/n_days) 

ggplot(plot_df, aes(x=carcass_age, n_competive_interactions_per_day, color = ccam_num))+
  geom_point()


#Plot number of competitive interactions based on species pairs

species_pairs <- competitive_interactions %>%
  filter(timelapse == TRUE) %>% 
  #Count number of species interaction detections and days for each carcass
  group_by(ccam_num, keyword) %>% 
  summarize(n_detections = n(),
            n_days = length(unique(day_num))) %>% 
  group_by(keyword) %>% 
  summarize(n_detections = sum(n_detections),
            n_days = sum(n_days),
            detections_per_day = n_detections/n_days) %>% 
  mutate(keyword = fct_reorder(keyword, detections_per_day, .desc = TRUE)) #Turn into a factor in descending order


ggplot(species_pairs, aes(x=keyword, y=detections_per_day))+
  geom_bar(stat = "identity")








library(tidyverse)

df <- carcass_camera_photo_data_raw

#------------------------------
# 1. Extract competition pairs
#   (1 interaction label per file)
#------------------------------

interaction_df <- df %>% 
  filter(keyword %in% c("TUVU-CORA", "TUVU-UNGU", "CORA-UNGU")) %>% 
  mutate(
    interaction_keyword = keyword,
    species_A = str_extract(keyword, "^[A-Z]+"),
    species_B = str_extract(keyword, "(?<=-)[A-Z]+")
  ) %>% 
  select(file_name, interaction_keyword, species_A, species_B)


#------------------------------
# 2. Feeding species 
#------------------------------
feeding_df <- df %>% 
  filter(str_detect(keyword, "^[0-9]")) %>% 
  mutate(
    feeding_species = str_extract(keyword, "(TUVU|CORA|UNGU)")
  ) %>% 
  filter(!is.na(feeding_species)) %>% 
  distinct(file_name, feeding_species)


#------------------------------
# 3. Join (now safely one-to-many)
#------------------------------

combined <- interaction_df %>% 
  left_join(feeding_df, by = "file_name", relationship = "many-to-many")

#------------------------------------------------------
# 4. Summarize dominance outcomes per interaction pair
#------------------------------------------------------
interaction_events <- combined %>% 
  group_by(file_name, interaction_keyword, species_A, species_B) %>% 
  summarise(
    feeding_A = any(species_A %in% feeding_species),
    feeding_B = any(species_B %in% feeding_species),
    .groups = "drop"
  )


interaction_results <- interaction_events %>% 
  mutate(
    outcome = case_when(
      feeding_A & !feeding_B ~ paste0(species_A, "_wins"),
      feeding_B & !feeding_A ~ paste0(species_B, "_wins"),
      feeding_A & feeding_B  ~ "both_feeding",
      TRUE                   ~ "neither_feeding"
    )
  )

results_table <- interaction_results %>% 
  group_by(interaction_keyword, outcome) %>% 
  summarise(
    n = n(),
    .groups = "drop_last"
  ) %>% 
  mutate(
    prop = n / sum(n)
  ) %>% 
  ungroup()

