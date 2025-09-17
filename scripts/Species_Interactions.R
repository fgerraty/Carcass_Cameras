##########################################################################
# Carcass Cameras ########################################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Species Interactions ###################################################
#-------------------------------------------------------------------------

carcass_camera_photo_data_raw <- read_csv("data/raw/carcass_camera_photo_data_raw.csv")


interactions <- carcass_camera_photo_data_raw %>% 
  filter(str_detect(keyword, "-") & keyword != "Human - Camera Trappers") %>% 
  filter(timelapse == TRUE)


#Plot number of competitive interactions per day (TL photos only) by carcass age

plot_df <- interactions %>% 
  group_by(ccam_num, carcass_age) %>% 
  summarize(n_competitive_interactions = n(),
            n_days = length(unique(day_num)),
            n_competive_interactions_per_day = n_competitive_interactions/n_days) 

ggplot(plot_df, aes(x=carcass_age, n_competive_interactions_per_day, color = ccam_num))+
  geom_point()


#Plot number of competitive interactions based on species pairs

species_pairs <- interactions %>%
  filter(!str_detect(keyword, "Bird")) %>%
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
