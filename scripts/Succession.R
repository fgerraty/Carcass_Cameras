##########################################################################
# Carcass Cameras ########################################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Succession #############################################################
#-------------------------------------------------------------------------

carcass_camera_photo_data_raw <- read_csv("data/raw/carcass_camera_photo_data_raw.csv")


table(carcass_camera_photo_data_raw$keyword)

# Select only scavenging timelapse pics

succession_photo_data_df <- carcass_camera_photo_data_raw %>% 
  #Remove species interactions
  filter(!str_detect(keyword, "-")) %>% 
  #Remove unidentified large birds
  filter(!str_detect(keyword, "TUVU/CORA/AMCR")) %>% 
  #Remove poor image quality
  filter(!keyword == "Poor image quality") %>% 
  #Remove disturbances
  filter(!keyword == "Wave disturbance") %>% 
  filter(!keyword == "ELSE") %>% 
  filter(!keyword == "Unknown") %>% 
  filter(timelapse == TRUE) %>% 
  
  #Break keyword into ID and count values
  
  mutate(
    # split into two parts: number (if present) and species ID
    count = as.numeric(str_extract(keyword, "^[0-9]+")),         # extract leading number
    species_id = str_remove(keyword, "^[0-9]+\\s*")  # remove leading number + space
  ) %>% 
  
  group_by(ccam_num, day_num) %>% 
  mutate(n_photos = n()) %>% 
  group_by(ccam_num, day_num, n_photos, species_id, carcass_age) %>% 
  summarise(n_detections = n())

table(succession_photo_data_df$keyword)


ggplot(succession_photo_data_df, aes(x=as.character(carcass_age), y=n_detections/n_photos))+
  geom_boxplot()+
  facet_wrap(facets = "species_id")
