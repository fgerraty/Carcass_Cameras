##########################################################################
# Carcass Cameras ########################################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 01: Data Clean ##################################################
#-------------------------------------------------------------------------

# Import raw data 
carcass_camera_photo_data_raw <- read_csv("data/raw/carcass_camera_photo_data_raw.csv")



#Calculate the amount of time per day that a camera is taking timelapse photos, which will be used to quantify scavenging rates and describe scavenger abundance/activity (allows us to correct for different sampling effort and/or detection ability on different days)
daily_timelapse_duration <- carcass_camera_photo_data_raw %>% 
  filter(timelapse == TRUE) %>% 
  group_by(ccam_num, day_num) %>% 
  #Pull date-time from first and last pics in each camera-day
  summarise(first_pic = min(date_time),
            last_pic = max(date_time),
            .groups = "drop") %>% 
  #Calculate amount of time (in seconds) of monitoring per camera-day
  mutate(monitoring_time =  as.duration(last_pic-first_pic)) %>% 
  select(-first_pic, -last_pic)



#Quantify the abundance of scavengers according to two metrics: number of minutes in which each species was detected scavenging per day, and number of scavenger-minutes per day. 
scavenger_community_setup <- carcass_camera_photo_data_raw %>% 
  filter(timelapse == TRUE) %>% 
  #Remove poor image quality so it doesn't count towards scavenger activity
  filter(!keyword == "Poor image quality") %>%
  group_by(ccam_num, day_num) %>% 
  mutate(n_photos = length(unique(file_name))) %>% 
  #Remove species interactions
  filter(!str_detect(keyword, "-")) %>% 
  #Remove unidentified large birds
  filter(!str_detect(keyword, "TUVU/CORA/AMCR")) %>% 
  #Remove disturbances
  filter(!keyword == "Wave disturbance") %>% 
  filter(!keyword == "ELSE") %>% 
  filter(!keyword == "Unknown") %>% 
  
  mutate(
    # split into two parts: number (if present) and species ID
    count = as.numeric(str_extract(keyword, "^[0-9]+")),         # extract leading number
    species_id = str_remove(keyword, "^[0-9]+\\s*")  # remove leading number + space
  ) %>% 
  
  group_by(ccam_num, day_num, carcass_age, species_id, n_photos) %>% 
  summarise(n_detections = n(),
            sum_scavenger_detections = sum(count, na.rm = TRUE), 
            .groups = "drop") %>% 
  mutate(prop_time_detected = n_detections/n_photos,
         prop_time_detected_abundance_adjusted = sum_scavenger_detections/n_photos) %>% 
  left_join(daily_timelapse_duration, by = c("ccam_num", "day_num")) %>% 
  
  
  # Filter out days with less than three hours of scavenger monitoring
  filter(monitoring_time > 60*60*3) %>%   # 60s * 60m * 3hrs
  #Calculate amount of time in seconds (1) in which scav species were detected on the carcass or (2) of scavenging activity by scavengers of a given species (i.e. scavenger-seconds)
  mutate(detection_duration = round(prop_time_detected * monitoring_time), #(1) 
         scavenger_duration = round(prop_time_detected_abundance_adjusted*monitoring_time)) #(2)


# Create "wide" community dataframes of scavenger abundance according to the two metrics calculated in the "scavenger_community_setup" dataframe above ####

    #Scavenger Community Dataframe 1: detection time of each species on carcass

    community_df1 <- scavenger_community_setup %>% 
      select(ccam_num, day_num, carcass_age, species_id, detection_duration) %>% 
      pivot_wider(names_from = species_id, values_from = detection_duration,
                  values_fill = duration(0)) %>% 
      rename(small_mammal = 'Small Mammal') %>% 
      select(-Blank)

    write_csv(community_df1, "data/processed/community_detection_rate.csv")

    #Scavenger Community Dataframe 2: scavenging time of each species on carcass

    community_df2 <- scavenger_community_setup %>% 
      select(ccam_num, day_num, carcass_age, species_id, scavenger_duration) %>% 
      pivot_wider(names_from = species_id, values_from = scavenger_duration,
                  values_fill = duration(0)) %>% 
      rename(small_mammal = 'Small Mammal') %>% 
      select(-Blank)

    write_csv(community_df2, "data/processed/community_scavenging_rate.csv")
    
    
