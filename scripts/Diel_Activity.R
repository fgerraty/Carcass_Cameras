##########################################################################
# Carcass Cameras ########################################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Diel Activity ##########################################################
#-------------------------------------------------------------------------

carcass_camera_photo_data_raw <- read_csv("data/raw/carcass_camera_photo_data_raw.csv")

diel_activity <- carcass_camera_photo_data_raw %>% 
  filter(timelapse == TRUE) %>% 
  #Remove species interactions
  filter(!str_detect(keyword, "-")) %>% 
  #Remove unidentified large birds
  filter(!str_detect(keyword, "TUVU/CORA/AMCR")) %>% 
  mutate(
    # split into two parts: number (if present) and species ID
    count = as.numeric(str_extract(keyword, "^[0-9]+")),         # extract leading number
    species_id = str_remove(keyword, "^[0-9]+\\s*")) %>%   # remove leading number + space
  filter(species_id %in% c("TUVU", "CORA", "UNGU", "Bird", "Small Mammal")) %>% 
  mutate(hour = hour(date_time)) %>% 
  group_by(species_id, hour) %>%
  summarize(freq = n())


#Create temporary dataframe with all species/hour combinations with "freq", the frequency of recorded scavenging events
temp_df <- data.frame(species_id = rep(unique(diel_activity$species_id), each = 24),
                      hour = rep(0:23, n_distinct(diel_activity$species_id)),
                      freq = 0)

#Combine temporary dataframe with "activity_df" and filter to that there is a value (>= 0) for each species/hour combination
activity_df <- rbind(temp_df, diel_activity)%>% #combine dataframes
  group_by(species_id, hour) %>% #group by species + hour
  mutate(freq = max(freq)) %>% #keep the maximum value for each species/hour combo
  distinct() #remove duplicates



#Set up polar coordinate system for plotting
cp <- coord_polar(start=0)
cp$is_free <- function() TRUE


# Plot with all species
activity_plot_all_spp <- ggplot(data = activity_df, 
                                aes(x = hour, y = freq)) +
  geom_bar(fill = "gray60", 
           stat = "identity",
           position = position_nudge (x=.5))+
  cp+
  facet_wrap("species_id", scales="free")+
  theme_bw()+
  theme(aspect.ratio = 1)+
  scale_x_continuous(limits = c(-.0001, 24), breaks = 0:23)+
  labs(x = "", 
       y = "No. Recorded Scavenging Events")
activity_plot_all_spp




# Exploring Diel Activity by Carcass Decomp Level # Exploratory! 

diel_activity <- carcass_camera_photo_data_raw %>% 
  filter(timelapse == TRUE) %>% 
  #Remove species interactions
  filter(!str_detect(keyword, "-")) %>% 
  #Remove unidentified large birds
  filter(!str_detect(keyword, "TUVU/CORA/AMCR")) %>% 
  mutate(
    # split into two parts: number (if present) and species ID
    count = as.numeric(str_extract(keyword, "^[0-9]+")),         # extract leading number
    species_id = str_remove(keyword, "^[0-9]+\\s*")) %>%   # remove leading number + space
  filter(species_id %in% c("TUVU", "CORA", "UNGU", "Bird", "Small Mammal")) %>% 
  mutate(hour = hour(date_time)) %>% 
  group_by(species_id, carcass_age, hour) %>%
  summarize(freq = n())



#Create temporary dataframe with all species/hour combinations with "freq", the frequency of recorded scavenging events

temp_df <- expand_grid(
  species_id = unique(diel_activity$species_id),
  hour = 0:23,
  carcass_age = 1:4
) %>%
  mutate(freq = 0)

#Combine temporary dataframe with "activity_df" and filter to that there is a value (>= 0) for each species/hour combination
activity_df <- rbind(temp_df, diel_activity)%>% #combine dataframes
  group_by(species_id, hour, carcass_age) %>% #group by species + hour
  mutate(freq = max(freq)) %>% #keep the maximum value for each species/hour combo
  distinct() #remove duplicates



#Set up polar coordinate system for plotting
cp <- coord_polar(start=0)
cp$is_free <- function() TRUE


# Plot with all species
activity_plot_all_spp <- ggplot(data = activity_df, 
                                aes(x = hour, y = freq, fill = species_id)) +
  geom_bar(stat = "identity",
           #position = position_nudge (x=.5)
           )+
  cp+
  facet_wrap("carcass_age", scales="free")+
  theme_bw()+
  theme(aspect.ratio = 1)+
  scale_x_continuous(limits = c(-.0001, 24), breaks = 0:23)+
  labs(x = "", 
       y = "No. Recorded Scavenging Events")
activity_plot_all_spp


