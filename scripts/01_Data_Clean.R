##########################################################################
# Carcass Cameras ########################################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 01: Data Clean ##################################################
#-------------------------------------------------------------------------


######################################
# Clean Carcass Camera Photo Data ####
######################################

# Import raw data 
carcass_camera_data <- read_csv("data/raw/carcass_camera_photo_data_raw.csv") %>% 
  mutate(
    #Make all keywords lowercase
    keyword = str_to_lower(keyword),
    
    #Add scavenger number to non-tallied scavengers
    keyword = case_when(
      keyword == "western fence lizard" ~ "1 western fence lizard",
      keyword == "else" ~ "1 northern elephant seal",
      keyword == "bobcat (no scavenging)" ~ "1 bobcat (no scavenging)",
      keyword == "brush rabbit (no scavenging)" ~ "1 brush rabbit (no scavenging)",
      keyword == "brush rabbit (mouth contact)" ~ "1 brush rabbit",
      keyword == "coyote (no scavenging)" ~ "1 coyote (no scavenging)",
      keyword == "1 mule deer (mouth contact)" ~ "1 mule deer",
      keyword == "virginia opossum (no scavenging)" ~ "1 virginia opossum (no scavenging)",
      keyword == "virginia opossum" ~ "1 virginia opossum",
      keyword == "bird" ~ "1 bird",
      TRUE ~ keyword),
    
    #replace species code with common names
    keyword = str_replace_all(
      keyword,
      c(
        "\\btuvu\\b" = "turkey vulture",
        "\\bcora\\b" = "common raven",
        "\\bungu\\b" = "gull",
        "\\bwora\\b" = "woodrat",
        "\\bdemo\\b" = "deer mouse"
      )
    ),
    
    #Categorize "event type"
    event_type = case_when(
      str_detect(keyword, "blank") ~ "blank",
      str_detect(keyword, "poor image quality") ~ "poor image quality",
      str_detect(keyword, "human") ~ "disturbance",
      str_detect(keyword, "wave") ~ "disturbance",
      str_detect(keyword, "no scavenging") ~ "other",
      str_detect(keyword, "^\\d+") ~ "scavenging",
      str_detect(keyword, "-") ~ "competition",
      TRUE ~ "other"),

    #For scavenging events, split apart scavenger count and species (species = species_1)
    count = if_else(
      event_type == "scavenging",
      as.numeric(str_extract(keyword, "^\\d+")),
      NA_real_ ),
    
    species_1 = case_when(
      event_type == "scavenging" ~ str_remove(keyword, "^\\d+\\s+"),
      TRUE ~ NA_character_ ),

    #For competitive events, split species into species_1 and species_2
  
    species_1 = if_else(
      event_type == "competition",
      str_split(keyword, "-", simplify = TRUE)[,1],
      species_1
    ),
    
    species_2 = if_else(
      event_type == "competition",
      str_split(keyword, "-", simplify = TRUE)[,2],
      NA_character_
    ))

write_csv(carcass_camera_data, "data/processed/carcass_camera_data.csv")


#######################################
# Clean Scavenger Assemblages Data ####
#######################################

scavenging_assemblages <- carcass_camera_data %>% 
  #remove poor image quality photos, disturbance photos, and competition photos (which all are also included in "scavenging")
  filter(event_type %in% c("blank", "scavenging", "other")) %>% 
  #Filter for only timelapse photos
  filter(timelapse == TRUE) %>% 
  #Group carcass age stages 1 and 2
  mutate(carcass_age = if_else(carcass_age %in% c(1,2), "1/2", 
                               as.character(carcass_age)), 
         #Turn into a factor
         carcass_age = factor(carcass_age, levels = c("1/2","3","4"))) %>% 
  #Calculate number of unique photos taken per carcass / decomposition level combo
  group_by(ccam_num, carcass_age) %>% 
  mutate(n_photos = length(unique(file_name))) %>% 
  #Calculate number of photos in which each scavenger species was detected
  group_by(ccam_num, carcass_age, n_photos, species_1) %>% 
  summarise(n_detections = length(unique(file_name))) %>% 
  
  #Filter out species / groups that are not of interest
  filter(!species_1 %in% c(NA, "northern elephant seal", 
                           "turkey vulture/common raven/amcr")) %>% 
  
  #Pivot wider
  pivot_wider(names_from = species_1, values_from = n_detections, values_fill = 0) %>% 
  clean_names()
  

write_csv(scavenging_assemblages, "data/processed/scavenging_assemblages.csv")


###################################
# Summarize Scavenger Dynamics ####
###################################

scavenger_summary <- carcass_camera_data %>% 
  filter(event_type == "scavenging") %>% 
  group_by(species_1, timelapse) %>% 
  summarise(n_detections = n(), .groups = "drop")  %>% 
  mutate(species_1 = fct_reorder(species_1, n_detections, .desc = TRUE)) %>% 
  group_by(species_1) %>%
  mutate(total = sum(n_detections))


#How many scavenging events
sum(scavenger_summary$n_detections)

#How many monitoring days

temp <- carcass_camera_data %>% 
  group_by(ccam_num) %>% 
  summarise(n_days = length(unique(day_num))) 
mean(temp$n_days)


ggplot(scavenger_summary, aes(x=species_1, y=n_detections, fill = timelapse))+
  geom_bar(stat="identity")+
  geom_text(
    aes(x = species_1, y = total, label = total),
    vjust = -0.3, size = 3
  ) +
  labs(x="Scavenger Species", y="Total Number of Scavenging Observations", 
       fill = "Photo Type")+
  scale_fill_manual(values = c("#378EC4", "#173753"), labels = c("Motion-triggered", "Timelapse"))+
  scale_y_continuous(limits = c(0, 17000))+
  theme_few()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(face = "bold"),
        panel.border = element_rect(linewidth = 2),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        legend.position = "inside", 
        legend.position.inside = c(.7, .7))



#Plot for HPAI meeting

scavenger_summary <- carcass_camera_data %>% 
  filter(event_type == "scavenging") %>% 
  mutate(species = case_when(
            species_1 == "bird" ~ "insectivorous bird", 
            species_1 == "turkey vulture/common raven/amcr" ~ "unidentified large bird",
            species_1 == "gull" ~ "gull (Larus spp.)",
            species_1 == "rodent" ~ "unidentified rodent",
            TRUE ~ species_1)) %>% 
  filter(!species %in% c("black phoebe")) %>% 
  group_by(species, timelapse) %>% 
  summarise(n_detections = n(), .groups = "drop")  %>% 
  mutate(species = fct_reorder(species, n_detections, .desc = TRUE)) %>% 
  group_by(species) %>%
  mutate(total = sum(n_detections))



temp_scav_plot <- ggplot(scavenger_summary, aes(x=species, y=n_detections, fill = timelapse))+
  geom_bar(stat="identity")+
  geom_text(
    aes(x = species, y = total, label = total),
    vjust = -0.3, size = 3
  ) +
  labs(x="Scavenger Species", y="Total Number of Scavenging Observations", 
       fill = "Photo Type")+
  scale_fill_manual(values = c("#378EC4", "#173753"), labels = c("Motion-triggered", "Timelapse"))+
  scale_y_continuous(limits = c(0, 17000))+
  theme_few()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(face = "bold"),
        panel.border = element_rect(linewidth = 2),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        legend.position = "inside", 
        legend.position.inside = c(.5, .7))

ggsave("output/temp_scav_plot.png", temp_scav_plot, 
       width = 8, height = 5, units = "in", dpi = 600)
