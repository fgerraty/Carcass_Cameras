##########################################################################
# Carcass Cameras ########################################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 01: Data Clean ##################################################
#-------------------------------------------------------------------------

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
  
temp <- carcass_camera_data %>% 
  select(keyword, event_type) %>% 
  unique()

write_csv(carcass_camera_data, "data/processed/carcass_camera_data.csv")



scavenger_summary <- carcass_camera_data %>% 
  filter(event_type == "scavenging") %>% 
  group_by(species_1, timelapse) %>% 
  summarise(n_detections = n(), .groups = "drop")  %>% 
  mutate(species_1 = fct_reorder(species_1, n_detections, .desc = TRUE)) %>% 
  group_by(species_1) %>%
  mutate(total = sum(n_detections))



ggplot(scavenger_summary, aes(x=species_1, y=n_detections, fill = timelapse))+
  geom_bar(stat="identity")+
  geom_text(
    aes(x = species_1, y = total, label = total),
    vjust = -0.3, size = 3
  ) +
  labs(x="Scavenger Species", y="Total Number of Scavenging Events", 
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

