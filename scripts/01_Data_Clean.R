##########################################################################
# Año Nuevo Pinniped Carcass Cameras #####################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 01: Data Clean ##################################################
#-------------------------------------------------------------------------


######################################
# Clean Carcass Camera Photo Data ####
######################################

# Import raw data 
carcass_camera_data <- read_csv("data/raw/carcass_camera_photo_data_raw.csv") |>  
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
      keyword == "eust" ~ "1 eust",
      keyword == "ungu" ~ "1 ungu",
      keyword == "songbird" ~ "1 songbird",
      TRUE ~ keyword),
    
    #replace species code with common names
    keyword = str_replace_all(
      keyword,
      c(
        "\\btuvu\\b" = "turkey vulture",
        "\\bcora\\b" = "common raven",
        "\\bungu\\b" = "gull",
        "\\bwora\\b" = "woodrat",
        "\\bdemo\\b" = "deer mouse",
        "\\beust\\b" = "European starling",
        "\\bsosp\\b" = "song sparrow",
        "\\bwcsp\\b" = "white-crowned sparrow",
        "\\bkill\\b" = "killdeer",
        "\\bblph\\b" = "black phoebe",
        "\\bgcsp\\b" = "golden-crowned sparrow",
        "\\bsavs\\b" = "savannah sparrow",
        "\\bbogu\\b" = "Bonaparte's gull",
        "\\bhosp\\b" = "house sparrow",
        "\\bhofi\\b" = "house finch",
        "\\bamcr\\b" = "American crow",
        "\\bsepl\\b" = "semipalmated plover")
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

#How many photos total

length(unique(carcass_camera_data$file_name))

#######################################
# Clean Scavenger Assemblages Data ####
#######################################

scavenging_assemblage_rates <- carcass_camera_data |>  
  #remove poor image quality photos, disturbance photos, and competition photos. Note that competition photos are also tagged as "scavenging" and therefore retained for total photo counts
  filter(event_type %in% c("blank", "scavenging", "other")) |>  
  #Filter for only timelapse photos
  filter(timelapse == TRUE) |>  
  #Group carcass age stages 1 and 2
  mutate(carcass_age = if_else(carcass_age %in% c(1,2), "1/2", 
                               as.character(carcass_age)), 
         #Turn into a factor
         carcass_age = factor(carcass_age, levels = c("1/2","3","4"))) |>  
  #Calculate number of unique photos taken per carcass / decomposition level combo
  group_by(ccam_num, carcass_age) |>  
  mutate(n_photos = length(unique(file_name))) |>  
  #Calculate number of photos in which each scavenger species was detected
  group_by(ccam_num, carcass_age, n_photos, species_1) |>  
  summarise(n_detections = length(unique(file_name)), .groups = "drop") |>  
  
  #Filter out species / groups that are not of interest or not IDed to low enough taxonomic level
  filter(!species_1 %in% c(NA, "northern elephant seal", 
                           "turkey vulture/common raven/American crow", 
                           "rodent",
                           "songbird",
                           "bird", 
                           "sparrow",
                           "plover")) |>  
  #Filter for only carcasses (carcass-age combos) with >100 monitoring photos (e.g. ~12 hrs)
  filter(n_photos > 100) |> 
  mutate(detection_prop = n_detections/n_photos) |> 
  select(-n_detections, -n_photos) |> 
  #Pivot wider
  pivot_wider(names_from = species_1, values_from = detection_prop, values_fill = 0) |>  
  clean_names()
  

write_csv(scavenging_assemblage_rates, "data/processed/scavenging_assemblage_rates.csv")


scavenging_assemblage_counts <- carcass_camera_data |>  
  #remove poor image quality photos, disturbance photos, and competition photos. Note that competition photos are also tagged as "scavenging" and therefore retained for total photo counts
  filter(event_type %in% c("blank", "scavenging", "other")) |>  
  #Filter for only timelapse photos
  filter(timelapse == TRUE) |>  
  #Group carcass age stages 1 and 2
  mutate(carcass_age = if_else(carcass_age %in% c(1,2), "1/2", 
                               as.character(carcass_age)), 
         #Turn into a factor
         carcass_age = factor(carcass_age, levels = c("1/2","3","4"))) |>  
  #Calculate number of unique photos taken per carcass / decomposition level combo
  group_by(ccam_num, carcass_age) |>  
  mutate(n_photos = length(unique(file_name))) |>  
  #Calculate number of photos in which each scavenger species was detected
  group_by(ccam_num, carcass_age, n_photos, species_1) |>  
  summarise(n_detections = length(unique(file_name)), .groups = "drop") |>  
  
  #Filter out species / groups that are not of interest or not IDed to low enough taxonomic level
  filter(!species_1 %in% c(NA, "northern elephant seal", 
                           "turkey vulture/common raven/American crow", 
                           "rodent",
                           "songbird",
                           "bird", 
                           "sparrow",
                           "plover")) |>  
  #Filter for only carcasses (carcass-age combos) with >100 monitoring photos (e.g. ~12 hrs)
  filter(n_photos > 100) |> 
  #Pivot wider
  pivot_wider(names_from = species_1, values_from = n_detections, values_fill = 0) |>  
  clean_names()


write_csv(scavenging_assemblage_counts, "data/processed/scavenging_assemblage_counts.csv")


###################################
# Summarize Scavenger Dynamics ####
###################################

scavenger_summary <- carcass_camera_data |>  
  filter(event_type == "scavenging") |>  
  #Clean species names
  mutate(species = case_when(
    species_1 == "bird" ~ "bird (Aves)", 
    species_1 == "turkey vulture/common raven/American crow" ~ "large bird (Aves)",
    species_1 == "gull" ~ "gull (Larus spp.)",
    species_1 == "rodent" ~ "rodent (Rodentia)",
    species_1 == "sparrow" ~ "sparrow (Passerellidae)",
    species_1 == "songbird" ~ "songbird (Passeri)",
    species_1 == "plover" ~ "plover (Charadriinae)",
    TRUE ~ species_1)) |>  
  group_by(species, timelapse) |>  
  summarise(n_detections = n(), .groups = "drop")  |>  
  group_by(species) |>  
  mutate(total = sum(n_detections)) |>  
  ungroup() |>  
  mutate(species = fct_reorder(species, total, .desc = TRUE))


#How many scavenging events
sum(scavenger_summary$n_detections)

#How many monitoring days

mean((carcass_camera_data |>  
  group_by(ccam_num) |>  
  summarise(n_days = length(unique(day_num))))$n_days)

sum((carcass_camera_data |>  
        group_by(ccam_num) |>  
        summarise(n_days = length(unique(day_num))))$n_days)



#Summary plot 

scav_summary_plot <- ggplot(scavenger_summary, aes(x=species, y=n_detections, fill = timelapse))+
  geom_bar(stat="identity")+
  geom_text(
    aes(x = species, y = total, label = total),
    vjust = -0.3, size = 2.2
  ) +
  labs(x="Scavenger Species", 
       y="Total Number of Scavenging Observations", 
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
        legend.position.inside = c(.4, .7))

scav_summary_plot


ggsave("output/scav_summary_plot.png", scav_summary_plot, 
       width = 8, height = 5, units = "in", dpi = 600)

