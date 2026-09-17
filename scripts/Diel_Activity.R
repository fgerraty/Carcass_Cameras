##########################################################################
# Carcass Cameras ########################################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Diel Activity ##########################################################
#-------------------------------------------------------------------------

carcass_camera_data <- read_csv("data/processed/carcass_camera_data.csv")

# Apply 10min temporal filter to nighttime detections ####
assign_events <- function(times, window_min) {
  n <- length(times)
  event_id <- integer(n)
  event_id[1] <- 1
  event_start <- times[1]
  current <- 1
  
  if (n > 1) {
    for (i in seq_len(n)[-1]) {
      if (as.numeric(difftime(times[i], event_start, units = "mins")) < window_min) {
        event_id[i] <- current
      } else {
        current <- current + 1
        event_start <- times[i]
        event_id[i] <- current}}}
  event_id
}

independent_nocturnal <- carcass_camera_data |> 
  filter(str_detect(file_name, "MT")) |> 
  filter(event_type == "scavenging") |>  
  arrange(ccam_num, deployment, species_1, date_time)  |> 
  group_by(ccam_num, deployment, species_1) |> 
  mutate(event_id = assign_events(date_time, 10)) |> 
  group_by(ccam_num, deployment, species_1, event_id) |> 
  mutate(n_lumped = n(), lumped = n_lumped > 1,
         lumped_files = paste(file_name, collapse = "; ")) |> 
  slice_max(count, n = 1, with_ties = FALSE) |> 
  ungroup() |> 
  select(-event_id, -n_lumped, -lumped, -lumped_files)




diel_activity <- carcass_camera_data |> 
  filter(event_type == "scavenging") |>   #Remove species interactions
  filter(!str_detect(file_name, "MT")) |> 
  filter(timelapse == TRUE) |> 
  bind_rows(independent_nocturnal) |> 
  filter(!species_1 == "northern elephant seal") |> 
  mutate(hour = hour(date_time)) |> 
  mutate(clade = case_when(
    species_1 %in% c("brush rabbit", "coyote", "deer mouse", "gray fox",
                     "mule deer", "rodent", "virginia opossum", "woodrat") ~ "mammal",
    species_1 == "western fence lizard" ~ "reptile",
    TRUE ~ "bird")) |> 
  group_by(clade, hour) |> 
  summarize(freq = n()) |> 
  filter(clade %in% c("bird", "mammal"))



#Create temporary dataframe with all species/hour combinations with "freq", the frequency of recorded scavenging events
temp_df <- data.frame(clade = rep(unique(diel_activity$clade), each = 24),
                      hour = rep(0:23, n_distinct(diel_activity$clade)),
                      freq = 0)

#Combine temporary dataframe with "activity_df" and filter to that there is a value (>= 0) for each species/hour combination
activity_df <- rbind(temp_df, diel_activity)%>% #combine dataframes
  group_by(clade, hour) %>% #group by species + hour
  mutate(freq = max(freq)) %>% #keep the maximum value for each species/hour combo
  distinct() #remove duplicates



#Set up polar coordinate system for plotting
cp <- coord_polar(start=0)
cp$is_free <- function() TRUE


# Plot with all species
activity_plot_all_spp <- ggplot(data = activity_df, 
                                aes(x = hour, y = freq, fill = clade)) +
  geom_bar(color="black",
           stat = "identity",
           position = position_nudge (x=.5),
           width = 1
          )+
  cp+
  facet_wrap("clade", scales="free")+
  theme_bw()+
  theme(aspect.ratio = 1)+
  scale_x_continuous(limits = c(-.0001, 24), breaks = 0:23)+
  scale_fill_manual(values =c("blue", "yellow"))+
  labs(x = "", 
       y = "No. Recorded Scavenging Events")
activity_plot_all_spp

ggsave("output/diel_activity.png", 
       width = 7, height = 5, units = "in", dpi = 600)


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


