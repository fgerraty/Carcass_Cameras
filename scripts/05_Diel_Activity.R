##########################################################################
# Carcass Cameras ########################################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 05: Diel Activity ###############################################
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


#################################################################################
# Plot diel activity patterns for the two dominant clades: birds and mammals ####
#################################################################################

diel_activity_clade <- carcass_camera_data |> 
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



#Create temporary dataframe with all clade/hour combinations with "freq", the frequency of recorded scavenging events
temp_df <- data.frame(clade = rep(unique(diel_activity$clade), each = 24),
                      hour = rep(0:23, n_distinct(diel_activity$clade)),
                      freq = 0)

#Combine temporary dataframe with "activity_df" and filter to that there is a value (>= 0) for each species/hour combination
activity_df <- rbind(temp_df, diel_activity_clade)|> #combine dataframes
  group_by(clade, hour) |> #group by species + hour
  mutate(freq = max(freq)) |> #keep the maximum value for each species/hour combo
  distinct() #remove duplicates


#Set up polar coordinate system for plotting
cp <- coord_polar(start=0)
cp$is_free <- function() TRUE

# Plot by clade
activity_plot_clade <- ggplot(data = activity_df, 
                                aes(x = hour, y = freq, 
                                    fill = clade, color=clade)) +
  geom_bar(stat = "identity",
           position = position_nudge (x=.5),
           width = 1
          )+
  cp+
  facet_wrap("clade", scales="free",
             labeller = labeller(clade = c(
               "bird" = "Birds",
               "mammal" = "Mammals")))+
  theme_bw()+
  theme(aspect.ratio = 1)+
  scale_x_continuous(limits = c(-.0001, 24), breaks = 0:23)+
  scale_fill_manual(values =c("#D5AD36", "#4488BF"))+
  scale_color_manual(values = c("#9E7502", "#1A3E6B"))+
  labs(x = "", 
       y = "No. Recorded Scavenging Events")+
  theme(strip.background = element_rect(fill = "transparent"),
        strip.text = element_text(face = "bold", color = "black"),
        legend.position = "none")
activity_plot_clade

ggsave("output/diel_activity_clade.png", 
       width = 6, height = 4, units = "in", dpi = 600)


#############################################################
# Plot diel activity patterns for all individual species ####
#############################################################


diel_activity_spp <- carcass_camera_data |> 
  filter(event_type == "scavenging") |>   #Remove species interactions
  filter(!str_detect(file_name, "MT")) |> 
  filter(timelapse == TRUE) |> 
  bind_rows(independent_nocturnal) |> 
  filter(!species_1 == "northern elephant seal") |> 
  mutate(hour = hour(date_time)) |> 
  group_by(species_1, hour) |> 
  summarize(freq = n(), .groups = "drop")



#Create temporary dataframe with all clade/hour combinations with "freq", the frequency of recorded scavenging events
temp_df2 <- data.frame(species_1 = rep(unique(diel_activity_spp$species_1), each = 24),
                      hour = rep(0:23, n_distinct(diel_activity_spp$species_1)),
                      freq = 0)

#Combine temporary dataframe with "activity_df" and filter to that there is a value (>= 0) for each species/hour combination
activity_df2 <- rbind(temp_df2, diel_activity_spp)|> #combine dataframes
  group_by(species_1, hour) |> #group by species + hour
  mutate(freq = max(freq)) |> #keep the maximum value for each species/hour combo
  distinct() |>  #remove duplicates
  mutate(clade = case_when(
    species_1 %in% c("brush rabbit", "coyote", "deer mouse", "gray fox",
                     "mule deer", "rodent", "virginia opossum", "woodrat") ~ "mammal",
    species_1 == "western fence lizard" ~ "reptile",
    TRUE ~ "bird")) |> 
  #remove photos not identified to species
  filter(!species_1 %in% c("bird", "plover", "rodent", "turkey vulture/common raven/American crow"))


# Plot by clade
activity_plot_spp <- ggplot(data = activity_df2, 
                              aes(x = hour, y = freq, 
                                  fill = clade, color=clade)) +
  geom_bar(stat = "identity",
           position = position_nudge (x=.5),
           width = 1
  )+
  cp+
  facet_wrap("species_1", scales="free", ncol=5)+
  theme_bw()+
  theme(aspect.ratio = 1)+
  scale_x_continuous(limits = c(-.0001, 24), breaks = 0:23)+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+
  scale_fill_manual(values =c("#D5AD36", "#4488BF", "#A56CB9"))+
  scale_color_manual(values = c("#9E7502", "#1A3E6B", "#602A80"))+
  labs(x = "", 
       y = "No. Recorded Scavenging Events",
       fill = "Clade", color = "Clade")+
  theme(strip.background = element_rect(fill = "transparent"),
        strip.text = element_text(face = "bold", color = "black"),
        legend.position = "inside",
        legend.position.inside = c(.7, .1))
activity_plot_spp

ggsave( "output/diel_activity_spp.png", activity_plot_spp,
       width = 8.5, height = 10, units = "in", dpi = 600)

