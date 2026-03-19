##########################################################################
# Carcass Cameras ########################################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Overlap ################################################################
#-------------------------------------------------------------------------

carcass_camera_photo_data_raw <- read_csv("data/raw/carcass_camera_photo_data_raw.csv")

overlap_df <- carcass_camera_photo_data_raw %>% 
  filter(timelapse == TRUE) %>% 
  #Remove species interactions
  filter(!str_detect(keyword, "-")) %>% 
  #Remove unidentified large birds
  filter(!str_detect(keyword, "TUVU/CORA/AMCR")) %>% 
  mutate(
    # split into two parts: number (if present) and species ID
    count = as.numeric(str_extract(keyword, "^[0-9]+")),         # extract leading number
    species_id = str_remove(keyword, "^[0-9]+\\s*")) %>%   # remove leading number + space
  filter(species_id %in% c("TUVU", "CORA", "UNGU")) %>% 
  mutate(
    # extract time of day as decimal hours
    hour = hour(date_time) + minute(date_time) / 60,
    # convert to radians
    time_rad = hour / 24 * 2 * pi
  )


species1 <- overlap_df %>% filter(species_id == "CORA")  # e.g., Common Raven
species2 <- overlap_df %>% filter(species_id == "TUVU")  # e.g., Turkey Vulture
species3 <- overlap_df %>% filter(species_id == "UNGU") # e.g., Turkey Vulture

# Plot kernel density of activity times
overlapPlot(species1$time_rad, species2$time_rad, main = "CORA vs TUVU")
# Plot kernel density of activity times
overlapPlot(species2$time_rad, species3$time_rad, main = "TUVU vs UNGU")

overlapPlot(species1$time_rad, species3$time_rad, main = "CORA vs UNGU")



# Choose estimator based on sample size
n1 <- nrow(species1)
n2 <- nrow(species2)

estimator <- ifelse(min(n1, n2) < 50, "Dhat1", "Dhat4")

overlap_est <- overlapEst(species1$time_rad, species2$time_rad, type = estimator)
overlap_est


ggplot(overlap_df, aes(x = time_rad, color = species_id)) +
  geom_density(adjust = 1.5) +
  facet_wrap(facets = "carcass_age")+
  scale_x_continuous(
    breaks = seq(0, 2*pi, length.out = 5),
    labels = c("Midnight", "6am", "Noon", "6pm", "Midnight")
  ) +
  theme_minimal() +
  labs(x = "Time of day", y = "Density", title = "Species diel activity patterns")

