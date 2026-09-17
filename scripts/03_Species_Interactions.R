##########################################################################
# Carcass Cameras ########################################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 03: Species Interactions ########################################
#-------------------------------------------------------------------------
set.seed(999)

carcass_camera_data <- read_csv("data/processed/carcass_camera_data.csv")

competitive_interactions <- carcass_camera_data %>% 
  filter(event_type == "competition")

#########################################################################
# How does competition for carcasses vary by carcass age? ###############
#########################################################################

competition_over_time <- carcass_camera_data %>% 
  filter(timelapse == TRUE) %>% 
  #Group carcass age stages 1 and 2
  mutate(carcass_age = if_else(carcass_age %in% c(1,2), "1/2", 
                               as.character(carcass_age)), 
         #Turn into a factor
         carcass_age = factor(carcass_age, levels = c("1/2","3","4"))) %>% 
  mutate(competition = if_else(event_type == "competition", TRUE, FALSE),
         ccam_num = as.factor(ccam_num)) %>% 
  group_by(ccam_num, carcass_age) %>% 
  summarize(n_competitive_interactions = n_distinct(file_name[competition==TRUE]),
            n_photos = length(unique(file_name)),
            n_days = length(unique(day_num)),
            n_competive_interactions_per_day = n_competitive_interactions/n_days,
            prop_photos_competition = n_competitive_interactions / n_photos,
            .groups = "drop") %>% 
  #Filter for only carcasses (carcass-age combos) with >100 monitoring photos (e.g. ~12 hrs)
  filter(n_photos > 100)


#Linear mixed effects model 
comp_glmer <- glmmTMB(n_competitive_interactions ~ carcass_age + offset(log(n_photos)) +
                        (1 | ccam_num),
                     family = nbinom1,
                     data = competition_over_time)
summary(comp_glmer)

# Check assumptions with DHARMa package
comp_glmer_res = simulateResiduals(comp_glmer)
plot(comp_glmer_res, rank = T)
testDispersion(comp_glmer_res)
plotResiduals(comp_glmer_res, factor(competition_over_time$ccam_num), xlab = "carcass #", main=NULL)

#emmeans model summary
mean_photos <- mean(competition_over_time$n_photos)
model_means <- emmeans(comp_glmer, ~ carcass_age, type = "response",
                       offset = log(mean(competition_over_time$n_photos))) |> 
  as.data.frame() |> 
  mutate(response_prop = response / mean_photos,
         LCL_prop = asymp.LCL / mean_photos,
         UCL_prop = asymp.UCL / mean_photos)


#Plot 

competition_over_time_plot_df <- competition_over_time %>%
  group_by(ccam_num) %>%
  mutate(carcass_age_jit = as.numeric(carcass_age) + runif(1, -0.1, 0.1)) %>%
  ungroup()


competition_over_time_plot <- ggplot(competition_over_time_plot_df, aes(x = carcass_age_jit, 
                                       y = prop_photos_competition, 
                                       group = ccam_num)) +
  geom_line(color = "grey80", alpha = .7) +
  geom_point(color = "grey80") +
  geom_point(data = model_means, aes(x = as.numeric(carcass_age), 
                                     y = response_prop), 
             inherit.aes = FALSE, size = 3) +
  geom_errorbar(data = model_means, 
                aes(x = as.numeric(carcass_age), 
                    y = response_prop, 
                    ymin = LCL_prop, ymax = UCL_prop), 
                inherit.aes = FALSE, width = 0) +
  scale_x_continuous(breaks = c(1,2,3), labels = c("Fresh", "Moderate", "Old"))+
  labs(x = "Carcass Age", y = "Proportion of photos documenting\ncompetitive interactions") +
  theme_few() +
  theme(axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"),
        panel.border = element_rect(linewidth = 2),
        axis.title = element_text(face = "bold"))

competition_over_time_plot

ggsave("output/competition_over_time.png", competition_over_time_plot,
        width = 8.5, height = 5, units = "in", dpi = 600)


#Characterize number of competitive interactions based on species pairs

species_pairs <- competitive_interactions %>%
  filter(timelapse == TRUE) %>% 
  #Count number of species interaction detections and days for each carcass
  group_by(ccam_num, keyword) %>% 
  summarize(n_detections = n(),
            n_days = length(unique(day_num)),
            .groups = "drop") %>% 
  group_by(keyword) %>% 
  summarize(n_detections = sum(n_detections),
            n_days = sum(n_days),
            detections_per_day = n_detections/n_days,
            .groups = "drop") %>% 
  mutate(keyword = fct_reorder(keyword, detections_per_day, .desc = TRUE)) #Turn into a factor in descending order


################################################################################
# Focal Competitive Interactions: Vultures, Ravens, Gulls ######################
################################################################################

focal_interactions <- competitive_interactions %>% 
  filter(keyword %in% c("turkey vulture-common raven", "turkey vulture-gull", "common raven-gull")) %>% 
  separate_wider_delim(cols= "keyword",
                       delim= "-",
                       names=c("species_A", "species_B"),
                       cols_remove = FALSE) %>% 
  select(file_name, keyword, species_A, species_B)


# Identify feeding species 

feeding_df <- carcass_camera_data %>% 
  filter(event_type == "scavenging",
         species_1 %in% c("turkey vulture", "common raven", "gull"))%>% 
  distinct(file_name, species_1) %>% 
  rename(feeding_species = species_1)


combined <- focal_interactions %>% 
  left_join(feeding_df, by = "file_name", relationship = "many-to-many")

#Check to make sure that all documented competitive interactions had associated feeding species
length(unique(focal_interactions$file_name)) == length(unique(combined$file_name))


#Summarize dominance outcomes per interaction pair
interaction_events <- combined %>% 
  group_by(file_name, keyword, species_A, species_B) %>% 
  summarise(
    feeding_A = any(species_A %in% feeding_species),
    feeding_B = any(species_B %in% feeding_species),
    .groups = "drop"
  )


interaction_results <- interaction_events %>% 
  mutate(
    outcome = case_when(
      feeding_A & !feeding_B ~ paste0(species_A, "_wins"),
      feeding_B & !feeding_A ~ paste0(species_B, "_wins"),
      feeding_A & feeding_B  ~ "both_feeding"))


results_table <- interaction_results %>% 
  group_by(keyword, outcome) %>% 
  summarise(
    n = n(),
    .groups = "drop_last"
  ) %>% 
  mutate(
    prop = n / sum(n)
  ) %>% 
  ungroup()

print(results_table)


#Plot! 

diverging <- results_table %>%
  separate(keyword, into = c("sp1", "sp2"), sep = "-", remove = FALSE) %>%
  # Manually define which species is the "right" (positive) side per pair
  mutate(
    right_sp = case_when(
      str_detect(keyword, "turkey vulture") ~ "turkey vulture",
      keyword == "common raven-gull"        ~ "common raven"
    ),
    left_sp = case_when(
      keyword == "turkey vulture-gull"         ~ "gull",
      keyword == "turkey vulture-common raven" ~ "common raven",
      keyword == "common raven-gull"           ~ "gull"
    ),
    direction = case_when(
      outcome == "both_feeding"                     ~ "neutral",
      str_detect(outcome, paste0(right_sp, "_wins")) ~ "positive",
      TRUE                                           ~ "negative"
    )
  ) %>%
  bind_rows(
    filter(., outcome == "both_feeding") %>%
      mutate(prop = prop / 2, outcome = "both_feeding_neg", direction = "negative"),
    filter(., outcome == "both_feeding") %>%
      mutate(prop = prop / 2, outcome = "both_feeding_pos", direction = "positive")
  ) %>%
  filter(outcome != "both_feeding") %>%
  mutate(
    prop_signed = if_else(direction == "negative", -prop, prop),
    outcome = fct_relevel(outcome,
                          # negative side: both_feeding_neg innermost (first), then winners outermost
                          "both_feeding_neg", "gull_wins", "common raven_wins",
                          # positive side: both_feeding_pos innermost, turkey vulture outermost  
                          "both_feeding_pos", "turkey vulture_wins"),
    #relabel so that plot displays correctly
    outcome = if_else(outcome == "common raven_wins" & sp2 == "gull", "common_raven_beats_gull", outcome), 
    keyword = fct_relevel(keyword,
                          "common raven-gull",
                          "turkey vulture-common raven",
                          "turkey vulture-gull"
    )
  ) %>% 
  mutate(n_label = if_else(outcome %in% c("both_feeding_neg", "both_feeding_pos"), NA, n))

# Update side labels to use right_sp / left_sp
side_labels <- diverging %>%
  distinct(keyword, right_sp, left_sp)

pal <- c(
  "turkey vulture_wins" = "#dc267f",
  "both_feeding_pos"    = "gray",
  "both_feeding_neg"    = "gray",
  "common raven_wins"   = "#648fff",
  "common_raven_beats_gull"   = "#648fff",
  "gull_wins"           = "#ffb000"
)


ggplot(diverging, aes(x = prop_signed, y = keyword, fill = outcome)) +
  geom_col(width = 0.6, position = position_stack(reverse = TRUE)) +
  geom_vline(xintercept = 0, linewidth = 0.9, color = "grey20") +
  geom_text(data = side_labels,
            aes(x =  0.82, y = keyword, label = str_to_title(right_sp)),
            hjust = 0, size = 3.2, fontface = "italic", color = "grey30",
            inherit.aes = FALSE) +
  geom_text(data = side_labels,
            aes(x = -0.82, y = keyword, label = str_to_title(left_sp)),
            hjust = 1, size = 3.2, fontface = "italic", color = "grey30",
            inherit.aes = FALSE) +
  geom_text(aes(label = n_label),
            position = position_stack(vjust = 0.5, reverse = TRUE),
            size = 3, color = "white", fontface = "bold", na.rm = TRUE)+
  scale_x_continuous(
    labels   = ~ scales::percent(abs(.x), accuracy = 1),
    limits   = c(-1.05, 1.05),
    breaks   = seq(-0.75, 0.75, 0.25),
    expand   = c(0, 0)
  ) +
  scale_fill_manual(
    values = pal,
    breaks = c("turkey vulture_wins",  "common raven_wins", "gull_wins", "both_feeding_pos"),
    labels = c("Turkey vulture feeding", "Common raven feeding", "Gull feeding", "Both feeding"),
    name   = NULL
  )+
  labs(x = "Proportion of competitive interactions", y = NULL,) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position    = "bottom",
    legend.key.size    = unit(0.45, "cm"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.title         = element_text(face = "bold", size = 14),
    plot.subtitle      = element_text(color = "grey50", size = 11),
    axis.text.y        = element_blank(),   # replaced by side labels
    axis.ticks.y       = element_blank()
  )

ggsave("output/competitive_interactions.png", 
       width = 8.5, height = 5, units = "in", dpi = 600)

