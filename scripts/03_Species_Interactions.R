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
comp_glmer <- glmmTMB(
  cbind(n_competitive_interactions, 
        n_photos - n_competitive_interactions) ~
    carcass_age + (1 | ccam_num),
  family = betabinomial,
  data = competition_over_time)
summary(comp_bb)


# Check assumptions with DHARMa package
comp_glmer_res = simulateResiduals(comp_glmer)
plot(comp_glmer_res, rank = T)
testDispersion(comp_glmer_res)
plotResiduals(comp_glmer_res, factor(competition_over_time$ccam_num), xlab = "carcass #", main=NULL)

#emmeans model summary
model_means <- emmeans(comp_bb, ~ carcass_age, type = "response") |>
  as.data.frame()
model_means
pairs(emmeans(comp_bb, ~ carcass_age))


#Plot 

competition_over_time_plot_df <- competition_over_time %>%
  group_by(ccam_num) %>%
  mutate(carcass_age_jit = as.numeric(carcass_age) + runif(1, -0.2, 0.2)) %>%
  ungroup()


competition_over_time_plot <- ggplot(competition_over_time_plot_df, 
                                       aes(x = carcass_age_jit, 
                                       y = prop_photos_competition, 
                                       group = ccam_num)) +
  geom_line(color = "grey80", alpha = .4) +
  geom_point(shape = 16, aes(color = carcass_age, size = n_photos), alpha = .5) +
  geom_errorbar(data = model_means, 
                aes(x = as.numeric(carcass_age), 
                    y = prob, 
                    ymin = asymp.LCL, ymax = asymp.UCL), 
                inherit.aes = FALSE, width =0, 
                linewidth = 1
  ) +
  geom_point(data = model_means, aes(x = as.numeric(carcass_age), 
                                     y = prob), 
             inherit.aes = FALSE, size = 5) +
  geom_point(data = model_means, aes(x = as.numeric(carcass_age), 
                                     y = prob, 
                                     color = carcass_age), 
             inherit.aes = FALSE, size = 3) +
  scale_x_continuous(breaks = c(1,2,3), labels = c("Fresh", "Moderate", "Old"))+
  scale_color_manual(labels = c("Fresh", "Moderate", "Old"), 
                     values = c("#648FFF","#FFB000", "#DC267F"))+
  labs(x = "Carcass age", 
       y = "Proportion of photos documenting\ncompetitive interactions",
       size = "Number of\nPhotos") +
  guides(color = "none")+
  theme_few() +
  theme(axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"),
        panel.border = element_rect(linewidth = 2),
        axis.title = element_text(face = "bold"),
        legend.position = "inside", 
        legend.position.inside = c(.85, .8)
        )

competition_over_time_plot

ggsave("output/competition_over_time.png", competition_over_time_plot,
        width = 5, height = 5, units = "in", dpi = 600)

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
  "turkey vulture_wins" = "#d95f02",
  "both_feeding_pos"    = "gray",
  "both_feeding_neg"    = "gray",
  "common raven_wins"   = "#1b9e77",
  "common_raven_beats_gull"   = "#1b9e77",
  "gull_wins"           = "#7570b3"
)


competitive_interactions_plot <- ggplot(diverging, aes(x = prop_signed, 
                                                       y = keyword, 
                                                       fill = outcome)) +
  geom_col(width = 0.7, position = position_stack(reverse = TRUE)) +
  geom_vline(xintercept = 0, linewidth = 0.9, color = "grey20") +
  geom_text(data = side_labels,
            aes(x =  0.81, y = keyword, label = str_to_title(right_sp)),
            hjust = 0, size = 3.2, fontface = "italic", color = "grey30",
            vjust = 3, inherit.aes = FALSE) +
  geom_text(data = side_labels,
            aes(x = -0.56, y = keyword, label = str_to_title(left_sp)),
            hjust = 1, size = 3.2, fontface = "italic", color = "grey30",
            vjust = 3, inherit.aes = FALSE) +
  geom_text(aes(label = n_label),
            position = position_stack(vjust = 0.5, reverse = TRUE),
            size = 3, color = "white", fontface = "bold", na.rm = TRUE)+
  scale_x_continuous(
    labels   = ~ scales::percent(abs(.x), accuracy = 1),
    limits   = c(-1, 1.3),
    breaks   = seq(-0.5, 0.75, 0.25),
    expand   = c(0, 0)) +
  scale_fill_manual(
    values = pal,
    breaks = c("turkey vulture_wins",  "common raven_wins", "gull_wins", "both_feeding_pos"),
    labels = c("Turkey vulture\nfeeding", "Common raven\nfeeding", "Gull feeding", "Both feeding"),
    name   = NULL
  )+
  labs(x = "Proportion of competitive interactions", y = NULL,) +
  theme_minimal() +
  theme(
    legend.position    = "bottom",
  #  legend.key.spacing.y = unit(8, "pt"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.y        = element_blank(),
    axis.ticks.y       = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    )
competitive_interactions_plot

ggsave("output/competitive_interactions.png", 
       width = 5, height = 5, units = "in", dpi = 600)

