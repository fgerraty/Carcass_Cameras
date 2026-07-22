##########################################################################
# Carcass Cameras ########################################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Species Interactions ###################################################
#-------------------------------------------------------------------------

carcass_camera_data <- read_csv("data/processed/carcass_camera_data.csv")

competitive_interactions <- carcass_camera_data %>% 
  filter(event_type == "competition")


#Plot number of competitive interactions per day (TL photos only) by carcass age

plot_df <- competitive_interactions %>% 
  filter(timelapse == TRUE) %>% 
  group_by(ccam_num, carcass_age) %>% 
  summarize(n_competitive_interactions = n(),
            n_days = length(unique(day_num)),
            n_competive_interactions_per_day = n_competitive_interactions/n_days) 

ggplot(plot_df, aes(x=carcass_age, n_competive_interactions_per_day, color = ccam_num))+
  geom_point()


#Plot number of competitive interactions based on species pairs

species_pairs <- competitive_interactions %>%
  filter(timelapse == TRUE) %>% 
  #Count number of species interaction detections and days for each carcass
  group_by(ccam_num, keyword) %>% 
  summarize(n_detections = n(),
            n_days = length(unique(day_num))) %>% 
  group_by(keyword) %>% 
  summarize(n_detections = sum(n_detections),
            n_days = sum(n_days),
            detections_per_day = n_detections/n_days) %>% 
  mutate(keyword = fct_reorder(keyword, detections_per_day, .desc = TRUE)) #Turn into a factor in descending order


ggplot(species_pairs, aes(x=keyword, y=detections_per_day))+
  geom_bar(stat = "identity")

# Focal Competitive Interactions: Vultures, Ravens, Gulls ######################

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
 #LOOK INTO WHY SOME FILES DROP HERE!!!!


combined <- focal_interactions %>% 
  left_join(feeding_df, by = "file_name", relationship = "many-to-many")

#------------------------------------------------------
# 4. Summarize dominance outcomes per interaction pair
#------------------------------------------------------
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
      feeding_A & feeding_B  ~ "both_feeding",
      TRUE                   ~ "neither_feeding"
    )
  )

#Look into why we have "neither feeding" results!!!

results_table <- interaction_results %>% 
  filter(! outcome == "neither_feeding") %>% 
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
      keyword == "common raven-gull"        ~ "common raven"   # you decide
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
                          "turkey vulture-gull"         # top of plot
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
    breaks = c("turkey vulture_wins", "both_feeding_pos", "common raven_wins", "gull_wins"),
    labels = c("Turkey vulture feeding", "Both feeding", "Common raven feeding", "Gull feeding"),
    name   = NULL
  )+
  labs(x = "Proportion of competitive interactions", y = NULL,
       title    = "Outcomes of interspecific competition") +
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

