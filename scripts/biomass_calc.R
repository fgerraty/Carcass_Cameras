
library(janitor)
library(ggthemes)

#Import Data
community_df2 <- read_csv("data/processed/community_scavenging_rate.csv")

scavenger_observations <- read_csv("data/raw/scavenger_observations.csv") %>% 
  clean_names() %>% 
  mutate(peck_rate = number_of_pecks/feeding_observation_time_seconds) %>% 
  drop_na(peck_rate) %>% 
  rename(species_id = scavenger_species_id) %>% 
  mutate(species_id = factor(species_id, levels = c("TUVU", "CORA", "UNGU")))


scavenger_observations_summarized <- scavenger_observations%>% 
  group_by(species_id) %>% 
  summarize(mean_peck_rate = mean(peck_rate)*60,
            se_peck_rate = sd(peck_rate)/sqrt(n())) %>% 
  mutate(species_id = factor(species_id, levels = c("TUVU", "CORA", "UNGU")))
  

beak_volume <- read_csv("data/raw/beak_volume.csv") %>% 
  filter(! Species1 == "All (12) spp.") %>%
  filter(! Code == "BAEA") %>% 
  rename(species_id = Code) %>% 
  select(species_id, g_per_peck)

#Summarize Data 

scavenging_duration <- community_df2 %>% 
  pivot_longer(cols = c(4:8),
               names_to = "species_id", 
               values_to = "scavenger_duration") %>% 
  filter(species_id %in% c("TUVU", "CORA", "UNGU")) %>% 
  select(-Human, -carcass_age) %>% 
  mutate(scavenger_duration = as.duration(scavenger_duration)/60) %>% 
  group_by(species_id, ccam_num) %>% 
  summarize(mean_minutes_scavenging = mean(scavenger_duration),
            se_minutes_scavenging = sd(scavenger_duration)/sqrt(n())) %>% 
  mutate(species_id = factor(species_id, levels = c("TUVU", "CORA", "UNGU")))



scavenging_duration_summary <- community_df2 %>% 
  pivot_longer(cols = c(4:8),
               names_to = "species_id", 
               values_to = "scavenger_duration") %>% 
  filter(species_id %in% c("TUVU", "CORA", "UNGU")) %>% 
  select(-Human, -carcass_age) %>% 
  mutate(scavenger_duration = as.numeric(as.duration(scavenger_duration))) %>% 
  group_by(species_id, ccam_num) %>% 
  summarize(mean_minutes_scavenging = mean(scavenger_duration)/60) %>% 
  group_by(species_id) %>% 
  summarize(se_minutes_scavenging = sd(mean_minutes_scavenging)/sqrt(n()),
            mean_minutes_scavenging = mean(mean_minutes_scavenging))%>% 
  mutate(species_id = factor(species_id, levels = c("TUVU", "CORA", "UNGU")))


combined <- scavenging_duration_summary %>% 
  left_join(scavenger_observations_summarized) %>% 
  left_join(beak_volume) %>% 
  mutate(g_per_day = mean_minutes_scavenging*mean_peck_rate*g_per_peck,
         species_id = factor(species_id, levels = c("TUVU", "CORA", "UNGU"))) 
  
  
  
# Peck rate plot

ggplot()+
  geom_bar(data=scavenger_observations_summarized, aes(x=species_id, y=mean_peck_rate, fill = species_id), stat = "identity")+
  geom_jitter(data= scavenger_observations, aes(x=species_id, y=peck_rate*60), color = "grey30", width = .1)+
  geom_errorbar(data=scavenger_observations_summarized, aes(x=species_id, y = mean_peck_rate,
                                                            ymin = mean_peck_rate-se_peck_rate*60, 
                                                            ymax = mean_peck_rate+se_peck_rate*60), width = .2)+
  scale_fill_manual(values = c("#666A86", "#92B6B1", "#E8DDB5"))+
  
  labs(x= "Scavenger Species", y= "Peck Rate (pecks/minute)", fill = "")+
  scale_x_discrete(labels = c("Turkey Vulture", "Common Raven", "Unidentified Gull"))+
  theme_few()+
  theme(legend.position = "",
        panel.border = element_rect(linewidth = 2),
        axis.title.x = element_text(margin = margin(t = 10)))

ggsave("output/selah_plots/peck_rate.png", width = 8, height = 5,
       units = c("in"))


#Scavenging Duration Plot
ggplot()+
  geom_bar(data=scavenging_duration_summary, aes(x=species_id, y=mean_minutes_scavenging, fill = species_id), stat = "identity")+
  geom_errorbar(data=scavenging_duration_summary, aes(x=species_id, y = mean_minutes_scavenging,
                                                            ymin = mean_minutes_scavenging-se_minutes_scavenging, 
                                                            ymax = mean_minutes_scavenging+se_minutes_scavenging), width = .2)+
  scale_fill_manual(values = c("#666A86", "#92B6B1", "#E8DDB5"))+
  
  labs(x= "Scavenger Species", y= "Active Scavenging Time (minutes/day)", fill = "")+
  scale_x_discrete(labels = c("Turkey Vulture", "Common Raven", "Unidentified Gull"))+
  theme_few()+
  theme(legend.position = "",
        panel.border = element_rect(linewidth = 2),
        axis.title.x = element_text(margin = margin(t = 10)))

ggsave("output/selah_plots/scavenger_duration.png", width = 8, height = 5,
       units = c("in"))

# Plot of biomass consumed per day
ggplot(combined, aes(x=species_id, y=g_per_day, fill = species_id))+
  geom_bar(stat = "identity") +
  labs(x= "Scavenger Species", y= "Biomass Consumed (g/day)", fill = "")+
  scale_x_discrete(labels = c("Turkey Vulture", "Common Raven", "Unidentified Gull"))+
  scale_fill_manual(values = c("#666A86", "#92B6B1", "#E8DDB5"))+
  theme_few()+
  theme(legend.position = "",
        panel.border = element_rect(linewidth = 2),
        axis.title.x = element_text(margin = margin(t = 10)))

ggsave("output/selah_plots/biomass_consumed.png", width = 8, height = 5,
       units = c("in"))