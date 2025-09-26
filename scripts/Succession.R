##########################################################################
# Carcass Cameras ########################################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Succession #############################################################
#-------------------------------------------------------------------------

#Import "wide" community dataframes
community_df1 <- read_csv("data/processed/community_detection_rate.csv") %>% 
  mutate(CORA = as.duration(CORA),
         TUVU = as.duration(TUVU),
         UNGU = as.duration(UNGU),
         Bird = as.duration(Bird),
         small_mammal = as.duration(small_mammal),
         )
community_df2 <- read_csv("data/processed/community_scavenging_rate.csv") %>% 
  mutate(CORA = as.duration(CORA),
         TUVU = as.duration(TUVU),
         UNGU = as.duration(UNGU),
         Bird = as.duration(Bird),
         small_mammal = as.duration(small_mammal),
  )


#Pivot longer for plotting

community_df1_longer <- community_df1 %>% 
  pivot_longer(cols = c(4:8),
               names_to = "species_id", 
               values_to = "detection_duration") %>% 
  mutate(carcass_age = if_else(carcass_age %in% c(1:2), 
                               "1 & 2", 
                               as.character(carcass_age)),
          carcass_age = factor(carcass_age, levels = c( "1 & 2", "3", "4")))

community_df2_longer <- community_df2 %>% 
  pivot_longer(cols = c(4:8),
               names_to = "species_id", 
               values_to = "scavenger_duration") %>% 
  mutate(carcass_age = if_else(carcass_age %in% c(1:2), 
                               "1 & 2", 
                               as.character(carcass_age)),
         carcass_age = factor(carcass_age, levels = c( "1 & 2", "3", "4")))



plot_df <- community_df1_longer %>% 
  group_by(ccam_num, carcass_age, species_id) %>% 
  summarise(detection_duration_mean = mean(detection_duration)) %>% 
  filter(species_id != "small_mammal") %>% 
  mutate(species_id = factor(species_id, 
                             levels = c("TUVU", "CORA", "UNGU", "Bird")))

# Plot

ggplot(plot_df, aes(x=as.character(carcass_age), 
                 y= detection_duration_mean / (60*60) , #transformed to hours
                 fill = species_id))+
  geom_boxplot()+
  facet_wrap(facets = "species_id", scales = "free_y")+
  scale_y_continuous()+
  labs(y ="Time detected on carcass per day (hours)", 
       x = "Carcass age", 
       fill = "Species ID")+
  theme_few()+
  theme(panel.border = element_rect(linewidth = 2),
        strip.text = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.title=element_text(face="bold"),
  )
  


plot_df2 <- plot_df %>% 
  group_by(carcass_age, species_id) %>% 
  summarise(detection_duration_se = sd(detection_duration_mean)/sqrt(n()),
            detection_duration_mean = mean(detection_duration_mean))


# Plot

ggplot(plot_df, aes(x=carcass_age, 
                    y= detection_duration_mean / (60*60) , #transformed to hours
                    color = species_id))+
  geom_jitter(width = .2, alpha = .6, shape = 16)+
  geom_point(data = plot_df2, size = 4, alpha = 1)+
  geom_errorbar(data = plot_df2, 
                aes(ymin = ((detection_duration_mean-detection_duration_se)/(60*60)),
                    ymax = ((detection_duration_mean+detection_duration_se)/(60*60)),
                    width = .2))+
  facet_wrap(facets = "species_id", scales = "free_y", ncol = 2)+
  scale_y_continuous()+
  labs(y ="Time detected on carcass per day (hours)", 
       x = "Carcass age", 
       color = "Species ID")+
  theme_few()+
  theme(panel.border = element_rect(linewidth = 2),
        strip.text = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.title=element_text(face="bold"),)

  