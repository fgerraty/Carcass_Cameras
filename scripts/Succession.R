##########################################################################
# Carcass Cameras ########################################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Succession #############################################################
#-------------------------------------------------------------------------

scavenging_assemblages <- read_csv("data/processed/scavenging_assemblages.csv") %>% 
  mutate(carcass_age = factor(carcass_age))
  

######################################
# Assess Succession using mvabund ####
######################################

set.seed (999)

# Create mvabund object composed of all of the scavenger species and their maxN values 
scav_assemblage <- mvabund(scavenging_assemblages[,4:ncol(scavenging_assemblages)])

#take a look at the abundance data
boxplot(scavenging_assemblages[,4:ncol(scavenging_assemblages)], 
        horizontal = TRUE, las = 2, main = "Abundance")

#check mean-variance relationship
meanvar.plot(scav_assemblage)


f1 <- manyglm(scav_assemblage ~ scavenging_assemblages$carcass_age, 
              family = "negative_binomial", #negative binomial distribution
              offset = log(scavenging_assemblages$n_photos)) #offset on link (log) scale

anova.manyglm(f1, p.uni = "adjusted")

#Plot model to make sure no trend in residuals vs. fitted plot
plot(f1) #Nope, a cloud of points. 



###########################################
# Individual glmms for primary species ####
###########################################            

tuvu_mod <- glmmTMB(turkey_vulture ~ carcass_age + (1|ccam_num),
                          data = scavenging_assemblages,
                          offset = log(n_photos), 
                          family = nbinom2)
summary(tuvu_mod)


cora_mod <- glmmTMB(common_raven ~ carcass_age + (1|ccam_num),
                    data = scavenging_assemblages,
                    offset = log(n_photos), 
                    family = nbinom2)
summary(cora_mod)


ungu_mod <- glmmTMB(gull ~ carcass_age + (1|ccam_num),
                    data = scavenging_assemblages,
                    offset = log(n_photos), 
                    family = nbinom2)
summary(ungu_mod)


bird_mod <- glmmTMB(bird ~ carcass_age + (1|ccam_num),
                    data = scavenging_assemblages,
                    offset = log(n_photos), 
                    family = nbinom2)
summary(bird_mod)


#Pivot longer for plotting

scavenging_assemblages_longer <- scavenging_assemblages %>% 
  pivot_longer(cols = c(4:11),
               names_to = "species_id", 
               values_to = "detection_count") %>% 
  mutate(carcass_age = factor(carcass_age, levels = c( "1/2", "3", "4")),
         detection_proportion = detection_count/n_photos) #%>% 
  
  #Filter for species of interest
  filter(species_id %in% c("bird", "common_raven", "gull", "turkey_vulture")) %>% 
  #filter for a minimum number of photos 
  filter(n_photos >100)


scav_assemblage_plot_summary <- scavenging_assemblages_longer %>% 
  group_by(species_id, carcass_age) %>% 
  summarise(mean = mean(detection_proportion), 
            ci = 1.96 * sd(detection_proportion)/sqrt(n()))

# Plot

ggplot(scavenging_assemblages_longer, aes(x=as.character(carcass_age), 
                 y=detection_proportion, #transformed to hours
                 fill = species_id))+
  geom_point(color = "grey")+
  geom_line(color = "grey", aes(group = ccam_num))+
  geom_pointrange(data =scav_assemblage_plot_summary, aes(y=mean, 
                                                          ymin = mean-ci, 
                                                          ymax = mean+ci))+
  facet_wrap(facets = "species_id", scales = "free_y")+
  scale_y_continuous()+
  labs(y ="Proportion of time detected on carcass", 
       x = "Carcass age", 
       fill = "Species ID")+
  theme_few()+
  theme(panel.border = element_rect(linewidth = 2),
        strip.text = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position="none",
        
  )
  
ggsave("output/succession_1.png", 
       width = 7, height = 5, units = "in", dpi = 600)




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

  





#NMDS! 

scavenging_assemblages_wider <- scavenging_assemblages_longer %>% 
  pivot_wider(names_from = species_id, values_from = detection_proportion, values_fill = 0)

set.seed(99)

#pull scavenger assemblage
scav_assemblage <- data.frame(scavenging_assemblages_wider[5:ncol(scavenging_assemblages_wider)]) %>% 
  filter(rowSums(.) > 0)

nMDS <- metaMDS(scav_assemblage, k=2, trymax = 1000, maxit = 10000)

#Check stress (less than 0.1 is great)
nMDS$stress

#Extract coordinates of nMDS points
nMDS_coords <- nMDS$points

#combine nMDS coordinates with site name
nMDS_coords <- cbind(scavenging_assemblages_wider, nMDS_coords)

ggplot(data=nMDS_coords, aes(x=MDS1, y=MDS2, color = carcass_age))+
  geom_point(size=6)
  