##########################################################################
# Carcass Cameras ########################################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 04: Succession ##################################################
#-------------------------------------------------------------------------

deployment_metadata_lookup <- read_csv("data/raw/deployments.csv") |> 
  mutate(ccam_num = as.numeric(str_extract(carcass_id, "\\d+")),
         deploy_date_parsed = dmy(deploy_date),
         year = year(deploy_date_parsed)) |> 
  select(ccam_num, beach, year, carcass_length) |> 
  unique()
  
scavenging_assemblage_rates <- read_csv("data/processed/scavenging_assemblage_rates.csv") |>
  mutate(carcass_age = factor(carcass_age)) |> 
  left_join(deployment_metadata_lookup, by = join_by(ccam_num)) |> 
  mutate(insectivorous_bird = killdeer+savannah_sparrow+song_sparrow+
                                  white_crowned_sparrow+european_starling+ 
                                  semipalmated_plover+black_phoebe)

scavenging_assemblage_rates2 <- read_csv("data/processed/scavenging_assemblage_rates2.csv") |> 
  mutate(carcass_age = factor(carcass_age)) |> 
  left_join(deployment_metadata_lookup, by = join_by(ccam_num)) |> 
  mutate(insectivorous_bird = killdeer+savannah_sparrow+song_sparrow+
           white_crowned_sparrow+european_starling+ 
           semipalmated_plover+black_phoebe)


scavenging_assemblage_counts <- read_csv("data/processed/scavenging_assemblage_counts.csv") |> 
  mutate(insectivorous_bird = killdeer+savannah_sparrow+song_sparrow+
           white_crowned_sparrow+european_starling+ 
           semipalmated_plover+black_phoebe)

scavenging_assemblage_counts2 <- read_csv("data/processed/scavenging_assemblage_counts2.csv") |> 
  mutate(insectivorous_bird = killdeer+savannah_sparrow+song_sparrow+
           white_crowned_sparrow+european_starling+ 
           semipalmated_plover+black_phoebe)

########################################
# Assess succession using PERMANOVA ####
########################################

set.seed(999)

scav_assemblage <- scavenging_assemblage_rates2 |> 
  select(common_raven:mule_deer)

predictors <- scavenging_assemblage_rates2 |> 
  select(1:2) |> 
  left_join(deployment_metadata_lookup, by = join_by(ccam_num)) |> 
  mutate(year = factor(year),
         carcass_age = factor(carcass_age),
         carcass_length = as.numeric(carcass_length),
         ccam_num = factor(ccam_num))

#Generate bray-curtis dissimilarity matrix
distance_matrix <- vegdist(scav_assemblage, method = "bray")
dist_mat_full <- as.matrix(distance_matrix)

#Betadisper assessment
disp_beach <- betadisper(distance_matrix, predictors$beach)
disp_year <- betadisper(distance_matrix, predictors$year)
disp_length <- betadisper(distance_matrix, predictors$carcass_length)
disp_age  <- betadisper(distance_matrix, predictors$carcass_age)

# permutation test 
permutest(disp_beach, permutations = 9999)
permutest(disp_year, permutations = 9999)
permutest(disp_length, permutations = 9999)
permutest(disp_age, permutations = 9999)

#Simple succession model
m1 <- adonis2(distance_matrix ~ carcass_age, data = predictors,
              permutations = 9999, by = "terms",
              strata = predictors$ccam_num)
m1

#Full model
m2 <- adonis2(distance_matrix ~ carcass_age + beach + year + carcass_length, 
              data = predictors,
              permutations = 9999, by = "terms",
              strata = predictors$ccam_num)
m2


##########################
# Visualize with NMDS ####
##########################

nMDS <- metaMDS(scav_assemblage, k=2, trymax = 1000, maxit = 10000)

#Check stress
nMDS$stress

#Extract coordinates of nMDS points
nMDS_coords <- nMDS$points

#combine nMDS coordinates with site name
nMDS_coords <- cbind(predictors, nMDS_coords)

ggplot(data=nMDS_coords, aes(x=MDS1, y=MDS2, color = carcass_age))+
  geom_point(size=6)
  
ggplot(data=nMDS_coords, aes(x=MDS1, y=MDS2, color = beach))+
  geom_point(size=6)

ggplot(data=nMDS_coords, aes(x=MDS1, y=MDS2, color = carcass_length))+
  geom_point(size=6)

ggplot(data=nMDS_coords, aes(x=MDS1, y=MDS2, color = year))+
  geom_point(size=6)


nmds_carcass_age <- ggplot(data = nMDS_coords, aes(x = MDS1, y = MDS2, 
                                               color = carcass_age)) +
  geom_point(size = 4, shape=16, alpha=.85) +
  scale_color_manual(labels = c("Fresh", "Moderate", "Old"), 
                     values = c("#dc267f","#648fff", "#ffb000"))+
  labs(x="NMDS1", y="NMDS2", color = "Carcass Age")+
  coord_cartesian(xlim = c(-2, 5))+
  theme_few()+
  theme(panel.border = element_rect(linewidth = 2),
               axis.title = element_text(face = "bold"),
               legend.title = element_text(face = "bold"))
nmds_carcass_age
  
ggsave("output/extra_plots/nmds_carcass_age.png", nmds_carcass_age, 
       width = 5, height = 5, units = "in", dpi = 600)


###########################################
# Individual glmms for primary taxa #######
###########################################            

#Turkey vulture model
tuvu_mod <- glmmTMB(
  cbind(turkey_vulture, 
        n_photos - turkey_vulture) ~
    carcass_age + (1 | ccam_num),
  family = betabinomial,
  data = scavenging_assemblage_counts)
summary(tuvu_mod)


# Check assumptions with DHARMa package
tuvu_mod_res = simulateResiduals(tuvu_mod)
plot(tuvu_mod_res, rank = T)
testDispersion(tuvu_mod_res)
plotResiduals(tuvu_mod_res, factor(scavenging_assemblage_counts$ccam_num), xlab = "carcass #", main=NULL)

#emmeans model summary
tuvu_model_means <- emmeans(tuvu_mod, ~ carcass_age, type = "response") |>
  as.data.frame()
tuvu_model_means
pairs(emmeans(tuvu_mod, ~ carcass_age))



#Common raven model
cora_mod <- glmmTMB(
  cbind(common_raven, 
        n_photos - common_raven) ~
    carcass_age + (1 | ccam_num),
  family = betabinomial,
  data = scavenging_assemblage_counts)
summary(cora_mod)


# Check assumptions with DHARMa package
cora_mod_res = simulateResiduals(cora_mod)
plot(cora_mod_res, rank = T)
testDispersion(cora_mod_res)
plotResiduals(cora_mod_res, factor(scavenging_assemblage_counts$ccam_num), xlab = "carcass #", main=NULL)

#emmeans model summary
cora_model_means <- emmeans(cora_mod, ~ carcass_age, type = "response") |>
  as.data.frame()
cora_model_means
pairs(emmeans(cora_mod, ~ carcass_age))

#Gull model
ungu_mod <- glmmTMB(
  cbind(gull, 
        n_photos - gull) ~
    carcass_age + (1 | ccam_num),
  family = betabinomial,
  data = scavenging_assemblage_counts)
summary(ungu_mod)


# Check assumptions with DHARMa package
ungu_mod_res = simulateResiduals(ungu_mod)
plot(ungu_mod_res, rank = T)
testDispersion(ungu_mod_res)
plotResiduals(ungu_mod_res, factor(scavenging_assemblage_counts$ccam_num), xlab = "carcass #", main=NULL)

#emmeans model summary
ungu_model_means <- emmeans(ungu_mod, ~ carcass_age, type = "response") |>
  as.data.frame()
ungu_model_means
pairs(emmeans(ungu_mod, ~ carcass_age))


insectivore_mod_df <- scavenging_assemblage_counts |> 
  filter(carcass_age %in% c(3,4))


#Insectivorous bird model
insectivore_mod <- glmmTMB(
  cbind(insectivorous_bird, 
        n_photos - insectivorous_bird) ~
    carcass_age + (1 | ccam_num),
  family = betabinomial,
  data = insectivore_mod_df)
summary(insectivore_mod)


# Check assumptions with DHARMa package
insectivore_mod_res = simulateResiduals(insectivore_mod)
plot(insectivore_mod_res, rank = T)
testDispersion(insectivore_mod_res)
plotResiduals(insectivore_mod_res, factor(insectivore_mod_df$ccam_num), xlab = "carcass #", main=NULL)

#emmeans model summary
insectivore_model_means <- emmeans(insectivore_mod, ~ carcass_age, 
                                   type = "response") |>
  as.data.frame() 
insectivore_model_means
pairs(emmeans(insectivore_mod, ~ carcass_age))

insectivore_model_means_plot_df <- insectivore_model_means |> 
  add_row(carcass_age = "1/2", prob = 0, asymp.LCL = 0, asymp.UCL = 0) |> 
  mutate(carcass_age = factor(carcass_age, levels = c("1/2", "3", "4")))

############################################
# Single-taxa succession ###################
############################################

single_taxa_succession_plot_df <- scavenging_assemblage_counts |> 
  group_by(ccam_num) |> 
  mutate(carcass_age = factor(carcass_age), 
           carcass_age_jit = as.numeric(carcass_age) + runif(1, -0.2, 0.2)) |> 
  ungroup()



tuvu_plot <- ggplot(single_taxa_succession_plot_df, 
                                     aes(x = carcass_age_jit, 
                                         y = turkey_vulture/n_photos, 
                                         group = ccam_num)) +
  geom_line(color = "grey80", alpha = .4) +
  geom_point(shape = 16, aes(color = carcass_age, size = n_photos), alpha = .5) +
  geom_errorbar(data = tuvu_model_means, 
                aes(x = as.numeric(carcass_age), 
                    y = prob, 
                    ymin = asymp.LCL, ymax = asymp.UCL), 
                inherit.aes = FALSE, width =0, 
                linewidth = 1
  ) +
  geom_point(data = tuvu_model_means, aes(x = as.numeric(carcass_age), 
                                     y = prob), 
             inherit.aes = FALSE, size = 5) +
  geom_point(data = tuvu_model_means, aes(x = as.numeric(carcass_age), 
                                     y = prob, 
                                     color = carcass_age), 
             inherit.aes = FALSE, size = 3) +
  scale_x_continuous(breaks = c(1,2,3), labels = c("Fresh", "Moderate", "Old"))+
  scale_color_manual(labels = c("Fresh", "Moderate", "Old"), 
                     values = c("#648FFF","#FFB000", "#DC267F"))+
  labs(x = "Carcass age", 
       y = "Proportion of photos with turkey vultures scavenging",
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

  tuvu_plot



cora_plot <- ggplot(single_taxa_succession_plot_df, 
                      aes(x = carcass_age_jit, 
                          y = common_raven/n_photos, 
                          group = ccam_num)) +
    geom_line(color = "grey80", alpha = .4) +
    geom_point(shape = 16, aes(color = carcass_age, size = n_photos), alpha = .5) +
    geom_errorbar(data = cora_model_means, 
                  aes(x = as.numeric(carcass_age), 
                      y = prob, 
                      ymin = asymp.LCL, ymax = asymp.UCL), 
                  inherit.aes = FALSE, width =0, 
                  linewidth = 1
    ) +
    geom_point(data = cora_model_means, aes(x = as.numeric(carcass_age), 
                                            y = prob), 
               inherit.aes = FALSE, size = 5) +
    geom_point(data = cora_model_means, aes(x = as.numeric(carcass_age), 
                                            y = prob, 
                                            color = carcass_age), 
               inherit.aes = FALSE, size = 3) +
    scale_x_continuous(breaks = c(1,2,3), labels = c("Fresh", "Moderate", "Old"))+
    scale_color_manual(labels = c("Fresh", "Moderate", "Old"), 
                       values = c("#648FFF","#FFB000", "#DC267F"))+
    labs(x = "Carcass age", 
         y = "Proportion of photos with common ravens scavenging", 
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
  
cora_plot
  

  
ungu_plot <- ggplot(single_taxa_succession_plot_df, 
                      aes(x = carcass_age_jit, 
                          y = gull/n_photos, 
                          group = ccam_num)) +
    geom_line(color = "grey80", alpha = .4) +
    geom_point(shape = 16, aes(color = carcass_age, size = n_photos), alpha = .5) +
    geom_errorbar(data = ungu_model_means, 
                  aes(x = as.numeric(carcass_age), 
                      y = prob, 
                      ymin = asymp.LCL, ymax = asymp.UCL), 
                  inherit.aes = FALSE, width =0, 
                  linewidth = 1
    ) +
    geom_point(data = ungu_model_means, aes(x = as.numeric(carcass_age), 
                                            y = prob), 
               inherit.aes = FALSE, size = 5) +
    geom_point(data = ungu_model_means, aes(x = as.numeric(carcass_age), 
                                            y = prob, 
                                            color = carcass_age), 
               inherit.aes = FALSE, size = 3) +
    scale_x_continuous(breaks = c(1,2,3), labels = c("Fresh", "Moderate", "Old"))+
    scale_color_manual(labels = c("Fresh", "Moderate", "Old"), 
                       values = c("#648FFF","#FFB000", "#DC267F"))+
    labs(x = "Carcass age", 
         y = "Proportion of photos with gulls scavenging", 
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
  
  ungu_plot
  
  
  
  insectivore_plot <- ggplot(single_taxa_succession_plot_df, 
                             aes(x = carcass_age_jit, 
                                 y = insectivorous_bird/n_photos, 
                                 group = ccam_num)) +
    geom_line(color = "grey80", alpha = .4) +
    geom_point(shape = 16, aes(color = carcass_age, size = n_photos), alpha = .5) +
    geom_errorbar(data = insectivore_model_means_plot_df, 
                  aes(x = as.numeric(carcass_age), 
                      y = prob, 
                      ymin = asymp.LCL, ymax = asymp.UCL), 
                  inherit.aes = FALSE, width =0, 
                  linewidth = 1
    ) +
    geom_point(data = insectivore_model_means_plot_df, aes(x = as.numeric(carcass_age), 
                                                           y = prob), 
               inherit.aes = FALSE, size = 5) +
    geom_point(data = insectivore_model_means_plot_df, aes(x = as.numeric(carcass_age), 
                                                           y = prob, 
                                                           color = carcass_age), 
               inherit.aes = FALSE, size = 3) +
    scale_x_continuous(breaks = c(1,2,3), labels = c("Fresh", "Moderate", "Old"))+
    scale_color_manual(labels = c("Fresh", "Moderate", "Old"), 
                       values = c("#648FFF","#FFB000", "#DC267F"))+
    labs(x = "Carcass age", 
         y = "Proportion of photos with insectivorous birds scavenging", 
         size = "Number of\nPhotos") +
    guides(color = "none")+
    theme_few() +
    theme(axis.text.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"),
          panel.border = element_rect(linewidth = 2),
          axis.title = element_text(face = "bold"),
          legend.position = "inside", 
          legend.position.inside = c(.25, .8)
    )
  
  insectivore_plot
  
  
  



# OLD STUFF BELOW!!!!! #########
##################################################################################

#Pivot longer for plotting

scavenging_assemblages_longer <- scavenging_assemblages %>% 
  pivot_longer(cols = c(4:18),
               names_to = "species_id", 
               values_to = "detection_count") %>% 
  mutate(carcass_age = factor(carcass_age, levels = c( "1/2", "3", "4")),
         detection_proportion = detection_count/n_photos)




scav_assemblage_plot_summary <- scavenging_assemblages_longer %>% 
  group_by(species_id, carcass_age) %>% 
  summarise(mean = mean(detection_proportion), 
            ci = 1.96 * sd(detection_proportion)/sqrt(n()))

# Plot

plot_df <- scavenging_assemblages_longer |> 
  filter(species_id %in% c("turkey_vulture", "common_raven", "gull")) |> 
  mutate(species_id = factor(species_id, 
                             levels = c("turkey_vulture", "common_raven", "gull")))

plot_df_summary <- plot_df %>% 
  group_by(species_id, carcass_age) %>% 
  summarise(mean = mean(detection_proportion), 
            ci = 1.96 * sd(detection_proportion)/sqrt(n()))

ggplot(plot_df, aes(x=as.character(carcass_age), 
                    y=detection_proportion, #transformed to hours
                    fill = species_id))+
  geom_point(color = "grey")+
  geom_line(color = "grey", aes(group = ccam_num))+
  geom_pointrange(data =plot_df_summary, aes(y=mean, 
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









###########################################
# Old (nbinom1) configuration ####
###########################################   

#note, deleted tuvu mod

cora_mod <- glmmTMB(common_raven ~ carcass_age +
                      offset(log(n_photos)) +
                      (1 | ccam_num),
                    family = nbinom1,
                    data = scavenging_assemblage_counts2)

summary(cora_mod)

# Check assumptions with DHARMa package
cora_mod_res = simulateResiduals(cora_mod)
plot(cora_mod_res, rank = T)
testDispersion(cora_mod_res)
plotResiduals(cora_mod_res, factor(scavenging_assemblage_rates$ccam_num), xlab = "carcass #", main=NULL)



ungu_mod <- glmmTMB(gull ~ carcass_age +
                      offset(log(n_photos)) +
                      (1 | ccam_num),
                    family = nbinom1,
                    data = scavenging_assemblage_counts2)
summary(ungu_mod)

# Check assumptions with DHARMa package
ungu_mod_res = simulateResiduals(ungu_mod)
plot(ungu_mod_res, rank = T)
testDispersion(ungu_mod_res)
plotResiduals(ungu_mod_res, factor(scavenging_assemblage_rates$ccam_num), xlab = "carcass #", main=NULL)



bird_mod <- glmmTMB(insectivorous_bird ~ carcass_age +
                      offset(log(n_photos)) +
                      (1 | ccam_num),
                    family = nbinom1,
                    data = scavenging_assemblage_counts2)
summary(bird_mod)

table(scavenging_assemblage_rates$carcass_age, 
      scavenging_assemblage_rates$insectivorous_bird == 0)


bird_data_reduced <- scavenging_assemblage_counts2 |>
  filter(carcass_age != "1/2") |>
  droplevels()

bird_mod_reduced <- glmmTMB(insectivorous_bird ~ carcass_age +
                              offset(log(n_photos)) +
                              (1 | ccam_num),
                            family = nbinom1,
                            data = bird_data_reduced)
summary(bird_mod_reduced)


bird_mod_reduced_res <- simulateResiduals(bird_mod_reduced)
plot(bird_mod_reduced_res, rank = TRUE)
testDispersion(bird_mod_reduced_res)
testZeroInflation(bird_mod_reduced_res)
