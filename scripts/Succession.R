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
  select(ccam_num, beach, year) |> 
  unique()
  
scavenging_assemblage_rates <- read_csv("data/processed/scavenging_assemblage_rates.csv") |> 
  mutate(carcass_age = factor(carcass_age)) |> 
  left_join(deployment_metadata_lookup, by = join_by(ccam_num)) |> 
  mutate(insectivorous_bird = killdeer+savannah_sparrow+song_sparrow+
                                  white_crowned_sparrow+european_starling+ 
                                  semipalmated_plover+black_phoebe)

########################################
# Assess succession using PERMANOVA ####
########################################

set.seed(999)

scav_assemblage <- scavenging_assemblage_rates |> 
  select(common_raven:mule_deer)

predictors <- scavenging_assemblage_rates |> 
  select(1:2) |> 
  left_join(deployment_metadata_lookup, by = join_by(ccam_num)) |> 
  mutate(year = factor(year),
         carcass_age = factor(carcass_age),
         ccam_num = factor(ccam_num))

#Generate bray-curtis dissimilarity matrix
distance_matrix <- vegdist(scav_assemblage, method = "bray")
dist_mat_full <- as.matrix(distance_matrix)

#Betadisper assessment
disp_beach <- betadisper(distance_matrix, predictors$beach)
disp_year <- betadisper(distance_matrix, predictors$year)
disp_age  <- betadisper(distance_matrix, predictors$carcass_age)

# permutation test 
permutest(disp_beach, permutations = 9999)
permutest(disp_year, permutations = 9999)
permutest(disp_age, permutations = 9999)



#Full model
m1 <- adonis2(distance_matrix ~ beach, data = predictors,
              permutations = 9999, by = "terms")
m1

m1 <- adonis2(distance_matrix ~ carcass_age, data = predictors,
              permutations = 9999, by = "terms",
              strata = predictors$ccam_num)
m1

##########################################
# Ordination visualization ###############
##########################################

ord <- cmdscale(distance_matrix, k = 2, eig = TRUE)

# Base R version
plot(ord$points, col = as.numeric(as.factor(predictors$year)), pch = 19,
     xlab = "PCoA1", ylab = "PCoA2", main = "Assemblage composition by year")
legend("topright", legend = levels(as.factor(predictors$year)),
       col = 1:length(unique(predictors$year)), pch = 19)

# vegan version with convex hulls (clearer for seeing spread vs. separation)
ordiplot(ord, type = "n", main = "Assemblage composition by year")
ordihull(ord, predictors$year, col = 1:3, draw = "polygon", alpha = 60, label = TRUE)
points(ord$points, col = as.numeric(as.factor(predictors$year)), pch = 19)





###########################################
# Individual glmms for primary species ####
###########################################            

tuvu_mod <- glmmTMB(turkey_vulture ~ carcass_age + (1|ccam_num),
                          data = scavenging_assemblage_rates,
                    ziformula = ~ 1,
                    family = beta_family())

summary(tuvu_mod)

# Check assumptions with DHARMa package
tuvu_mod_res = simulateResiduals(tuvu_mod)
plot(tuvu_mod_res, rank = T)
testDispersion(tuvu_mod_res)
plotResiduals(tuvu_mod_res, factor(scavenging_assemblage_rates$ccam_num), xlab = "carcass #", main=NULL)
testZeroInflation(tuvu_mod_res)


cora_mod <- glmmTMB(common_raven ~ carcass_age + (1|ccam_num),
                    data = scavenging_assemblage_rates,
                    ziformula = ~ 1,
                    family = beta_family())
summary(cora_mod)

# Check assumptions with DHARMa package
cora_mod_res = simulateResiduals(cora_mod)
plot(cora_mod_res, rank = T)
testDispersion(cora_mod_res)
plotResiduals(cora_mod_res, factor(scavenging_assemblage_rates$ccam_num), xlab = "carcass #", main=NULL)


cora_mod_simple <- glmmTMB(common_raven ~ carcass_age,
                           data = scavenging_assemblage_rates,
                           ziformula = ~ 1, family = beta_family())
AIC(cora_mod, cora_mod_simple)


ungu_mod <- glmmTMB(gull ~ carcass_age + (1|ccam_num),
                    data = scavenging_assemblage_rates,
                    ziformula = ~ 1,
                    family = beta_family())
summary(ungu_mod)

# Check assumptions with DHARMa package
ungu_mod_res = simulateResiduals(ungu_mod)
plot(ungu_mod_res, rank = T)
testDispersion(ungu_mod_res)
plotResiduals(ungu_mod_res, factor(scavenging_assemblage_rates$ccam_num), xlab = "carcass #", main=NULL)
testZeroInflation(ungu_mod_res)
testOutliers(ungu_mod_res)


bird_mod <- glmmTMB(insectivorous_bird ~ carcass_age,
                    data = scavenging_assemblage_rates,
                    ziformula = ~ 1,
                    family = beta_family())
summary(bird_mod)

table(scavenging_assemblage_rates$carcass_age, 
      scavenging_assemblage_rates$insectivorous_bird == 0)


bird_data_reduced <- scavenging_assemblage_rates |>
  filter(carcass_age != "1/2") |>
  droplevels()

bird_mod_reduced <- glmmTMB(insectivorous_bird ~ carcass_age,
                            data = bird_data_reduced,
                            ziformula = ~ 1,
                            family = beta_family())
summary(bird_mod_reduced)


bird_mod_reduced_res <- simulateResiduals(bird_mod_reduced)
plot(bird_mod_reduced_res, rank = TRUE)
testDispersion(bird_mod_reduced_res)
testZeroInflation(bird_mod_reduced_res)

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

  


library(vegan)
library(ggrepel)

#NMDS! 

scavenging_assemblages_wider <- scavenging_assemblages_longer %>% 
  select(-n_photos, -detection_count) |> 
  pivot_wider(names_from = species_id, values_from = detection_proportion, values_fill = 0)

set.seed(99)

#pull scavenger assemblage
scav_assemblage <- data.frame(scavenging_assemblages[3:ncol(scavenging_assemblages_wider)]) %>% 
  filter(rowSums(.) > 0)

nMDS <- metaMDS(scav_assemblage, k=2, trymax = 1000, maxit = 10000)

#Check stress (less than 0.1 is great)
nMDS$stress

#Extract coordinates of nMDS points
nMDS_coords <- nMDS$points

#combine nMDS coordinates with site name
nMDS_coords <- cbind(predictors, nMDS_coords)

ggplot(data=nMDS_coords, aes(x=MDS1, y=MDS2, color = carcass_age))+
  geom_point(size=6)
  

fit <- envfit(nMDS, scav_assemblage, permutations = 999)
fit  # shows r2 and p-value per species



fit <- envfit(nMDS, scav_assemblage, permutations = 999)

vectors_df <- as.data.frame(scores(fit, "vectors")) * ordiArrowMul(fit)
vectors_df$species <- rownames(vectors_df)
vectors_df$r2 <- fit$vectors$r
vectors_df$pval <- fit$vectors$pvals

# keep only meaningful fits
vectors_df_sig <- vectors_df %>% filter(pval < 0.05)

vectors_df_sig

library(viridis)

species_df <- as.data.frame(species_scores)
species_df$species <- rownames(species_df)

nmds_nolabel <- ggplot(data = nMDS_coords, aes(x = MDS1, y = MDS2, color = carcass_age)) +
  geom_point(size = 4, shape=16, alpha=.9) +
  scale_color_manual(labels = c("Fresh", "Moderate", "Old"), 
                     values = c("#dc267f","#648fff", "#ffb000"))+
  labs(x="NMDS1", y="NMDS2", color = "Carcass Age")+
  coord_cartesian(xlim = c(-2, 8.5))+
  theme_few()+
  theme(panel.border = element_rect(linewidth = 2),
               axis.title = element_text(face = "bold"),
               legend.title = element_text(face = "bold"),)
  

nmds_label <- ggplot(data = nMDS_coords, aes(x = MDS1, y = MDS2, color = carcass_age)) +
  geom_point(size = 4, shape=16, alpha=.9) +
  geom_point(data = species_df, aes(x = NMDS1, y = NMDS2), inherit.aes = FALSE,
             shape = 17, size = 3, color = "black") +
  geom_text_repel(data = species_df, aes(x = NMDS1, y = NMDS2, label = species), 
                  inherit.aes = FALSE, vjust = -0.5, size = 3)+
  scale_color_manual(labels = c("Fresh", "Moderate", "Old"), 
                     values = c("#dc267f","#648fff", "#ffb000"))+
  labs(x="NMDS1", y="NMDS2", color = "Carcass Age")+
  coord_cartesian(xlim = c(-2, 8.5))+
  theme_few()+
  theme(panel.border = element_rect(linewidth = 2),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),)


ggsave("output/nmds_nolabel.png", nmds_nolabel, 
       width = 8, height = 6, units = "in", dpi = 600)

ggsave("output/nmds_label.png", nmds_label, 
       width = 8, height = 6, units = "in", dpi = 600)






#Plot for talk

plot_df <- scavenging_assemblages %>% 
  pivot_longer(cols = c(4:18),
               names_to = "species_id", 
               values_to = "detection_count") %>% 
  mutate(carcass_age = factor(carcass_age, levels = c( "1/2", "3", "4")),
         detection_proportion = detection_count/n_photos,
         species_id = if_else(
           species_id %in% c("killdeer", "savannah_sparrow", "song_sparrow",
                             "white_crowned_sparrow", "european_starling",
                             "semipalmated_plover","black_phoebe"), 
           "insectivorous_bird", species_id)) |> 
  group_by(ccam_num, carcass_age, n_photos, species_id) |> 
  summarise(count = sum(detection_count), .groups="drop") |> 
  mutate(detection_rate = count/n_photos) |> 
  filter(species_id %in% c("turkey_vulture", "insectivorous_bird")) |> 
  mutate(species_id = case_when(
          species_id == "turkey_vulture"~"Turkey Vulture", 
          species_id == "insectivorous_bird" ~ "Insectivorous Bird"),
          species_id = factor(species_id, 
                             levels = c("Turkey Vulture", "Insectivorous Bird")))

plot_df_summary <- plot_df %>% 
  group_by(species_id, carcass_age) %>% 
  summarise(mean = mean(detection_rate), 
            ci = 1.96 * sd(detection_rate)/sqrt(n()))


succ_temp <- ggplot(plot_df, aes(x=as.character(carcass_age), 
                    y=detection_rate, #transformed to hours
                    fill = species_id))+
  geom_point(color = "grey")+
  geom_pointrange(data =plot_df_summary, aes(y=mean, 
                                             ymin = mean-ci, 
                                             ymax = mean+ci))+
  facet_wrap(facets = "species_id", scales = "free_y")+
  scale_x_discrete(labels = c("Fresh", "Moderate", "Old"))+
  labs(y ="Proportion of time detected on carcass", 
       x = "Carcass age", 
       fill = "Species ID")+
  theme_few()+
  theme(panel.border = element_rect(linewidth = 2),
        strip.text = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position="none",)


ggsave("output/succ_temp.png", succ_temp, 
       width = 8, height = 5, units = "in", dpi = 600)
