

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)

deployments <- read_csv("data/raw/deployments.csv")


#Import california counties shapefile, originally downloaded from https://purl.stanford.edu/jm667wq2232
counties <- st_read("data/shapefiles/stanford-jm667wq2232-shapefile/jm667wq2232.shp") %>% 
  st_make_valid() %>% 
  st_union() 



states <- map_data("state")
ca_df <- subset(states, region == "california")

ggplot() + 
  coord_fixed(1.3) + 
  geom_polygon(data = ca_df, mapping = aes(x = long, y = lat, group = group), 
               color = "black", fill = "lightgrey") +
  geom_point(aes(y= 37.1134	, x= -122.32917),
            color = "red", size = 3, fill = NA) +
  theme_void()


ca <- ne_states(country = "united states of america", returnclass = "sf") %>%
  dplyr::filter(name == "California")




california_map <- ggplot() +
  geom_sf(data = ca, fill = "lightgrey", color = "black") +
  geom_point(data = deployments,
             aes(x = longitude, y = latitude),
             color = "red", size = 2, shape = 8) +
  coord_sf(xlim = c(-125, -114), ylim = c(32, 42))+
  theme_void()
california_map



inset_map <- ggplot() +
  geom_sf(data = counties, fill = "antiquewhite",, color = "black") +
  geom_point(data = deployments,
             aes(x = longitude, y = latitude),
             color = "black", size = 2) +
  geom_rect(aes(
    xmin = -122.319, 
    xmax = -122.341, 
    ymin = 37.109,
    ymax = 37.131),
    fill = NA, 
    colour = "black",
    linewidth = 2)+   
  coord_sf(xlim = c(-122.319, -122.341),
           ylim = c(37.109, 37.131), 
           expand = FALSE) +
  scale_x_continuous(
    breaks = c(-122.32,-122.325, -122.33,-122.335,-122.34),
    labels = c("-122.32","", "-122.33","","-122.34")) +
  scale_y_continuous(
    breaks = c(37.110, 37.115, 37.120, 37.125, 37.130),
    labels = c("37.11", "", "37.12", "", "37.13"),)+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(color = "white", linewidth = 0.5),
        panel.grid.minor = element_line(color = "white", linewidth = 0.3),
        panel.background = element_rect(fill = "#BDE8FE"),
        plot.background = element_rect(fill='transparent', color=NA))+
  # Add scale bar
  annotation_scale(location = "bl", width_hint = 0.2)+
  # Add north arrow
  annotation_north_arrow(
    location = "bl", which_north = "true",
    height = unit(1, "cm"), width = unit(1, "cm"),
    pad_y = unit(.75, "cm"),
    style = north_arrow_fancy_orienteering())
inset_map



ggsave("output/california_map.png", california_map, 
       width = 1.5, height = 2, units = "in", dpi = 600)
ggsave("output/inset_map.png", inset_map, 
       width = 3, height = 3, units = "in", dpi = 600)
