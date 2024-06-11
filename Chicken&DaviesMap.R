# for maps
install.packages("remotes")
remotes::install_github("https://github.com/open-AIMS/gisaimsr")
library(raster) # for map
library(ggspatial) 
library(sf)
library(dataaimsr)
library(gisaimsr)
library(ggrepel)
library(rgeos)
library(rnaturalearth)
library("readxl")
library(ggplot2)
library(remotes)
install.packages("rnaturalearthdata")
library(rnaturalearthdata)


coordsmap <- read_excel("D:/AIMS/Master Thesis/Data/Coordinates.xlsx")

Coord <- data.frame(Coord = ("Coord"), lat = -18.7, lon = 147.65)

gbr_feat
FNGBR = ggplot() +
  geom_sf(data = gbr_feat, lwd = 0) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.25, "in"), pad_y = unit(0.4, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(146, 149), ylim = c(-19.5, -18)) +
  geom_point(data = coordsmap, mapping = aes(x = lon, y = lat), size=2.8) +
  geom_text_repel(data = coordsmap, 
                   mapping = aes(x = lon, y = lat,
                                 label = Reef),
                   vjust=4,
                   hjust=2,
                   segment.size = 0.4,
                   size = 4, family='Product Sans') +
  theme(axis.title = element_text(family="Product Sans"),
        axis.text = element_text(family="Imprima"),
        legend.title = element_text(family="Product Sans"),
        legend.text = element_text(family="Product Sans"),
        panel.background = element_rect(fill = "#E6F5F7"),
        panel.border = element_rect(fill = "transparent"),
        panel.grid = element_blank())+
   labs(x = "Longitude",
       y = "Latitude",
       title = "",
       subtitle = "")
FNGBR

?geom_sf

oz <- ne_countries(country = "Australia", scale='small',returnclass = 'sf')

inset = ggplot() +
  geom_sf(data = oz, fill = "grey80", col = "transparent") +
  geom_point(data = Coord, mapping = aes(x = lon, y = lat), size=7, colour="grey20")+
  theme_void() +
  theme(legend.position = "none") 
inset

 #Australia
annotated_map = FNGBR + 
  annotation_custom(ggplotGrob(inset), xmin = 148, xmax = 149, ymin = -18.8, ymax = -18)
  
g1 <- annotated_map
g1


#ggsave("HeatStressMethodsMap.jpeg")