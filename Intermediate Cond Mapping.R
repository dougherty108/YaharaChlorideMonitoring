#load packages
library(tidyverse)
library(sf)
library(ggspatial)
library(lubridate)
library(dataRetrieval)
library(MetBrewer)

#Read in data and 
cond <- read_csv("Data/Chloride.csv") |> 
  mutate(`Date` = mdy(`Date`))
str(cond)


#Rename certain columns to make life easier later
names(cond)[2] <- "SITEID"
names(cond)[8] <- "spc" 
names(cond)[6] <- "longitude"
names(cond)[5] <- "latitude"

cond.sf = cond |> group_by(SITEID, latitude, longitude, .groups = TRUE) |> 
  summarise(conductivity = spc) |> 
  mutate(cond_group = case_when(conductivity < 200 ~ '0-200', 
                                conductivity < 400 ~ '200-400', 
                                conductivity < 600 ~ '400-600', 
                                conductivity < 800 ~ '600-800',
                                conductivity < 1000 ~ '800-1000',
                                conductivity >1000 ~ '>1000')) |> 
  mutate(cl_group = factor(cond_group, levels = c('0-200', '200-400', '400-600', '600-800', '800-1000', '>1000'))) |> 
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326)

################### MAPPING ###################
world_gray <-  paste0('https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/${z}/${y}/${x}.jpeg')

ggplot(cond.sf) +
  annotation_map_tile(type = world_gray, zoom = 12) +
  geom_sf(data = cond.sf, aes(fill = cond_group), size = 3, shape = 21) + 
  scale_fill_manual(values = rev(met.brewer("Manet", 5)), 
                    breaks = c('0-200', '200-400', '400-600', '600-800', '800-1000', '>1000'),
                    name = 'Specific Conducivity (uS/cm at 25 C)') +
  #scale_fill_gradient(colors=rev(met.brewer("Manet"))) +
  theme_bw(base_size = 8) + 
  annotation_north_arrow(
    location = "br", which_north = "true", 
    pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"), 
    height = unit(0.3, "in"), width = unit(0.3, "in"),
    style = north_arrow_nautical) + 
  annotation_scale(location = "tr", width_hint = 0.5, height = unit(0.05, 'in')) + #scale bar
  scale_alpha_continuous(range = c(0.1, 0.8)) +
  xlab('') + ylab('') + 
  labs(title = 'Specific Conductivity (uS/cm at 25 C)')
  



  
  
  
