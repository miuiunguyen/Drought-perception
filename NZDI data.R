rm(list=ls())
setwd("E:/1. PHD/Data/data NZDI")
getwd()
###Loading packages
library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
##Visualization packages
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(haven)
library(dplyr)
library(tidyr)
library(tibble)
library(readxl)
library(foreign)
library(MASS)
library(nnet)
library(ggmap)
library(RColorBrewer)
nzdi <- read_excel("data_fo_maps-nz.xlsx", sheet = 1)
library(nzcensr)
library(ggrepel)

## Tmap basic
tas_simple_1000 <- st_simplify(tas, dTolerance = 1000)
tas_simple <- rename(tas_simple_1000, district = TA2013_NAM)
nzdi_map <- left_join(tas_simple, nzdi, by.x = "district")

d <- tm_shape(nzdi_map) + tm_borders() + tm_fill(col = "d_total", title = "D Occurrences") + 
 tm_legend(legend.title.fontface = 2,  # legend bold
           legend.title.size = 0.75, 
           legend.text.size = 0.65, 
           legend.bg.alpha = 0) 
sd <- tm_shape(nzdi_map) + tm_borders() + tm_fill(col = "sd_total", title = "SD Occurrences")+ 
 tm_legend(legend.title.fontface = 2,  # legend bold
           legend.title.size = 0.75, 
           legend.text.size = 0.65, 
           legend.bg.alpha = 0, 
           legend.width = 5)
d_total <- tm_shape(nzdi_map) + tm_borders() + tm_fill(col = "drought", title = "D&SD Occurrences") +
 tm_legend(legend.title.fontface = 2,  # legend bold
           legend.title.size = 0.75, 
           legend.text.size = 0.65, 
           legend.bg.alpha = 0, 
           legend.width = 5)
tmap_arrange(d, sd, d_total)
d + tm_compass() + tm_scale_bar()

#Draw the maps showed number of farmers in the survey
farmers <- tm_shape(nzdi_map) + tm_borders() + tm_fill(col = "farmers", title = "Total farmers\n (N=1570)") + tm_layout(frame=F)+ 
 tm_compass() + tm_scale_bar() + tm_text(text = "farmers", size = 0.5)
farmers