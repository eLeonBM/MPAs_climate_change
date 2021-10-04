library(raster)
library(maptools)
library(sp)
library(sf)



# MPAs ------

mpa <-
        sf::st_read("Data/shapefiles/MPA_MX_v1.0_01032021/clean/mpas.shp") #Import MPAs shapefile

mpa <-
        tibble::rowid_to_column(mpa, "ID") #Generate row ID
mpa <- mpa[,-c(2, 4, 5, 10:15)] #Substract extra columns


#BIOLOGICAL VARIABLES------

bio <-
        list.files("Data/rasters/biological/", pattern = "*.tif$")# List raster files
bio_length <- length(bio)


#Summarize number of biological variables inside AMPs

s_bio <- stack(paste0("Data/rasters/biological/", bio))

for (i in 1:length(bio)) {
        bg <- extract(s_bio,
                      mpa,
                      fun = sum,
                      na.rm = TRUE,
                      df = TRUE)
}

biological <-
        data.frame(bg)# Data frame with biological variables per AMP sum


#Merge sum of biological variables with MPAs shp

mpa_biological <-
        merge(mpa, biological[, c("ID","coldcorals", "kelp", "seagrasses", "warmcorals")], by =
                      "ID")




#HEATWAVES-----

atmos <- list.files("Data/rasters/atmospheric/", pattern = "*.tif$")
atmos_length <- length(atmos)

s_atmos <- stack(paste0("Data/rasters/atmospheric/", atmos))

for (a in 1:length(atmos)) {
        hw <- extract(s_atmos,
                      mpa,
                      fun = mean,
                      na.rm = TRUE,
                      df = TRUE)
}

heatwaves <- data.frame(hw)
heatwaves <-
        dplyr::rename(heatwaves, mean_heatwaves = heatwaves_cropped)

mpa_heatwaves <-
        merge(mpa_biological, heatwaves[, c("ID", "mean_heatwaves")], by = "ID")


#TOPOGRAPHIC-----

topo <- list.files("Data/rasters/geographic/", pattern = "*.tif$")
topo_length <- length(topo)

s_topo <- stack(paste0("Data/rasters/geographic/", topo))

for (b in 1:length(topo)) {
        tp <- extract(s_topo,
                      mpa,
                      fun = sum,
                      na.rm = TRUE,
                      df = TRUE)
}

topographic <- data.frame(tp)


mpa_topographic <-
        merge(mpa_heatwaves, topographic[, c("ID", "seamounts", "knolls")], by =
                      "ID")





# MEAN SLOPE-----

s <- list.files("Data/rasters/slope/", pattern = "*.tif$")
s_length <- length(s)

s_s <- stack(paste0("Data/rasters/slope/", s))




for (d in 1:length(s)) {
        sl <- extract(s_s,
                      mpa,
                      fun = mean,
                      na.rm = TRUE,
                      df = TRUE)

}




slope <- data.frame(sl)

slope <- dplyr::rename(slope, mean_slope = raster_mexico_full_slope)

mpa_slope <- merge(mpa_topographic, slope[, c("ID", "mean_slope")], by = "ID")



#DUMMY VARIABLES----

mpa_dummy <- read.csv("Data/mpas_dummy.csv")

mpa_variables <-
        merge(mpa_slope, mpa_dummy[, c("ID", "type")], by = "ID")

mpa_variables <- mpa_variables[!is.na(mpa_variables$type), ]

library(tidyverse)
test <- mpa_variables %>% 
        as.data.frame() %>% 
        select(-geometry) %>% 
        dplyr::mutate(ID= row_number())

#EXPORT MPAs SHAPEFILE (Delete previous if an update's needed)
sf::st_write(mpa_variables, "Data/shapefiles/outputs/mpa_variables.shp")
