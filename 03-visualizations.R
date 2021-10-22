library(sp)
library(tidyverse)
library(hrbrthemes)
library(sf)
library(ggplot2)
library(ggrepel)
library(ggthemes)

#Load Data----
mpas_cvi <- sf::st_read("Data/MPA_CVI/MPA_CVI_v.0.0/MPA_CVI_v.0.1_28092021.shp") #Use the latest version


mpas_cvi <- mpas_cvi %>%
        dplyr::mutate(ID = row_number())

#Function for extracting centroids of polygons: Coordinates for Label ID

st_centroid_within_poly <- function(poly) {
        # check if centroid is in polygon
        ctrd <- st_centroid(poly, of_largest_polygon = TRUE)
        in_poly <- diag(st_within(ctrd, poly, sparse = F))
        
        # replace geometries that are not within polygon with st_point_on_surface()
        st_geometry(ctrd[!in_poly, ]) <-
                st_geometry(st_point_on_surface(poly[!in_poly, ]))
        
        ctrd
}

## BAR PLOT

# 
# library(remotes)
# remotes::install_version("Rttf2pt1", version = "1.3.8")
# extrafont::font_import()
loadfonts(device = "win")
bar <- mpas_cvi %>%
        as.data.frame() %>%
        select(NOMBRE, category) %>%
        group_by(category) %>%
        count() %>%
        mutate(category = factor(category, levels = c("Low", "Medium", "High", "Very High"))) %>%
        ggplot(aes(x = category, y = n,  fill = category)) +
        geom_col(width = 0.75) +
        lims(y = c(0, 11)) +
        # geom_bar(width =1 )+
        scale_fill_manual(values = c("#005a32", "#ede61a", "#ed671a", "firebrick"),
                          name = "Category") +
        theme_ipsum() +
        guides(fill = guide_legend(title.position = "bottom", title.hjust = 0.5)) +
        labs(x = "", y = "Number of MPAs") +
        theme(
                strip.text.x = element_text(angle = 0, hjust = .5),
                axis.text.x = element_blank(),
                axis.title.y = element_text(
                        face = "bold",
                        hjust = 0.5,
                        family = "Times New Roman",
                        size = 18
                ),
                legend.position = "bottom",
                legend.text = element_text(
                        size = 20,
                        colour = "black",
                        family = "Times New Roman"
                ),
                legend.title = element_text(
                        face = "bold",
                        size = 25,
                        family = "Times New Roman"
                )
                # plot.background = element_rect(fill="#c4c4c4", colour="#c4c4c4")
        )
bar
ggsave(
        "Graphics/MPA_CVI_bars.png",
        dpi = 500,
        height = 8,
        width = 12
)



#Selecting the fields of interest
cvi <- mpas_cvi %>%
        select(ID, NOMBRE, category, geometry) %>%
        mutate(
                lon = map_dbl(geometry, ~ st_point_on_surface(.x)[[1]]),
                lat = map_dbl(geometry, ~ st_point_on_surface(.x)[[2]])
        )

#Subsetting the shapefile into separate categories
low <- subset(cvi, category == "Low")
medium <- subset(cvi, category == "Medium")
high <- subset(cvi, category == "High")
very_high <- subset(cvi, category == "Very High")

#Plot spatial polygons-----
library(extrafont)
loadfonts(device = "win")



spdf_mx <- st_transform(
        st_as_sf(rnaturalearth::ne_countries(country = 'mexico')), 
        crs = 4326)#Selecting Mexico

## LOW
p1 <- ggplot() + 
        geom_sf(data = spdf_mx,
                         fill = "gray90",
                         col = NA) +
        ggplot2::geom_sf(data = low, fill = "#005a32") +
        geom_label_repel(
                data = low,
                aes(x = lon, y = lat, label = ID),
                #function to plot label ID to polygons
                box.padding = unit(0.5, "lines"),
                # point.padding = unit(0.5, "lines"),
                segment.color = 'black',
                label.padding = 0.1,
                segment.size = 1,
                fill = alpha(c("white"), 0),
                label.size = NA,
                # fontface= "bold",
                color = "black",
                size = 7
        ) +
        coord_sf(xlim = c(-118, -85),
                 ylim = c(13, 32),
                 expand = TRUE) +
        theme_bw(base_family = "Lato") +
        labs(title = "MPA Vulnerability Index", subtitle = "Low") +
        theme_map() +
        theme(
                plot.title = element_text(
                        size = 20,
                        face = "bold",
                        family = "Times New Roman",
                        hjust = 0.5
                ),
                plot.subtitle = element_text(
                        size = 18,
                        family = "Times New Roman",
                        hjust = 0.5
                ),
                legend.position = ""
        ) +
        ggspatial::annotation_scale()
p1

ggsave(
        "Graphics/MPA_CVI_Low.png",
        dpi = 700,
        height = 8,
        width = 12
)

## MEDIUM
p2 <- ggplot() + 
        geom_sf(data = spdf_mx,
                         fill = "gray90",
                         col = NA) +
        ggplot2::geom_sf(data = medium, fill = "#ede61a") +
        geom_label_repel(
                data = medium,
                aes(x = lon, y = lat, label = ID),
                box.padding = unit(0.5, "lines"),
                # point.padding = unit(0.5, "lines"),
                segment.color = 'black',
                label.padding = 0.1,
                segment.size = 1,
                fill = alpha(c("white"), 0.2),
                label.size = NA,
                # fontface= "bold",
                color = "black",
                size = 7
        ) +
        coord_sf(xlim = c(-118, -85),
                 ylim = c(13, 32),
                 expand = TRUE) +
        theme_bw(base_family = "Lato") +
        labs(title = "MPA Vulnerability Index", subtitle = "Medium") +
        theme_map() +
        theme(
                plot.title = element_text(
                        size = 20,
                        face = "bold",
                        family = "Times New Roman",
                        hjust = 0.5
                ),
                plot.subtitle = element_text(
                        size = 18,
                        family = "Times New Roman",
                        hjust = 0.5
                )
        ) +
        ggspatial::annotation_scale()
# ggsn:: scalebar(medium, dist = 100, dist_unit == "m", st.size=3, height=0.01, transform = TRUE, model = 'WGS84' )
p2

ggsave(
        "Graphics/MPA_CVI_Medium.png",
        dpi = 700,
        height = 8,
        width = 12
)


## HIGH
p3 <- ggplot() + 
        geom_sf(data = spdf_mx,
                         fill = "gray90",
                         col = NA) +
        ggplot2::geom_sf(data = high, fill = "#ed671a") +
        geom_label_repel(
                data = high,
                aes(x = lon, y = lat, label = ID),
                box.padding = unit(0.5, "lines"),
                # point.padding = unit(0.5, "lines"),
                segment.color = 'black',
                label.padding = 0.1,
                segment.size = 1,
                fill = alpha(c("white"), 0.2),
                label.size = NA,
                # fontface= "bold",
                color = "black",
                size = 7
        ) +
        coord_sf(xlim = c(-118, -85),
                 ylim = c(13, 32),
                 expand = TRUE) +
        theme_bw(base_family = "Lato") +
        labs(title = "MPA Vulnerability Index", subtitle = "High") +
        theme_map() +
        theme(
                plot.title = element_text(
                        size = 20,
                        face = "bold",
                        family = "Times New Roman",
                        hjust = 0.5
                ),
                plot.subtitle = element_text(
                        size = 18,
                        family = "Times New Roman",
                        hjust = 0.5
                )
        ) +
        ggspatial::annotation_scale()
p3
ggsave(
        "Graphics/MPA_CVI_High.png",
        dpi = 700,
        height = 8,
        width = 12
)


## VERY HIGH
p4 <- ggplot() + 
        geom_sf(data = spdf_mx,
                         fill = "gray90",
                         col = NA) +
        ggplot2::geom_sf(data = very_high, 
                         fill = "firebrick") +
        geom_label_repel(
                data = very_high,
                aes(x = lon, y = lat, label = ID),
                box.padding = unit(0.5, "lines"),
                # point.padding = unit(0.5, "lines"),
                segment.color = 'black',
                label.padding = 0.1,
                segment.size = 1,
                fill = alpha(c("white"), 0.2),
                label.size = NA,
                # fontface= "bold",
                color = "black",
                size = 7
        ) +
        coord_sf(xlim = c(-118, -85),
                 ylim = c(13, 32),
                 expand = TRUE) +
        theme_bw(base_family = "Lato") +
        labs(title = "MPA Vulnerability Index", subtitle = "Very High") +
        theme_map() +
        theme(
                plot.title = element_text(
                        size = 20,
                        face = "bold",
                        family = "Times New Roman",
                        hjust = 0.5
                ),
                plot.subtitle = element_text(
                        size = 18,
                        family = "Times New Roman",
                        hjust = 0.5
                )
        ) +
        ggspatial::annotation_scale()
p4
ggsave(
        "Graphics/MPA_CVI_Very_High.png",
        dpi = 700,
        height = 8,
        width = 12
)

(p1 + p2) / (p3 + p4)

