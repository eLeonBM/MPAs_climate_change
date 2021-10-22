library(tidyverse)

cvi <- readxl::read_xlsx("Data/MPA_CVI_metadata_v.0.1_28092021.xlsx")

glimpse(cvi)

ggplot(cvi, aes(x = cvi, fill = category)) +
        geom_density()


cvi %>% 
        select(-c(category, habitat, hw_slope)) %>% 
        pivot_longer(coldcorals:knolls) %>% 
        ggplot(aes(x=cvi, y=value, label=NOMBRE)) +
        geom_point() +
        geom_hline(yintercept=3) +
        geom_vline(aes(xintercept = mean(cvi))) +
        facet_wrap(~name, nrow = 2) +
        ggrepel::geom_text_repel() +
        theme_bw() +
        theme(panel.grid = element_blank())



