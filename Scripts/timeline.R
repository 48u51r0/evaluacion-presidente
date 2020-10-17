# Start-up
rm(list = ls())
source("Scripts/libs.R")

# Specify and install all the necessary packages
packages <- (c("weights","haven", "sandwich", "multcomp", "emmeans", 
               "tidyverse", "conflicted", "ggeffects", "splines",
               "robustbase", "stargazer", "vistime",
               "ggrepel"))
install_pack(packages)

# para generar los timeline con fechas en español
Sys.setlocale("LC_TIME", "Spanish")

# leemos el excel con los datos del timeline
data <- readxl::read_xlsx("timeline.xlsx")

gg_vistime(data, 
                show_labels = FALSE,
                col.color = "color",
                background_lines = 0,
                optimize_y = TRUE)+ 
  geom_text_repel(aes(label = event), 
                  color = "black",
                  force = 2,
                  box.padding = 1,
                  segment.color = NA) +
  geom_vline(data = data[data$group == "Hitos", ], 
             mapping = aes(xintercept = start), 
             color= "grey69",
             alpha = 0.5
               )


# Una vez finalizado retornamos la configuración inglesa
Sys.setlocale("LC_TIME", "English")