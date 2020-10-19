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
Sys.setlocale("LC_ALL", "Spanish")

# leemos el excel con los datos del timeline
dat <- readxl::read_xlsx("timeline.xlsx")

# adaptamos los datos del timeline para customizar el gráfico
df <- vistime_data(dat) %>%
  mutate(label = case_when(
    str_detect(label, "Pos")  ~ "1",
    str_detect(label, "Ener") ~ "2",
    str_detect(label, "Feb")  ~ "3",
    str_detect(label, "Marz") ~ "4",
    str_detect(label, "Abr")  ~ "5",
    str_detect(label, "May")  ~ "6",
    TRUE~label),
    fontcol = if_else(group == "Hitos", "black", "white"),
    col = if_else(group == "Hitos", "white", "grey35"))
gg_vistime(df, 
           col.event = "label",
           show_labels = TRUE,
           col.color = "col",
           background_lines = 0,
           col.fontcolor = "fontcol", 
           optimize_y = TRUE,
           linewidth = 10)+ 
  # añadimos lineas de referencia de los hitos
  geom_vline(data = df[df$group == "Hitos", ], 
             mapping = aes(xintercept = start), 
             color= "black",
             alpha = 0.25
  )+ 
  # configuramos la frecuencia y formato de valores en el eje x 
  scale_x_datetime(breaks = date_breaks("1 month"),
                      labels = date_format("%b  %y"))
# exportamos el graf como png y medidas adaptadas

# retornamos a configuración inglesa
Sys.setlocale("LC_ALL", 'english')
