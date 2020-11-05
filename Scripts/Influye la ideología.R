#-----------------Start up------------------------------------------
#3269:Postelectoral
#3271:Enero
#3273:Febrero
#3277:Marzo
#3279:Abril
#3281:Mayo

#Loading all the necessary packages and datasets
source("Scripts/Cargar_datos.R")

#-----------------Planteamiento de modelos-------------------------------------

mod_vacio <- "eval_pres ~ man + higher_educ+ welloff"
mod_voto <- "eval_pres ~ RV + man + higher_educ + welloff"
#mod_partidismo <- "eval_pres ~ partidismo_1 + man + higher_educ + welloff"
mod_ideologia <- "eval_pres ~ ideol_GMC + man+higher_educ+welloff"
mod_completo <- "eval_pres ~ RV*(ideol_GMC ) + man+higher_educ+welloff"

datos <- bind_rows(df_3269, df_3271, df_3273, df_3277, df_3279, df_3281)
# ponderamos los pesos de cada dataset por la contribucion de cada
# conjunto de datos al total
aux <- numeric()
for (i in 1:6) {
  aux[i] <- sum(datos$PESO[datos$Periodo== i])/(sum(datos$PESO))
}
datos <- datos %>% 
  mutate(ponderacion = case_when(Periodo == 1 ~ aux[1],
                                 Periodo == 2 ~ aux[2],
                                 Periodo == 3 ~ aux[3],
                                 Periodo == 4 ~ aux[4],
                                 Periodo == 5 ~ aux[5],
                                 Periodo == 6 ~ aux[6]),
         w = PESO * ponderacion)

`Vacío` <- lm(mod_vacio,
                 data = df_3277,
                 weights = PESO)
Voto <- lm(mod_voto,
              data = df_3277,
              weights = PESO)
`Ideología` <- lm(mod_ideologia,
                     data = df_3277,
                     weights = PESO)
Completo <- lm(mod_completo,
                  data = df_3277,
                  weights = PESO)
Completo2 <- lm(eval_pres ~ RV*poly(ideol_GMC, 3) + man+higher_educ+welloff,
                  data = df_3277,
                weights = PESO)
#-----------------Summary------------------------------------------------------

# cambiamos los nombres de los coeficientes de las regresiones para que 
# salgan correctamente en el output
names(`Vacío`$coefficients)[1]<- "Intercepto"
names(`Vacío`$coefficients)[2]<- "Hombre"
names(`Vacío`$coefficients)[3]<- "No universitario"
names(`Vacío`$coefficients)[4]<- "No acomodado"

names(Simple$coefficients)[1]<- "Intercepto"
names(Simple$coefficients)[2]<- "Ideología"
names(Simple$coefficients)[3]<- "Hombre"
names(Simple$coefficients)[4]<- "No universitario"
names(Simple$coefficients)[5]<- "No acomodado"

names(Completo$coefficients)[1]<- "Intercepto"
names(Completo$coefficients)[2]<- "Ideología"
names(Completo$coefficients)[3]<- "Ideología (cuadrático)"
names(Completo$coefficients)[4]<- "Ideología (cúbico)"
names(Completo$coefficients)[5]<- "Hombre"
names(Completo$coefficients)[6]<- "No universitario"
names(Completo$coefficients)[7]<- "No acomodado"
stargazer(`Vacío`, `Ideología`, Voto, Completo, type = "text", 
          intercept.bottom = FALSE, model.names = TRUE,
          object.names = TRUE, model.numbers = FALSE,
          out = "tresmodelos.html")


# utilizaremos este y el dots and whiskers 
# https://mran.microsoft.com/snapshot/2015-08-06/web/packages/dotwhisker/vignettes/dwplot-vignette.html 
vacio <- coeftest(`Vacío`, vcov. = vcovHC(`Vacío`, type = "HC0"))
ideologia <- coeftest(Ideología, vcov. = vcovHC(Ideología, type = "HC0"))
voto <- coeftest(Voto, vcov. = vcovHC(Voto, type = "HC0"))
completo <- coeftest(Completo, vcov. = vcovHC(Completo, type = "HC0"))
completo2 <- coeftest(Completo2, vcov. = vcovHC(Completo2, type = "HC0"))

#adaptamos la salida de stargazer 
#https://www.jakeruss.com/cheatsheets/stargazer/#robust-standard-errors-replicating-statas-robust-option
stargazer(vacio, ideologia, voto, completo, type = "text", 
          intercept.bottom = FALSE, model.names = TRUE,
          object.names = TRUE, model.numbers = FALSE,
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001))

Completo %>% ggpredict(c("ideol_GMC[all]", "RV"), 
                       vcov.fun = "vcovHC", 
                       vcov.type = "HC0") %>% 
  filter(!group %in% c("Otros", "MP", "Cs", "Vox")) %>%
  mutate(group = fct_drop(group)) %>% 
  ggplot(aes(x=x, 
             y=predicted, 
             group = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high), 
              alpha = .3, 
              colour = NA) +
  guides(color = FALSE,
         size = FALSE) +
  theme(legend.title = element_blank()) +
  facet_wrap(~group) +
  labs(x = "Autoubicación ideológica centrada en la media",
       y = "Valoración Presidente (1-10)") +
  theme(legend.background = element_rect(fill = "white", 
                                         colour = NA),
        panel.background = element_blank(),
        panel.border =element_rect(colour = "black", 
                                   fill = NA, 
                                   size = 1),
        #panel.grid.major = element_line(colour = "lightgrey", 
        #                                size = .15),
        panel.grid.major = element_blank(),
        strip.background = element_rect(color = "black", 
                                        fill = "white", 
                                        size = 1, 
                                        linetype = "solid"),
        strip.text.x = element_text(size = 12, 
                                    face = "bold"),
        axis.title=element_text(size=14)
  )+
  coord_fixed(ratio = 1)+
  ylim(0,10)
