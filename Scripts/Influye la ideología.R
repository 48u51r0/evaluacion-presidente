# Start up------------------------------------------
#3269:Postelectoral
#3271:Enero
#3273:Febrero
#3277:Marzo
#3279:Abril
#3281:Mayo

#Loading all the necessary packages and datasets
source("Scripts/Cargar_datos.R")

# Planteamiento de modelos-------------------------------------

# Fórmulas para los modelos
f_vacio <- "eval_pres ~ man + higher_educ+ welloff"
f_voto <- "eval_pres ~ RV + man + higher_educ + welloff"
f_ideologia <- "eval_pres ~ ideol_pers + man+higher_educ+welloff"
f_adicion <-  "eval_pres ~ RV + ideol_pers + man+higher_educ+welloff"
f_interaccion <- "eval_pres ~ RV*ideol_pers + man+higher_educ+welloff"

# wrapper de lm para no ser verbose
regresion <- function(formula){
  expresion <- as.formula(formula)
  lm(expresion,
     data =df_3277, 
     weights = PESO)
}

# funciones para aplicar a todos los modelos
prepara_modelo <-  . %>%   
  varcov_sandwich() %>% 
  tidy() 
filtrar <- . %>% 
  filter(!term %in% c("(Intercept)", 
                      "manHombre",
                      "higher_educUniversitario", 
                      "welloffMedio-alto")
  )

# Inicializamos los modelos (utiles para stargazer)----------------------------
voto <- regresion(f_voto)
ideologia <- regresion(f_ideologia)
adicion <- regresion(f_adicion)
interaccion <- regresion(f_interaccion)

# Preparamos los modelos para gráfico dots & whiskers--------------------------
# Primero tidy y luego filtrado para poder usarlo en stargazer
t_voto <- voto %>%  prepara_modelo() 
t_ideologia <- ideologia %>% prepara_modelo() 
t_adicion <- adicion %>% prepara_modelo() 
t_interaccion <- interaccion %>% prepara_modelo() 

m_voto <- t_voto %>% filtrar() 
m_ideologia <- t_ideologia %>% filtrar()
m_adicion <- t_adicion %>% filtrar() 
m_interaccion <- t_interaccion %>% filtrar() 

# Todo modelo con las mismas variables y en el mismo orden
# ncol(m_ideologia)-1 = 4
m_voto <- rbind(m_voto, 
                c("ideol_pers", rep(NA_real_, 4)),
                c("RVPP:ideol_pers", rep(NA_real_, 4)),
                c("RVPSOE:ideol_pers", rep(NA_real_, 4)),
                c("RVCs:ideol_pers", rep(NA_real_, 4)),
                c("RVUP:ideol_pers", rep(NA_real_, 4)),
                c("RVMP:ideol_pers", rep(NA_real_, 4)),
                c("RVVOX:ideol_pers", rep(NA_real_, 4))
)
m_ideologia <- rbind(c("RVPP", rep(NA, 4)),
                     c("RVPSOE", rep(NA, 4)),
                     c("RVCs", rep(NA, 4)),
                     c("RVUP", rep(NA, 4)),
                     c("RVMP", rep(NA, 4)),
                     c("RVVOX", rep(NA, 4)),
                     m_ideologia,
                     c("RVPP:ideol_pers", rep(NA, 4)),
                     c("RVPSOE:ideol_pers", rep(NA, 4)),
                     c("RVCs:ideol_pers", rep(NA, 4)),
                     c("RVUP:ideol_pers", rep(NA, 4)),
                     c("RVMP:ideol_pers", rep(NA, 4)),
                     c("RVVOX:ideol_pers", rep(NA, 4))
)
m_adicion <- rbind(m_adicion,
                   c("RVPP:ideol_pers", rep(NA, 4)),
                   c("RVPSOE:ideol_pers", rep(NA, 4)),
                   c("RVCs:ideol_pers", rep(NA, 4)),
                   c("RVUP:ideol_pers", rep(NA, 4)),
                   c("RVMP:ideol_pers", rep(NA, 4)),
                   c("RVVOX:ideol_pers", rep(NA, 4))
)


# Funcion para Reetiquetar los coeficientes
reetiquetar <- . %>% 
  relabel_predictors(c(RVPP = "PP",
                       RVPSOE = "PSOE",
                       RVCs = "Cs",
                       RVUP = "UP",
                       RVMP = "MP",
                       RVVOX = "VOX",
                       ideol_pers = "Ubicación",
                       `RVPP:ideol_pers` = "Ubic. * PP",
                       `RVPSOE:ideol_pers` = "Ubic. * PSOE",
                       `RVCs:ideol_pers` = "Ubic. * Cs",
                       `RVUP:ideol_pers` = "Ubic. * UP",
                       `RVMP:ideol_pers` = "Ubic. * MP",
                       `RVVOX:ideol_pers` = "Ubic. * VOX")
  )

m_voto <- m_voto %>% 
  # Añadimos un identificador de modelo
  mutate(model = "Recuerdo voto") %>% 
  # reconvertimos en numerica las vars que rbind ha hecho char
  mutate_at(2:5, as.numeric) %>% 
  reetiquetar()

m_ideologia <- m_ideologia %>% 
  mutate(model = "Ubicación D-I") %>% 
  mutate_at(2:5, as.numeric) %>% 
  reetiquetar()

m_adicion <- m_adicion %>% 
  mutate(model = "Aditivo") %>% 
  mutate_at(2:5, as.numeric) %>% 
  reetiquetar()

m_interaccion <- m_interaccion %>% 
  mutate(model = "Interacción") %>% 
  mutate_at(2:5, as.numeric) %>% 
  reetiquetar()

m <- rbind(m_voto, m_ideologia, m_adicion, m_interaccion)

# Ploteamos el dots and whiskers-----------------------------------------------
# https://mran.microsoft.com/snapshot/2015-08-06/web/packages/dotwhisker/
# vignettes/dwplot-vignette.html 
dwplot(m, dodge_size = .5,
       vline = geom_vline(xintercept = 0, 
                          colour = "grey60", 
                          linetype = 2),
       dot_args = list(aes(shape = model,color=model)),
       whisker_args = list(aes(color = model)))+
  scale_color_grey(end = .6)+
  theme_bw() + 
  xlab("Estimación") + 
  ylab("") +
  labs(color = "Modelo", shape = "Modelo")+
  theme(legend.background = element_rect(colour="grey80"),
        legend.title = element_text(face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())  

# Tabla resumen de los modelos de regresión------------------------------------
# https://www.jakeruss.com/cheatsheets/stargazer
# /#robust-standard-errors-replicating-statas-robust-option




vacio <- coeftest(`Vacío`, vcov. = vcovHC(`Vacío`, type = "HC0"))
ideologia <- coeftest(Ideología, vcov. = vcovHC(Ideología, type = "HC0"))
voto <- coeftest(Voto, vcov. = vcovHC(Voto, type = "HC0"))
completo <- coeftest(Completo, vcov. = vcovHC(Completo, type = "HC0"))
completo2 <- coeftest(Completo2, vcov. = vcovHC(Completo2, type = "HC0"))

#adaptamos la salida de stargazer 

stargazer(voto, ideologia, adicion, interaccion, type = "text", 
          intercept.bottom = FALSE, model.names = TRUE,
          object.names = FALSE, model.numbers = FALSE,
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001))

Completo %>% ggpredict(c("ideol_pers[all]", "RV"), 
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
