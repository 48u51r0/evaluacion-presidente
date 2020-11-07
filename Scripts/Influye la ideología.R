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
  mutate(model = 1) %>% 
  # reconvertimos en numerica las vars que rbind ha hecho char
  mutate_at(2:5, as.numeric) %>% 
  reetiquetar()

m_ideologia <- m_ideologia %>% 
  mutate(model = 2) %>% 
  mutate_at(2:5, as.numeric) %>% 
  reetiquetar()

m_adicion <- m_adicion %>% 
  mutate(model = 3) %>% 
  mutate_at(2:5, as.numeric) %>% 
  reetiquetar()

m_interaccion <- m_interaccion %>% 
  mutate(model = 4) %>% 
  mutate_at(2:5, as.numeric) %>% 
  reetiquetar()

m <- rbind(m_voto, m_ideologia, m_adicion, m_interaccion) %>% 
  arrange(desc(model)) %>% 
  mutate(model = factor(model,
                        labels = c("Voto",
                                   "Ubicación Izq-Dcha",
                                   "Efecto Aditivo",
                                   "Efecto Interacción")))

# Ploteamos el dots and whiskers-----------------------------------------------
# https://mran.microsoft.com/snapshot/2015-08-06/web/packages/dotwhisker/
# vignettes/dwplot-vignette.html 

dwplot(m, 
       dodge_size = .7,
       vline = geom_vline(xintercept = 0, 
                          colour = "grey60", 
                          linetype = 2),
       dot_args = list(size = 2,aes(shape = model,color=model)),
       whisker_args = list(aes(color = model)))+
  scale_shape_manual(values = c(18,17,16, 15))+
  scale_color_grey(start = 0, end = .8)+
  theme_bw() + 
  xlab("Estimación") + 
  ylab("") +
  labs(color = "Modelo", shape = "Modelo")+
  # invertimos orden en la leyenda
  guides(color = guide_legend(reverse = TRUE),
         shape = guide_legend(reverse = TRUE))+
  theme(legend.background = element_rect(colour="grey80"),
        legend.title = element_text(face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# Tabla resumen de los modelos de regresión------------------------------------
# https://www.jakeruss.com/cheatsheets/stargazer
# /#robust-standard-errors-replicating-statas-robust-option

# adaptamos la salida de stargazer 
vars_independientes <- c("Intercepto", 
                         "PP", "PSOE", "Cs", "UP", "MP", "VOX", 
                         "Ubicación",
                         "Ubic. * PP", "Ubic. * PSOE", "Ubic. * Cs", 
                         "Ubic. * UP", "Ubic. * MP", "Ubic. * VOX",
                         "Hombre", "Nivel Universitario", "Acomodado")

reordenar <- c(1:8, 12:17, 9:11)

modelos_latex <- c("Voto", "Ubicación\\\\&Izq-Dcha", 
             "Efecto\\\\&Aditivo", "Efecto\\\\&Interacción")

modelos_html <- c("Voto", "Ubicación <br>Izq-Dcha", 
                   "Efecto <br>Aditivo", "Efecto <br>Interacción")

robust_se <- list(voto = t_voto$std.error, 
                 ideologia = t_ideologia$std.error, 
                 adicion = t_adicion$std.error, 
                 interaccion = t_interaccion$std.error)
robust_p <- list(voto = t_voto$p.value, 
                 ideologia = t_ideologia$p.value, 
                 adicion = t_adicion$p.value, 
                 interaccion = t_interaccion$p.value)

stargazer(voto, ideologia, adicion, interaccion, 
          type = "html", 
          intercept.bottom = FALSE, 
          model.names = FALSE,
          object.names = FALSE,
          column.labels = modelos_html,
          model.numbers = FALSE,
          dep.var.labels.include = FALSE,
          covariate.labels = vars_independientes,
          order = reordenar,
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001),
          se = robust_se,
          p = robust_p,
          omit.stat = c("f", "ser"),
          #single.row = TRUE,
          out = "modelos_alternativos.html")

adicion %>% ggpredict(c("ideol_pers[all]", "RV"), 
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
