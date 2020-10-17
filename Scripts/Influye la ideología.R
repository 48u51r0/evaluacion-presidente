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
mod_ideologia <- "eval_pres ~ ideol_GMC + ideol_2 + ideol_3 + man+higher_educ+welloff"
mod_completo <- "eval_pres ~ RV +ideol_GMC + ideol_2 + ideol_3 + man+higher_educ+welloff"

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

`Vacío` <- lmrob(mod_vacio,
               data = datos,
            weights = w)
Voto <- lmrob(mod_voto,
               data = datos,
               weights = w)
`Ideología` <- lmrob(mod_ideologia,
                     data = datos,
                     weights = w)
Completo <- lmrob(mod_completo,
               data = datos,
               weights = w)
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
