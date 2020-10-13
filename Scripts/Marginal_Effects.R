#-----------------Start up-----------------------------------------------------
#3269:Postelectoral
#3271:Enero
#3273:Febrero
#3277:Marzo
#3279:Abril
#3281:Mayo

#Loading all the necessary packages and datasets
source("Scripts/Cargar_datos.R")

#-----------------Preparación de los datos-------------------------------------
#una función para construir un data set de predict que permita hacer un facet
# The args will be all the data sets available: evolucion(df_1, df_2, df_3)
#generará una salida con dos elementos: df será un data frame con todas las
#predicciones. pooled será un data set de todos los datos de muestra agrupados
#por transiciones:enero-febrero, febrero-marzo, marzo-abril...
predictions <- function(..., significance = 0.99){
  aux <- list(...)
  pooled <- list()
  for (i in seq_len(length(aux)-1)){
    pre <- aux[[i]]
    post <- aux[[i+1]]
    pooled[[i]] <- bind_rows(pre, post) %>% 
      mutate(breakpoint = case_when(Periodo == min(Periodo) ~ "Antes",
                                    Periodo == max(Periodo) ~ "Después") %>% 
               factor()
      )
  }
  df <<- pooled %>%
    map(~lmrob(eval_pres ~ breakpoint*ns(ideol_GMC, 3) + man + higher_educ + welloff, 
               data= ., 
               weights = PESO))  %>% 
    map(~ggpredict(., c("ideol_GMC[all]", "breakpoint"), ci.lvl = significance)) %>% 
    map(~as_tibble(.))
  for (i in seq_along(df)){
    df[[i]] <<- df[[i]] %>% 
      mutate(transition = case_when(i == 1 ~ "Noviembre-Enero",
                                    i == 2 ~ "Enero-Febrero",
                                    i == 3 ~ "Febrero-Marzo ",
                                    i == 4 ~ "Marzo-Abril",
                                    i == 5 ~ "Abril-Mayo",
                                    TRUE ~ "Ampliar el script") %>% 
               factor(levels = c("Noviembre-Enero",
                                 "Enero-Febrero",
                                 "Febrero-Marzo ",
                                 "Marzo-Abril",
                                 "Abril-Mayo",
                                 "Ampliar el script")
                      )
             )
  }
  df <- bind_rows(df)
  rm(pre, post, aux) 
  salida <- list(df = df, pooled = pooled)
  return(salida)
}

#Función para transformar las muestras pooled de forma que agrupamos los datos por
#valores de ideologia y valoración presidente y teniendo en cuenta los pesos
#determinamos el tamaño relativo de los individuos de las muestras que ocupan 
#cada posición
muestra_pooled <- function(data){
  sample <- data %>% 
    select(eval_pres, ideol_GMC, PESO, breakpoint) %>%
    arrange(eval_pres,ideol_GMC,breakpoint) %>% 
    group_by(breakpoint,eval_pres, ideol_GMC) %>% 
    summarise(size=sum(PESO)) %>% 
    ungroup() %>% 
    mutate(prop = size/sum(size)*100) %>% 
    rename( predicted=eval_pres ,  
            x=ideol_GMC,
            group = breakpoint) %>% 
    select(-size)
  return(sample)
}

tmp <- predictions(df_3269, df_3271, df_3273, df_3277, df_3279, df_3281)
datos_muestra <- map(tmp$pooled, muestra_pooled)

#-----------------Visualización------------------------------------------------
tmp$df %>% 
  ggplot(aes(x=x, y=predicted, group = group, color= group))+
  geom_line() +
  geom_ribbon(aes(ymin= conf.low, ymax=conf.high, fill=group), alpha= .3, colour=NA)+
  guides(color = FALSE,
         size = FALSE)+
  theme(legend.title = element_blank())+
  facet_wrap(~transition)+
  theme(legend.position = c(0.85, 0.25)
          ,legend.background = element_rect(fill = "white", colour = NA))

#-----------------Comprobación medias por periodos-----------------------------

# Comprobamos las diferencias de medias entre periodos para 
# descartar que los valores de ideologia centrados en la media 
# utilizados para modelizar ofrezcan valores no comparables entre
# periodos consecutivos.
# Los valoers p se calculan, como los SE, mediante bootstrapping

diff_means <- function(...){
  aux <- list(...)
  salida <- list()
  diff_aux <- function(data_1, data_2){wtd.t.test(data_1$ideol_pers, 
                                                  data_2$ideol_pers, 
                                                  weight = data_1$PESO, 
                                                  weighty = data_2$PESO, 
                                                  bootse = TRUE, 
                                                  bootp = TRUE, 
                                                  bootn = 10000)
  } 
  for (i in seq_len(length(aux)-1)){
    pre <- aux[[i]]
    post <- aux[[i+1]]
    salida[[i]] <- diff_aux(pre, post)
  }
  salida <- salida %>% 
    map(~c(.x$additional[2], .x$additional[3])) #retorna las medias por periodo
  # map_dbl(~.x$coefficients[3]) --> retorna los p values
  return(salida)
}
diff_means(df_3269,df_3271,df_3273,df_3277,df_3279,df_3281)

# el resultado de los t-test welch para los datos ponderados(H_0: medias iguales)
# es pvalues=(0.0886 0.2112 0.8652 0.0936 0.0142). Solo la ultima transición
# presente medias diferentes significativas al 0.05 pero no al 0.01. Además
# la diferencia de medias es 0.144 por lo que no altera los resultados y su 
# interpretación

# las medias ideologia por periodos son (4.58, 4.48, 4.57, 4.58, 4.68, 4.53)

#-----------------Restos para borrar-------------------------------------------
ggplot()+
  geom_point(data=aux, 
             mapping=aes(x=x, y=predicted, size=prop, color = group), 
             alpha = 0.2)+
  geom_jitter(width = 0.5, height = -0.5)+
  geom_line(pred, 
            mapping=aes(x=x, y=predicted,group = group, color= group)) +
  geom_ribbon(pred, 
              mapping=aes(x=x, y=predicted,group = group,
                          ymin= conf.low, ymax=conf.high, fill=group),
              alpha= .3, colour=NA) +
  guides(color = FALSE,
         size = FALSE)+
  theme(legend.title = element_blank())