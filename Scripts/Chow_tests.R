#-----------------Start up------------------------------------------

#Loading all the necessary packages and datasets
source("Scripts/Cargar_datos.R")

#-----------------Several models of relations between DV and IV--------------
mod0 <- "eval_pres~man+higher_educ+welloff"
mod1 <- "eval_pres~ideol_GMC + man+higher_educ+welloff"
mod2 <- "eval_pres~ideol_GMC + ideol_2  + man+higher_educ+welloff"
mod3 <- "eval_pres~ideol_GMC + ideol_2 + ideol_3 + man+higher_educ+welloff"
mod4 <- "eval_pres~RV+man+higher_educ+welloff"

#function to apply the likelihood ratio test
lrt <- function(data = data){
  models <- list("mod0" = mod0 ,"mod1" = mod1,"mod2"= mod2, "mod3"=mod3, "mod4"=mod4) %>% 
    map(~lm(.,
            data = data,
            weights = PESO)
    )
  tmp1 <- lmtest::lrtest(models$mod0, models$mod1)
  tmp2 <- lmtest::lrtest(models$mod1, models$mod2)
  tmp3 <- lmtest::lrtest(models$mod2, models$mod3)
  tmp4 <- lmtest::lrtest(models$mod1, models$mod3)
  tmp5 <- lmtest::lrtest(models$mod3, models$mod4)
  tmp6 <- lmtest::lrtest(models$mod0, models$mod3)
  tmp7 <- lmtest::lrtest(models$mod0, models$mod4)
  # substitute returns an object name as a string
  message(substitute(data), ":p-val para mod0 versus mod1: ", tmp1$`Pr(>Chisq)`[2])
  message(substitute(data), ":p-val para mod1 versus mod2: ", tmp2$`Pr(>Chisq)`[2])
  message(substitute(data), ":p-val para mod2 versus mod3: ", tmp3$`Pr(>Chisq)`[2])
  message(substitute(data), ":p-val para mod1 versus mod3: ", tmp4$`Pr(>Chisq)`[2])
  message(substitute(data), ":p-val para mod3 versus mod4: ", tmp5$`Pr(>Chisq)`[2])
  message(substitute(data), ":p-val para mod0 versus mod3: ", tmp6$`Pr(>Chisq)`[2])
  message(substitute(data), ":p-val para mod0 versus mod4: ", tmp7$`Pr(>Chisq)`[2])
}
#Se ha probado con todos los data_Sets por separado, 
#para todos los casos el modelo ampliado es mejor que el restringido.
#las excepciones han sido a partir del confinamiento ya que el modelo cuadrático
#era peor que el model lineal. Sin embargo, como el cúbico era mejor que el lineal
#usaré término cúbico (ideo_3)

# Función para extraer la R2 ajustada  de cada modelo y conjunto de datos
adj_r2 <- function(data = data){
  models <- list("mod0" = mod0 ,"mod1" = mod1,"mod2"= mod2, "mod3"=mod3, "mod4"=mod4) %>% 
    map(~lm(.,
            data = data,
            weights = PESO)
    )
  tmp0 <- summary(models$mod0)$adj.r.squared
  tmp1 <- summary(models$mod1)$adj.r.squared
  tmp2 <- summary(models$mod2)$adj.r.squared
  tmp3 <- summary(models$mod3)$adj.r.squared
  tmp4 <- summary(models$mod4)$adj.r.squared
  # substitute returns an object name as a string
  message(substitute(data), ":Adj_R2 mod0: ", tmp0)
  message(substitute(data), ":Adj_R2 mod1: ", tmp1)
  message(substitute(data), ":Adj_R2 mod2: ", tmp2)
  message(substitute(data), ":Adj_R2 mod3: ", tmp3)
  message(substitute(data), ":Adj_R2 mod4: ", tmp4)
}
#probamos qué modelo es más óptimo
adj_r2(df_3269)
adj_r2(df_3271)
adj_r2(df_3273)
adj_r2(df_3277)
adj_r2(df_3279)
adj_r2(df_3281)
# sistematicamente todos los modelos explican más varianza que el modelo vacío de controles
# el modelo completo de ideologia explica siempre menos que el modelo de Recuerdo de voto
# utilizamos la R2 ajustada para evitar la inflación de var explicada por añadir predictores

rm(list = c("mod0", "mod1", "mod2", "mod3", "mod4"))

#-----------------Function to run Chow tests (Lee, 2008)--------------------------------

chow <- function(pre, post, formula, significance = .05){
  pooled <- bind_rows(pre, post) %>% 
    mutate(breakpoint = case_when(Periodo == min(Periodo) ~ 0,
                                  Periodo == max(Periodo) ~ 1) %>% 
             factor()
    )
  models <- list("pre" = pre,"post"= post, "pooled"=pooled) %>% 
    map(~lm(formula,
            data= .,
            weights = PESO)
    )
  RSS <- models %>% map(deviance)
  N <- length(pooled$PESO[pooled$PESO != 0])
  df1 <- length(models$pre$coefficients)
  df2 <- N - 2 * df1
  F_chow <- ((RSS$pooled -RSS$pre - RSS$post) / df1) / ((RSS$pre + RSS$post) / df2)
  F_critical <- qf(1-significance, df1, df2)
  cat(paste0("Model: ", formula, "\n"))
  cat("sigma-pre and sigma-post stability. If Stable, chow test is suitable\n")
  var.test(models$pre, models$post)
  aux <- data.frame(var = c(models$pre$residuals, models$post$residuals),
                    group = c(rep(0, nobs(models$pre)), rep(1, nobs(models$post))) %>% 
                      factor()
  )
  levene <- car::leveneTest(aux$var, aux$group, center = median)
  fk <- fligner.test(aux$var, aux$group)
  if(levene$`Pr(>F)`[1] < significance){
    cat("Levene test: Heteroskedastico al ", significance, ". Resultado Chow: dudoso\n")
  } else {
    cat("Levene test: Homoskedástico al ", significance, ". Resultado Chow: probable\n")
  }
  if(fk$p.value < significance){
    cat("Fligner-Killeen: Heteroskedastico al ", significance, ". Resultado Chow: dudoso\n")
  } else {
    cat("Fligner-Killeen: Homoskedástico al ", significance, ". Resultado Chow: probable\n")
  }
  if(F_chow > F_critical){
    cat("Se ha producido un cambio estructural\n")
  } else{
    cat("No se ha producido un cambio estructural\n")
  }
  return(c(F_chow, F_critical))
}

#-----------------Running Chow tests----------------------------------------------

#3269 - 3271
df_pre <- df_3269 
df_post <- df_3271
f1 <- chow(pre = df_pre, post = df_post, formula = mod3, significance = 0.01) #Stable. Probable

#3271 - 3273
df_pre <- df_3271 
df_post <- df_3273 
f2 <- chow(pre = df_pre, post = df_post, formula = mod3, significance = 0.01) #Stable. Probable

#3273 - 3277
df_pre <- df_3273 
df_post <- df_3277 
f3  <- chow(pre = df_pre, post = df_post, formula = mod3, significance = 0.01) #Stable. Probable

#3277 - 3279
df_pre <- df_3277
df_post <- df_3279 
f4 <- chow(pre = df_pre, post = df_post, formula = mod3, significance = 0.01) #Stable. Probable

#3279 - 3281
df_pre <- df_3279
df_post <- df_3281 
f5 <- chow(pre = df_pre, post = df_post, formula = mod3, significance = 0.01) #Stable. Probable

#3269+3271 - 3273
df_pre <- bind_rows(df_3269, df_3271) 
df_post <- df_3273
chow(pre = df_pre, post = df_post, formula = mod3, significance = 0.01)#CAmbio. Probable

#3269+3271+3273 - 3277
df_pre <- bind_rows(df_3269, df_3271, df_3273) 
df_post <- df_3277
chow(pre = df_pre, post = df_post, formula = mod3, significance = 0.01)#CAmbio. Dudoso

#3269+3271+3273+3277 - 3279
df_pre <- bind_rows(df_3269, df_3271, df_3273, df_3277) 
df_post <- df_3279
chow(pre = df_pre, post = df_post, formula = mod3, significance = 0.01)#CAmbio. Dudoso



#debido a la heteroskedasticidad los resultados de este test no son fiables

#-----------------Dummy variable alternative to Chow--------------------------------------------

#A different approach to test coefficient discontinuities is to use dummy with
#pooled data, instead of 2 samples plus the pooled data of the Chow original approach.
#The unrestricted model will be:
mod3_unrestr <- "eval_pres ~ breakpoint*ideol_GMC + breakpoint*ideol_2 + 
breakpoint*ideol_3 + man + higher_educ + welloff"

#the idea is to infer a "joint hypothesis" test of multiple restrictions.
#This joint hypothesis will be: all coefficients for terms containing the dummy
#are equal to 0. Thus, the restricted model will be:
mod3_restr <- "eval_pres ~ ideol_GMC + ideol_2 + ideol_3 + man+higher_educ+welloff"

#Dummy Chow function
chow_dummy <- function(pre, 
                       post, 
                       restricted, 
                       unrestricted, 
                       significance = .05){
  #definimos el peso de los df en el pooled
  w_pooled <- sum(pre$PESO, post$PESO)
  w_pre <- sum(pre$PESO) / w_pooled
  w_post <- sum(post$PESO) / w_pooled
  # definimos el pooled data set con una dummy de punto discontinuidad
  # ponderamos el peso de acuerdo al peso que cada df aporta a pooled
  pooled <- bind_rows(pre, post) %>% 
    mutate(breakpoint = case_when(Periodo == min(Periodo) ~ 0,
                                  Periodo == max(Periodo) ~ 1) %>% 
             factor(),
           ponderacion = case_when(breakpoint == 0 ~ w_pre,
                                   breakpoint == 1 ~ w_post),
           w = ponderacion * PESO
    )
  #definimos los modelos lineales y robusto
  lin_restr_pre <- lm(restricted, data = pre, weights = PESO)
  lin_restr_post <- lm(restricted, data = post, weights = PESO)
  lin_unrestr <- lm(unrestricted, data = pooled, weights = w)
  robust_unrestr <- robustbase::lmrob(unrestricted, data = pooled, weights = w)
  #definimos las matrices de hipótesis (multcomp)
  k_restr <- read_rds("k_chow.rds") #Solo los coeficientes con la dummy son = 0
  k_unrestr <- cbind(diag(length(coef(robust_unrestr)))) #todos los coeficientes = 0
  rownames(k_unrestr) <- names(coef(robust_unrestr))
  #Definimos los objeto multcomp del test de Chow
  #los modelos chow se utilizan para la inferencia multiple de las variables dummy
  #usamos dos alternativas robustas ante la heteroscedasticidad:
  #sandwich y estimador MM (regresion robusta)
  aux_linear_chow <- glht(lin_unrestr, linfct = k_restr, vcov =sandwich)
  aux_robust_chow <- glht(robust_unrestr, linfct = k_restr)
  #Calculamos los F-statistic del test chow con dummy
  linear_chow <- summary(aux_linear_chow, test = Ftest())
  robust_chow <- summary(aux_robust_chow, test =Ftest())
  #Calculamos el valor F-crítico. No importa si usamos linear_chow o robust_chow
  fcritico <- qf(1-significance, 
                 linear_chow$test$df[1], 
                 linear_chow$test$df[2])
  #otra alternativa es simplemente realizar la inferencia múltiple mediante
  #el max-t test descrito por Hothorn, Bretz y Westfall(2008)
  linear_chow_maxt <- summary(aux_linear_chow)
  robust_chow_maxt <- summary(aux_robust_chow)
  #Contraste de hipótesis para los dos métodos alternativos:robust and sandwich
  chow_linear <- if_else(linear_chow$test$fstat > fcritico, 
                         paste0("Hay discontinuidad en la regresión al ", 
                                significance), 
                         paste0("No hay discontinuidad en la regresión al", 
                                significance)
  )
  chow_robust <- if_else(robust_chow$test$fstat > fcritico, 
                         paste0("Hay discontinuidad en la regresión al ", 
                                significance), 
                         paste0("No hay discontinuidad en la regresión al ", 
                                significance)
  )
  
  #los modelos completo se utlizarán para realizar la regresión, obtener
  #sus coeficientes, SE y p-value
  #también se utiliza el ajuste multcomp de inferencia múltiple
  #y las dos aproximaciones a la heteroscedasticity sandwich y robust MM
  aux_linear_completo <- glht(lin_unrestr, linfct = k_unrestr, vcov= sandwich)
  aux_robust_completo <- glht(robust_unrestr, linfct = k_unrestr)
  linear_completo <- summary(aux_linear_completo)
  robust_completo <- summary(aux_robust_completo)
  
  aux <- data.frame(var = c(lin_restr_pre$residuals, 
                            lin_restr_post$residuals),
                    group = c(rep(0, nobs(lin_restr_pre)), 
                              rep(1, nobs(lin_restr_post))) %>% 
                      factor()
  )
  levene <- car::leveneTest(aux$var, aux$group, center = median)
  fk <- fligner.test(aux$var, aux$group)
  aux1 <- if(levene$`Pr(>F)`[1] < significance){
    paste0("Levene test: Heteroskedastico al ", significance, ". Resultado Chow: dudoso")
  } else {
    paste0("Levene test: Homoskedástico al ", significance, ". Resultado Chow: probable")
  }
  
  aux2 <- if(fk$p.value < significance){
    paste0("Fligner-Killeen: Heteroskedastico al ", significance, ". Resultado Chow: dudoso")
  } else {
    paste0("Fligner-Killeen: Homoskedástico al ", significance, ". Resultado Chow: probable")
  }
  
  salida <- list(Homoscedastico = list(levene = aux1, fk = aux2),
                 discontinuidad_sandwich = chow_linear,
                 discontinuidad_robust = chow_robust,
                 f = as.numeric(linear_chow$test$fstat),
                 fcritico = fcritico,
                 maxt_linear = broom::tidy(linear_chow_maxt) %>%
                 inner_join(broom::tidy(confint(aux_linear_chow, 
                                                level = 1 - significance))) %>% 
                   mutate(p.value = format(adj.p.value, scientific = FALSE) %>% 
                            as.numeric() %>% 
                            round(3) 
                   )%>% 
                   select('Parámetro' = contrast, Beta = estimate, 
                          'Error Est.' = std.error, 'IC inferior' = conf.low, 
                          'IC superior' = conf.high, 'P ajustado' = p.value),
                 maxt_robust = broom::tidy(robust_chow_maxt) %>%
                 inner_join(broom::tidy(confint(aux_robust_chow, 
                                                level = 1 - significance))) %>% 
                   mutate(p.value = format(adj.p.value, scientific = FALSE) %>% 
                            as.numeric() %>% 
                            round(3) 
                   ) %>%
                   select('Parámetro' = contrast, Beta = estimate, 
                          'Error Est.' = std.error, 'IC inferior' = conf.low, 
                          'IC superior' = conf.high, 'P ajustado' = p.value),
                 MM = broom::tidy(robust_completo) %>%
                  inner_join(broom::tidy(confint(aux_robust_completo, 
                                                 level = 1 - significance))) %>% 
                  mutate(p.value = format(adj.p.value, scientific = FALSE) %>% 
                           as.numeric() %>% 
                           round(3)
                  ) %>% 
                  select('Parámetro' = contrast, Beta = estimate, 'Error Est.' = std.error,
                         'IC inferior' = conf.low, 'IC superior' = conf.high, 'P ajustado' = p.value)
  )
  return(salida)
  message(salida$Homoscedastico$levene)
  message(salida$Homoscedastico$fk)
  message("F = ", salida$f)
  message("F critico = ", salida$fcritico)
  message("Mediante estimador sandwich: ", discontinuidad_robust)
}

# Aplicamos chow_dummy a cada transición de periodo
aux1 <- chow_dummy(df_3269, df_3271, mod3_restr, mod3_unrestr, significance = 0.01)
aux2 <- chow_dummy(df_3271, df_3273, mod3_restr, mod3_unrestr, significance = 0.01)
aux3 <- chow_dummy(df_3273, df_3277, mod3_restr, mod3_unrestr, significance = 0.01)
aux4 <- chow_dummy(df_3277, df_3279, mod3_restr, mod3_unrestr, significance = 0.01)
aux5 <- chow_dummy(df_3279, df_3281, mod3_restr, mod3_unrestr, significance = 0.01)

# Alternativa con un alfa = 0.05
aux1 <- chow_dummy(df_3269, df_3271, mod3_restr, mod3_unrestr, significance = .05)
aux2 <- chow_dummy(df_3271, df_3273, mod3_restr, mod3_unrestr, significance = .05)
aux3 <- chow_dummy(df_3273, df_3277, mod3_restr, mod3_unrestr, significance = .05)
aux4 <- chow_dummy(df_3277, df_3279, mod3_restr, mod3_unrestr, significance = .05)
aux5 <- chow_dummy(df_3279, df_3281, mod3_restr, mod3_unrestr, significance = .05)


#hay una discontinuidad en aux2 al 0.05 pero no al 0.01
#hay una discontinuidad en aux4 


#comprobamos la homocedasticidad
aux1$Homoscedastico
aux2$Homoscedastico
aux3$Homoscedastico #Heteroskedastico
aux4$Homoscedastico #Heteroskedastico
aux5$Homoscedastico

#-----------------Visualización del contraste de hipótesis----------------
#Creamos una tabla con todos los resultados de los test max-t
#indicamos a que periodo corresponde cada contraste
#cada periodo implica una transición. periodo 1 es la transicion entre
#los datos del posteletoral y el barómetro de enero
aux1$maxt_robust <- within(aux1$maxt_robust, {periodo <- 1})
aux2$maxt_robust <- within(aux2$maxt_robust, {periodo <- 2})
aux3$maxt_robust <- within(aux3$maxt_robust, {periodo <- 3})
aux4$maxt_robust <- within(aux4$maxt_robust, {periodo <- 4})
aux5$maxt_robust <- within(aux5$maxt_robust, {periodo <- 5})
#fusionamos
bind_rows(aux1$maxt_robust, aux2$maxt_robust, aux3$maxt_robust, aux4$maxt_robust, aux5$maxt_robust) %>% 
  mutate(periodo = factor(periodo, 
                          levels= 1:5,
                          labels = paste("Test", 1:5)
                          ),
         facet = `Parámetro` %>% 
           #hay que cambiar las formulas de las etiquetas. solo ejemplo
           #https://rstudio-pubs-static.s3.amazonaws.com/136237_170402e5f0b54561bf7605bdea98267a.html
           #to write powers like 2^3 --> 2^{3}
           factor(levels = unique(aux1$maxt_robust$`Parámetro`),
                  labels = c("tau[0]", "tau[1]",
                             "tau[2]", "tau[3]")
                  ),
         label = case_when(`P ajustado` <= .05 & `P ajustado` >0.01  ~ "*",
                           `P ajustado` <= .01 & `P ajustado` >0.001 ~ "**",
                           `P ajustado` <= .001                      ~ "***",
                           TRUE                                      ~ "")
         ) %>% 
  ggplot(aes(x = periodo, 
             y = Beta, 
             group =  `Parámetro`)) + 
  geom_point() + 
  geom_text(aes(label = label),
            hjust = 1.5, 
            vjust = 0)+
  geom_errorbar(aes(ymin=`IC inferior`, 
                    ymax=`IC superior`, 
                    width = .1)) + 
  geom_hline(yintercept = 0, 
             colour = "red", 
             linetype = "dashed") +
  geom_vline(xintercept = 3.5, 
             colour = "darkblue", 
             linetype = "dotted", 
             size = 1) + 
  labs(x = "",
       y = "Valor coeficientes \u03c4 con IC = 99%")+
  facet_wrap(.~facet, 
             scales = "free", 
             labeller = label_parsed)

# Repetimos el analisis con la estimación de la matriz var- cov sandwich

#los datos del posteletoral y el barómetro de enero
aux1$maxt_linear <- within(aux1$maxt_linear, {test <- 1})
aux2$maxt_linear <- within(aux2$maxt_linear, {test <- 2})
aux3$maxt_linear <- within(aux3$maxt_linear, {test <- 3})
aux4$maxt_linear <- within(aux4$maxt_linear, {test <- 4})
aux5$maxt_linear <- within(aux5$maxt_linear, {test <- 5})
#fusionamos
bind_rows(aux1$maxt_linear, aux2$maxt_linear, aux3$maxt_linear, aux4$maxt_linear, aux5$maxt_linear) %>% 
  mutate(test = factor(test, 
                          levels= 1:5,
                          labels = c("Test 1", "Test 2", "Test 3", "Test 4", "Test 5")
  ),
  facet = `Parámetro` %>% 
    #hay que cambiar las formulas de las etiquetas. solo ejemplo
    #https://rstudio-pubs-static.s3.amazonaws.com/136237_170402e5f0b54561bf7605bdea98267a.html
    #to write powers like 2^3 --> 2^{3}
    factor(levels = unique(aux1$maxt_linear$`Parámetro`),
           labels = c("tau[0]", "tau[1]",
                      "tau[2]", "tau[3]")
    )
  ) %>% 
  ggplot(aes(x = periodo, y = Beta, group =  `Parámetro`)) + 
  geom_point() + 
  geom_errorbar(aes(ymin=`IC inferior`, ymax=`IC superior`, width = .1)) + 
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") +
  geom_vline(xintercept = 3.5, colour = "darkblue", linetype = "dotted", size = 1) +
  facet_wrap(.~facet, scales = "free", labeller = label_parsed)
# resultado más favorable con sandwich

#-----------------Test QLR------------------------------------------------
#Este test sirve para detectar en qué momento o parte de un dataset hay
#un cambio estructural. Vamos a obviarlo ya que es redundante

F_serie <- c('Enr' = aux1$f, 'Feb' =aux2$f, 'Mar' = aux3$f, 'Abr' = aux4$f, 'May' = aux5$f)
Fcritico_serie <- c('Enr' = aux1$fcritico, 'Feb' =aux2$fcritico, 'Mar' = aux3$fcritico, 
                    'Abr' = aux4$fcritico, 'May' = aux5$fcritico)
tiempos <- names(F_serie) %>% factor(levels = names(F_serie))
qlr <- if_else(F_serie == max(F_serie), "QLR", "")
cambio <- if_else(F_serie > Fcritico_serie, 1, 0) %>% factor(levels = c("0", "1"), 
                                                             labels = c("estable","cambio"))
QLR_data <- data.frame(Tiempo = tiempos, F = F_serie, Crit = Fcritico_serie, 
                       qlr = qlr, cambio = cambio)
QLR_test <- ggplot(QLR_data, aes(x= Tiempo)) + 
  geom_line(aes(y = F, group= 1)) + 
  geom_point(aes(y = F, color = cambio), size = 1.5)+
  geom_line(aes(x= Tiempo, y = Crit, group= 1), linetype = "dashed")
#-----------------Marginal Effects---------------------------------------------

