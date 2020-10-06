#-----------------Loading datasets------------------------------------------

df_3269 <- read_rds("df_3269.rds")
df_3271 <- read_rds("df_3271.rds")
df_3273 <- read_rds("df_3273.rds")
df_3277 <- read_rds("df_3277.rds")
df_3279 <- read_rds("df_3279.rds")
df_3281 <- read_rds("df_3281.rds")

lista_datos <- list(df_3269, df_3271, df_3273, 
                    df_3277, df_3279, df_3281)

#-----------------Several models of relations between DV and IV--------------
mod0 <- "eval_pres~man+higher_educ+welloff"
mod1 <- "eval_pres~ideol_GMC + man+higher_educ+welloff"
mod2 <- "eval_pres~ideol_GMC + ideol_2  + man+higher_educ+welloff"
mod3 <- "eval_pres~ideol_GMC + ideol_2 + ideol_3 + man+higher_educ+welloff"
#function to apply the likelihood ratio test
lrt <- function(data = data){
  models <- list("mod0" = mod0 ,"mod1" = mod1,"mod2"= mod2, "mod3"=mod3) %>% 
    map(~lm(.,
            data = data,
            weights = PESO)
    )
  tmp1 <- lmtest::lrtest(models$mod0, models$mod1)
  tmp2 <- lmtest::lrtest(models$mod1, models$mod2)
  tmp3 <- lmtest::lrtest(models$mod2, models$mod3)
  tmp4 <- lmtest::lrtest(models$mod1, models$mod3)
  message(substitute(data), ":p-val para mod0 versus mod1: ", tmp1$`Pr(>Chisq)`[2])
  message(substitute(data), ":p-val para mod1 versus mod2: ", tmp2$`Pr(>Chisq)`[2])
  message(substitute(data), ":p-val para mod2 versus mod3: ", tmp3$`Pr(>Chisq)`[2])
  message(substitute(data), ":p-val para mod1 versus mod3: ", tmp4$`Pr(>Chisq)`[2])
}
#probamos qué modelo es más óptimo
lrt(df_3269)
lrt(df_3271)
lrt(df_3273)
lrt(df_3277)
lrt(df_3279)
lrt(df_3281)
#Se ha probado con todos los data_Sets por separado, 
#para todos los casos es modelo ampliado es mejor que el restringido.
#las excepciones han sido a partir del confinamiento ya que el modelo cuadrático
#era peor que el model lineal. Sin embargo, como el cúbico era mejor que el lineal
#usaré término cúbico (ideo_3)

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
  return(F_chow)
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

#Dummy variable alternative to Chow--------------------------------------------

#formulae

mod3_unrestr <- "eval_pres ~ breakpoint*ideol_GMC + breakpoint*ideol_2 + 
breakpoint*ideol_3 + man + higher_educ + welloff"
mod3_restr <- "eval_pres ~ ideol_GMC + ideol_2 + ideol_3 + 
man+higher_educ+welloff"

#Dummy Chow function
chow_dummy <- chow <- function(pre, post, significance = .05){
  unrestricted <- mod3_unrestr
  restricted <- mod3_restr
  #definimos el pooled data set con una dummy de punto discontinuidad
  pooled <- bind_rows(pre, post) %>% 
    mutate(breakpoint = case_when(Periodo == min(Periodo) ~ 0,
                                  Periodo == max(Periodo) ~ 1) %>% 
             factor()
    )
  #definimos los modelos lineales y robusto
  lin_restr <- lm(restricted, data = pooled, weights = PESO)
  lin_unrestr <- lm(unrestricted, data = pooled, weights = PESO)
  robust_unrestr <- robustbase::lmrob(unrestricted, data = pooled, weights = PESO)
  #definimos las matrices de hipótesis (multcomp)
  k_chow <- read_rds("k_chow.rds") #Solo los coeficientes con la dummy son = 0
  K <- cbind(diag(length(coef(robust_unrestr)))) #todos los coeficientes = 0
  rownames(K) <- names(coef(robust_unrestr)) 
  #Definimos los objeto multcomp del test de chow
  #los modelos chow se utilizan para la inferencia multiple de las variables dummy
  #usamos dos alternativas robustas ante la heteroscedasticidad:
  #sandwich y estimador MM (regresion robusta)
  aux_linear_chow <- glht(lin_unrestr, linfct = k_chow, vcov =sandwich)
  aux_robust_chow <- glht(robust_unrestr, linfct = k_chow)
  #Calculamos los F-statistic 
  linear_chow <- summary(aux_linear_chow, test = Ftest())
  robust_chow <- summary(aux_robust_chow, test =Ftest())
  
  aux_linear_completo <- glht(lin_unrestr, linfct = K, vcov= sandwich)
  aux_robust_completo <- glht(robust_unrestr, linfct = K)
  
  linear_completo <- summary(aux_linear_completo)
  robust_completo <- summary(aux_robust_completo)
  #Calculamos el valor F-crítico. No importa si usamos linear_chow o robust_chow
  fcritico <- qf(1-significance, 
                 linear_chow$test$df[1], 
                 linear_chow$test$df[2])
  chow_linear <- if_else(linear_chow$test$fstat > fcritico, 
                         paste0("Hay discontinuidad en la regresión al ", significance), 
                         paste0("No hay discontinuidad en la regresión al ", significance)
  )
  aux <- data.frame(var = c(linear$residuals, lin_restr$residuals),
                    group = c(rep(0, nobs(linear)), rep(1, nobs(lin_restr))) %>% 
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
                 discontinuidad = chow_linear,
                 f = as.numeric(linear_chow$test$fstat),
                 fcritico = fcritico,
                 MM = broom::tidy(robust_completo ) %>%
                   inner_join(broom::tidy(confint(aux_robust_completo))) %>% 
                   mutate(p.value = format(p.value, scientific = FALSE) %>% 
                            as.numeric() %>% 
                            round(3)
                   ) %>% 
                   select(Parameters = lhs, Beta = estimate, Lower = conf.low, 
                          Upper = conf.high, 'P value' = p.value)
  )
  return(salida)
}

# Aplicamos chow_dummy a cada transición de periodo
aux1 <- chow_dummy(df_3269, df_3271, significance = 0.01)
aux2 <- chow_dummy(df_3271, df_3273, significance = 0.01)
aux3 <- chow_dummy(df_3273, df_3277, significance = 0.01)
aux4 <- chow_dummy(df_3277, df_3279, significance = 0.01)
aux5 <- chow_dummy(df_3279, df_3281, significance = 0.01)

#comprobamos la homocedasticidad
aux1$Homoscedastico
aux2$Homoscedastico
aux3$Homoscedastico
aux4$Homoscedastico
aux5$Homoscedastico
#todas las transiciones son homoscedasticas

# Añadimos una variable para identificar el periodo
aux1$MM <- within(aux1$MM, {periodo <- 1})
aux2$MM <- within(aux2$MM, {periodo <- 2})
aux3$MM <- within(aux3$MM, {periodo <- 3})
aux4$MM <- within(aux4$MM, {periodo <- 4})
aux5$MM <- within(aux5$MM, {periodo <- 5})
#fusionamos
bind_rows(aux1$MM, aux2$MM, aux3$MM, aux4$MM, aux5$MM) %>% 
  mutate(periodo = factor(periodo, labels = c("Enr",
                                              "Feb",
                                              "Mar",
                                              "Abr",
                                              "May"))) %>% 
  filter(!str_detect(Parameters, "higher|welloff|man")) %>% 
  ggplot(aes(x = periodo, y = Beta, group = Parameters)) + 
  geom_line() + 
  geom_point() + 
  geom_errorbar(aes(ymin=Lower, ymax=Upper, width = .1)) + 
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") +
  geom_vline(xintercept = 3.5, colour = "darkblue", linetype = "dotted", size = 1) +
  facet_wrap(~Parameters, scales = "free")

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