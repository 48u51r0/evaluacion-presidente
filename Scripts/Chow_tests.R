#-----------------Loading datasets------------------------------------------

df_3269 <- read_rds("df_3269.rds")
df_3271 <- read_rds("df_3271.rds")
df_3273 <- read_rds("df_3273.rds")
df_3277 <- read_rds("df_3277.rds")
df_3279 <- read_rds("df_3279.rds")
df_3281 <- read_rds("df_3281.rds")

#-----------------Function to run Chow tests (Lee, 2008)--------------------------------

chow <- function(pre, post, formula, significance = .05){
  pooled <- bind_rows(pre, post)
  models <- list("pre" = pre,"post"= post, "pooled"=pooled) %>% 
    map(~lm(eval_pres~distideo_GMC + distideo_2 + man+higher_educ+welloff,
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
  if(F_chow > F_critical){
    cat("Se ha producido un cambio estructural\n")
  } else{
    cat("No se ha producido un cambio estructural\n")
  }
}

#-----------------Several models of relations between DV and IV--------------

mod1 <- "eval_pres~distideo_GMC + distideo_2 + distideo_3 + man+higher_educ+welloff"
mod2 <- "dist_eval~distideo_GMC + distideo_2 + distideo_3 + man+higher_educ+welloff"
mod3 <- "evalpres_GMC~distideo_GMC + distideo_2 + distideo_3 + man+higher_educ+welloff"
modelo <- list(mod1, mod2)

#robust standard error in the regression
library(lmtest)
library(sandwich)
#m <- lm(formula, data)
#coeftest(m, vcov = vcovHC(m, type="HC1"))

#Robust linear regression(MM Estimator)
#robustbase::lmrob(formula, data, weights)

#-----------------Running Chow tests----------------------------------------------

#3269 - 3271
df_pre <- df_3269 
df_post <- df_3271
chow(df_pre, df_post, mod1, 0.01) #Stable
chow(df_pre, df_post, mod2, 0.01)
map(modelo, ~chow(df_pre, df_post, ., 0.01))
#3271 - 3273
df_pre <- df_3271 
df_post <- df_3273 
chow(df_pre, df_post, mod1, 0.01)
chow(df_pre, df_post, mod2, 0.01)

#3269+3271 - 3273
df_pre <- bind_rows(df_3269, df_3271) 
df_post <- df_3273
chow(df_pre, df_post, mod1, 0.01)
chow(df_pre, df_post, mod2, 0.01)

#3269 - 3273
df_pre <- df_3269
df_post <- df_3273
chow(df_pre, df_post, mod1, 0.01)
chow(df_pre, df_post, mod2, 0.01)
#test dist_eval

#Dummy variable alternative to Chow--------------------------------------------

#formulae
dist_unrestr <- "eval_pres ~ distideo_GMC*breakpoint + distideo_2*breakpoint + 
distideo_3*breakpoint + man+higher_educ+welloff"
dist_restr <- "eval_pres ~ distideo_GMC + distideo_2 + distideo_3 + man+
higher_educ+welloff"
ideo_unrestr <- "eval_pres ~ breakpoint*ideolpers_GMC + breakpoint*ideol_2 + 
breakpoint*ideol_3 + man + higher_educ + welloff"
ideo_restr <- "eval_pres ~ ideolpers_GMC + ideol_2 + ideol_3 + 
man+higher_educ+welloff"

chow_dummy <- chow <- function(pre, post, formula, significance = .05){
  k_chow <- read_rds("k_chow.rds")
  pooled <- bind_rows(pre, post) %>% 
    mutate(breakpoint = case_when(Periodo == min(Periodo) ~ 0,
                                  Periodo == max(Periodo) ~ 1) %>% 
             factor()
    )
  linear <- lm(formula, data = pooled, weights = PESO)
  robust <- robustbase::lmrob(formula, data = pooled, weights = PESO)
  K <- cbind(diag(length(coef(robust))))
  rownames(K) <- names(coef(robust))
  aux_linear_chow <- glht(linear, linfct = k_chow, vcov =sandwich)
  aux_linear_completo <- glht(linear, linfct = K, vcov= sandwich)
  aux_robust_chow <- glht(robust, linfct = k_chow)
  aux_robust_completo <- glht(robust, linfct = K)
  linear_chow <- summary(aux_linear_chow, test = Ftest())
  robust_chow <- summary(aux_robust_chow, test =Ftest())
  linear_completo <- summary(aux_linear_completo)
  robust_completo <- summary(aux_robust_completo)
  chow <- if_else(linear_chow$test$fstat > qf(1-significance, 
                                              linear_chow$test$df[1], 
                                              linear_chow$test$df[2]), 
                  paste0("Hay discontinuidad en la regresión al ", significance), 
                  paste0("No hay discontinuidad en la regresión al ", significance)
  )
  salida <- list(discontinuidad = chow, 
                 MM = broom::tidy(robust_completo )%>% 
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
aux1 <- chow_dummy(df_3269, df_3271, formula = ideo_unrestr)
aux2 <- chow_dummy(df_3271, df_3273, formula = ideo_unrestr)
aux3 <- chow_dummy(df_3273, df_3277, formula = ideo_unrestr)
aux4 <- chow_dummy(df_3277, df_3279, formula = ideo_unrestr)
aux5 <- chow_dummy(df_3279, df_3281, formula = ideo_unrestr)
# Añadimos una variable para identificar el periodo
aux1$MM <- within(aux1$MM, {periodo <- 1})
aux2$MM <- within(aux2$MM, {periodo <- 2})
aux3$MM <- within(aux3$MM, {periodo <- 3})
aux4$MM <- within(aux4$MM, {periodo <- 4})
aux5$MM <- within(aux5$MM, {periodo <- 5})
#fusionamos
bind_rows(aux1$MM, aux2$MM, aux3$MM, aux4$MM, aux5$MM) %>% 
  mutate(periodo = factor(periodo, labels = c("Postelectoral-Enero",
                                              "Enero-Febrero",
                                              "Febrero-Marzo",
                                              "Marzo-Abril",
                                              "Abril-Mayo"))) %>% 
  ggplot(aes(x = periodo, y = Beta, group = Parameters)) + 
  geom_line() + 
  geom_point() + 
  geom_errorbar(aes(ymin=Lower, ymax=Upper, width = .1)) + 
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") +
  geom_vline(xintercept = 3.5, colour = "darkblue", linetype = "dotted") +
  facet_wrap(~Parameters, scales = "free")