#-----------------Loading datasets------------------------------------------

df_3269 <- read_rds("df_3269.rds")
df_3271 <- read_rds("df_3271.rds")
df_3273 <- read_rds("df_3273.rds")
df_3277 <- read_rds("df_3277.rds")

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
  pooled <- bind_rows(pre, post) %>% 
    mutate(breakpoint = case_when(Periodo == min(Periodo) ~ 0,
                                  Periodo == max(Periodo) ~ 1) %>% 
             factor()
           )
  reg <- robustbase::lmrob(formula = formula, data = pooled,weights = PESO)
  K <- cbind(1, diag(coef(reg))-1)
  rownames(K) <- names(coef(reg))
}
""""""""""""""""""""""""
la matriz K para chow debe ser: una fila y una columna por cada parametro incluido el intercepto
Despues nos quedamos solo con las filas de los parámetros sobre los que queremos hacer una inferencia
En la columna del parametro sobre el que hacemos inferencia y su fila correspondiente va un 1 y los
demás elementos va un cero.
Podemos hacer un Test F. (Chow)

pre <- df_3269
post <- df_3271
formula <- ideo_unrestr
pooled <- bind_rows(pre, post) %>% 
  mutate(breakpoint = case_when(Periodo == min(Periodo) ~ 0,
                                Periodo == max(Periodo) ~ 1) %>% 
           factor()
  )
reg <- robustbase::lmrob(formula = formula, data = pooled,weights = PESO)
K <- cbind(diag(length(coef(reg))))
rownames(K) <- names(coef(reg))
aux_linear <- summary(linear <- lm(formula, data = pooled, weights = PESO))
aux_robust <- summary(robust <- robustbase::lmrob(formula, data = pooled, weights = PESO))
linear_glht <- glht(linear, linfct = k_chow)
robust_glht <- glht(robust, linfct = k_chow)
aux_linear_glht <- summary(linear_glht, test = Ftest())
aux_robust_glht <- summary(robust_glht, test =Ftest())

aux_linear_glht$test$fstat
aux_linear_glht$test$pvalue

#valor critico de F
qf(c(1-.05, 1-.01, 1-.001), aux_linear_glht$test$df[1], aux_linear_glht$test$df[2])

También podemos hacer una maxi T con todos los parámetros = 0 para estimar. este test maxi-t