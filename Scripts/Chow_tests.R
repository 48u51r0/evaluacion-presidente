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