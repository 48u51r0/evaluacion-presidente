chow_robust <- function(pre, post, formula, significance = .05){
  pooled <- bind_rows(pre, post)
  models <- list("pre" = pre,"post"= post, "pooled"=pooled) %>% 
    map(~lmrob(eval_pres~distideo_GMC + distideo_2 + man+higher_educ+welloff,
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

