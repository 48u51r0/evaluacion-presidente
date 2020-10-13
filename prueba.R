create <- function(significance = 0.95, ...){
  aux <- list(...)
  pooled <- list()
  formula <- "eval_pres ~ breakpoint*ns(ideol_GMC, 3) + man + higher_educ + welloff"
  for (i in seq_len(length(aux)-1)){
    pre <- aux[[i]]
    post <- aux[[i+1]]
    pooled[[i]] <- bind_rows(pre, post) %>% 
      mutate(breakpoint = case_when(Periodo == min(Periodo) ~ "Antes",
                                    Periodo == max(Periodo) ~ "Después") %>% 
               factor()
      )
  }
  
  df <- pooled %>%
    map(~lmrob(eval_pres ~ breakpoint*ns(ideol_GMC, 3) + man + higher_educ + welloff, 
               data= ., 
               weights = PESO)) %>% 
    map(~ggpredict(., c("ideol_GMC[all]", "breakpoint"), ci.lvl = significance)) %>% 
    map(~as_tibble(.))
  for (i in seq_algon(df)){
    df[[i]] <- df[[i]] %>% 
      mutate(transition = case_when(i == 1 ~ "Noviembre-Enero",
                                    i == 2 ~ "Enero-Febrero",
                                    i == 3 ~ "Febrero-Marzo ",
                                    i == 4 ~ "Marzo-Abril",
                                    i == 5 ~ "Abril-Mayo",
                                    TRUE ~ "Ampliar el script"))
  }
  df <- bind_rows(df)
  rm(pre, post, aux, formula) 
  #df <- bind_rows(pooled)
  return(df)
}


