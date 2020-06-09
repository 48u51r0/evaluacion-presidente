#-----------------Loading datasets------------------------------------------

df_3269 <- read_rds("df_3269.rds")
df_3271 <- read_rds("df_3271.rds")
df_3273 <- read_rds("df_3273.rds")
df_3277 <- read_rds("df_3277.rds")

#------------------Generate pooled data---------------------------------------

pooled <- bind_rows(df_3269, df_3271, df_3273, df_3277) %>%
  mutate(Periodo = factor(Periodo, levels = 1:5, 
                          labels = c("Poselectoral", "Enero", "Febrero", "Marzo", "Abril")
  ),
  ideologia = factor(ideol_pers, levels = 1:10)
  )

#--------------------Visualise means with errorbars--------------------------------

ggplot(pooled, aes(y = eval_pres, x = ideologia))+
  stat_summary(fun=mean,geom="point",aes(group= Periodo,colour=Periodo))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .1, aes(group= Periodo,colour=Periodo))+
  stat_summary(fun = mean, geom = "line",aes(group= Periodo,colour=Periodo)) + 
  labs(x = "Autoubicación ideológica (1-10)", y = "Valoración del presidente (1-10)")

# 2-way ANOVA------------------------------------------

mod_int <- lm(eval_pres ~ ideologia * Periodo, data=pooled, weights = PESO)
mod_ad <- lm(eval_pres ~ ideologia + Periodo, data=pooled, weights = PESO)
fit_int <- aov(mod_int)
fit_ad <- aov(mod_ad)
summary(fit_int)
summary(fit_ad)
#we see there are main effects and interaction effects

# Pair wise t-tests-----------------------------------------------------------------------------

#comprobar en el libro-manual
pairwise.t.test(pooled$eval_pres,pooled$niveduc,p.adjust="bonferroni")
pairwise.t.test(pooled$tvhoras,pooled$niveduc,p.adjust="bonferroni")

# Ordinary Tukey---------------------------------------------------------------------------------

tukey <- fit_int %>% 
  TukeyHSD() %>% #it's a list of matrices: main fx + interaction fx
#seleccionamos las diferencias significativas al p-value 0.05
  map(~.[.[, "p adj"] <= 0.05,] %>% #substting a matrix that meets a condition
        as.data.frame %>% 
        rownames_to_column(var = "Categories") %>% 
        arrange(Categories)
  )
 
interaction <- tukey %>% 
  pluck("ideologia:Periodo") %>% #selecciona el elemento de una lista
  mutate(aux_ideol1 = str_match(Categories, pattern = "^(.*?):")[, 2] %>% as.numeric(),
         aux_periodo1 = str_match(Categories, pattern = ":(.*?)-")[, 2] %>% 
           factor(levels = levels(pooled$Periodo)),
         aux_ideol2  = str_match(Categories, pattern = "-(.*?):")[, 2] %>% as.numeric(),
         aux_periodo2= word(Categories, 3, sep = fixed(':')) %>% 
           factor(levels = levels(pooled$Periodo)),
         periodo_1 = if_else(as.numeric(aux_periodo1) <= as.numeric(aux_periodo2), 
                             aux_periodo1, aux_periodo2),
         ubicacion_1 = if_else(aux_periodo1 == periodo_1,
                               if_else(aux_periodo1 == aux_periodo2,
                                       pmin(aux_ideol1, aux_ideol2),
                                       aux_ideol1),
                               aux_ideol2),
         periodo_2 = if_else(aux_periodo1 == periodo_1, aux_periodo2, aux_periodo1),
         ubicacion_2 = if_else(ubicacion_1 == aux_ideol1,
                               aux_ideol2,
                               aux_ideol1),
         diff_adj = if_else(aux_ideol1 != ubicacion_1 | aux_periodo1 != periodo_1, -1*diff, diff),
         upr_adj = diff_adj + abs((upr - lwr) / 2),
         lwr_adj = diff_adj - abs((upr - lwr) / 2)
  ) %>% 
  arrange(ubicacion_1, periodo_1, periodo_2, ubicacion_2) %>% 
  select(ubicacion_1, periodo_1, ubicacion_2, periodo_2, diff_adj, upr_adj, lwr_adj, 'p adj')

mainfx_ideologia <- tukey %>% 
  pluck("ideologia") %>% 
  mutate(aux_ideol1 = str_match(Categories, pattern = "^(.*?)-")[, 2] %>% as.numeric(),
         aux_ideol2 = str_match(Categories, pattern = "-(.*?)$")[, 2] %>% as.numeric(),
         ubicacion_1 = pmin(aux_ideol1, aux_ideol2),
         ubicacion_2 = pmax(aux_ideol1, aux_ideol2),
         diff_adj = if_else(ubicacion_1 == aux_ideol1, diff, -1*diff),
         upr_adj = diff_adj + abs((upr - lwr) / 2),
         lwr_adj = diff_adj - abs((upr - lwr) / 2)
  ) %>% 
  arrange(ubicacion_1, ubicacion_2) %>% 
  select(ubicacion_1, ubicacion_2, diff_adj, upr_adj, lwr_adj, 'p adj')

mainfx_Periodo <- tukey %>% 
  pluck("Periodo") %>% 
  mutate(aux_periodo1 = str_match(Categories, pattern = "^(.*?)-")[, 2] %>% 
           factor(levels = levels(pooled$Periodo)),
         aux_periodo2 = str_match(Categories, pattern = "-(.*?)$")[, 2] %>% 
           factor(levels = levels(pooled$Periodo)),
         periodo_1 = if_else(as.numeric(aux_periodo1) <= as.numeric(aux_periodo2), 
                               aux_periodo1, aux_periodo2),
         periodo_2 = if_else(aux_periodo1 == periodo_1, aux_periodo2, aux_periodo1),
         diff_adj = if_else(periodo_1 == aux_periodo1, diff, -1*diff),
         upr_adj = diff_adj + abs((upr - lwr) / 2),
         lwr_adj = diff_adj - abs((upr - lwr) / 2)
  ) %>% 
  arrange(periodo_1, periodo_2) %>% 
  select(periodo_1, periodo_2, diff_adj, upr_adj, lwr_adj, 'p adj')

# Ajuste de p valores para inferencias simultáneas-----------------------------------------

#We apply the functions for multiple comparisons Hothron, Bretz & Westfall (2008)
#We try to reduce the Type I error inflation due to simultaneous inferences
# Se trata de un ajuste de los p valores

tukey_glht_int <- glht(model = mod_int, 
                 linfct = lsm(pairwise ~ ideologia:Periodo), 
                 by = NULL)
#para calcular los efectos principales necesitamos usar el modelo de efectos aditivos
tukey_glht_ideologia <- glht(model = mod_ad, 
                      linfct = mcp(ideologia = "Tukey"), 
                      by = NULL)
tukey_glht_Periodo <- glht(model = mod_ad, 
                         linfct = mcp(Periodo = "Tukey"), 
                         by = NULL)
tukey_glht_sw_int <- glht(model = mod_int, 
                    linfct = lsm(pairwise ~ ideologia:Periodo), 
                    by = NULL, 
                    vcov = sandwich)

tukey_glht_sw_ideologia <- glht(model = mod_ad, 
                         linfct = mcp(ideologia = "Tukey"), 
                         by = NULL, 
                         vcov = sandwich)

tukey_glht_sw_Periodo <- glht(model = mod_ad, 
                                linfct = mcp(Periodo = "Tukey"), 
                                by = NULL, 
                                vcov = sandwich)

intervals_glht_int <- confint(tukey_glht_int, level = .95)$confint %>% 
  as.data.frame() %>% 
  rownames_to_column()

intervals_glht_sw_int <- confint(tukey_glht_sw_int)$confint %>% 
  as.data.frame() %>% 
  rownames_to_column()

intervals_glht_sw_periodo <- confint(tukey_glht_sw_Periodo)$confint %>% 
  as.data.frame() %>% 
  rownames_to_column()

intervals_glht_sw_ideologia <- confint(tukey_glht_sw_ideologia)$confint %>% 
  as.data.frame() %>% 
  rownames_to_column()

interaction_glht <- summary(tukey_glht_int) %>% 
  broom::tidy() %>% 
  inner_join(intervals_glht_int, by = c("lhs" = "rowname")) %>% 
  select(lhs, Estimate, lwr, upr, p.value) %>% 
  write_rds("interaction_glht.rds")

interaction_glht_sw <- summary(tukey_glht_sw_int) %>% 
  broom::tidy() %>% 
  inner_join(intervals_glht_sw_int, by = c("lhs" = "rowname")) %>% 
  select(lhs, Estimate, lwr, upr, p.value) %>% 
  write_rds("interaction_glht_sw.rds") 

interaction_adj <- interaction_glht_sw %>%  
  mutate(aux_ideol1 = str_match(lhs, pattern = "^(.*?),")[, 2] %>% as.numeric(),
         aux_periodo1 = str_match(lhs, pattern = ",(.*?) - ")[, 2] %>% 
           factor(levels = levels(pooled$Periodo)),
         aux_ideol2  = str_match(lhs, pattern = "- (.*?),")[, 2] %>% as.numeric(),
         aux_periodo2= word(lhs, 3, sep = fixed(',')) %>% 
           factor(levels = levels(pooled$Periodo)),
         periodo_1 = if_else(as.numeric(aux_periodo1) <= as.numeric(aux_periodo2), 
                             aux_periodo1, aux_periodo2),
         ubicacion_1 = if_else(aux_periodo1 == periodo_1,
                               if_else(aux_periodo1 == aux_periodo2,
                                       pmin(aux_ideol1, aux_ideol2),
                                       aux_ideol1),
                               aux_ideol2),
         periodo_2 = if_else(aux_periodo1 == periodo_1, aux_periodo2, aux_periodo1),
         ubicacion_2 = if_else(ubicacion_1 == aux_ideol1,
                               aux_ideol2,
                               aux_ideol1),
         Estimate_adj = if_else(aux_ideol1 != ubicacion_1 | aux_periodo1 != periodo_1, -1*Estimate, Estimate),
         upr_adj = Estimate_adj + abs((upr - lwr) / 2),
         lwr_adj = Estimate_adj - abs((upr - lwr) / 2)
  ) %>% 
  arrange(ubicacion_1, periodo_1, periodo_2, ubicacion_2) %>% 
  select(ubicacion_1, periodo_1, ubicacion_2, periodo_2, Estimate_adj, upr_adj, lwr_adj, p.value)
  

# Se producen diferencias, entre usar sandwich o no. Hay más diferencias significativas al usarlo.
#A partir de ahora todas las inferencias las hacemos mediante sw

mainfx_Periodo_glht_sw <- summary(tukey_glht_sw_Periodo) %>% 
  broom::tidy() %>% 
  inner_join(intervals_glht_sw_periodo, by = c("lhs" = "rowname")) %>% 
  select(lhs, Estimate, lwr, upr, p.value) %>% 
  write_rds("mainfx_Periodo_glht_sw.rds")

mainfx_Periodo_adj <- mainfx_Periodo_glht_sw %>% 
  mutate(aux_periodo1 = str_match(lhs, pattern = "^(.*?) -")[, 2] %>% 
           factor(levels = levels(pooled$Periodo)),
         aux_periodo2 = str_match(lhs, pattern = "- (.*?)$")[, 2] %>% 
           factor(levels = levels(pooled$Periodo)),
         periodo_1 = if_else(as.numeric(aux_periodo1) <= as.numeric(aux_periodo2), 
                             aux_periodo1, aux_periodo2),
         periodo_2 = if_else(aux_periodo1 == periodo_1, aux_periodo2, aux_periodo1),
         Estimate_adj = if_else(periodo_1 == aux_periodo1, Estimate, -1*Estimate),
         upr_adj = Estimate_adj + abs((upr - lwr) / 2),
         lwr_adj = Estimate_adj - abs((upr - lwr) / 2)
  ) %>% 
  arrange(periodo_1, periodo_2) %>% 
  select(periodo_1, periodo_2, Estimate_adj, upr_adj, lwr_adj, p.value)

mainfx_ideologia_glht_sw <- summary(tukey_glht_sw_ideologia) %>% 
  broom::tidy() %>% 
  inner_join(intervals_glht_sw_ideologia, by = c("lhs" = "rowname")) %>% 
  select(lhs, Estimate, lwr, upr, p.value) %>% 
  write_rds("mainfx_ideologia_glht_sw.rds")

mainfx_ideologia_adj <- mainfx_ideologia_glht_sw %>% 
  mutate(aux_ideol1 = str_match(lhs, pattern = "^(.*?) -")[, 2] %>% as.numeric(),
         aux_ideol2 = str_match(lhs, pattern = "- (.*?)$")[, 2] %>% as.numeric(),
         ubicacion_1 = pmin(aux_ideol1, aux_ideol2),
         ubicacion_2 = pmax(aux_ideol1, aux_ideol2),
         Estimate_adj = if_else(ubicacion_1 == aux_ideol1, Estimate, -1*Estimate),
         upr_adj = Estimate_adj + abs((upr - lwr) / 2),
         lwr_adj = Estimate_adj - abs((upr - lwr) / 2)
  ) %>% 
  arrange(ubicacion_1, ubicacion_2) %>% 
  select(ubicacion_1, ubicacion_2, Estimate_adj, upr_adj, lwr_adj, p.value)

comparaciones <- list(interaccion = interaction_adj, 
                      ideologia = mainfx_ideologia_adj, 
                      periodo = mainfx_Periodo_adj) %>% 
  map(~.[.[, "p.value"] <= 0.05,])

write_rds(comparaciones$interaccion, "interaccion_final.rds")
write_rds(comparaciones$ideologia, "ideologia_final.rds")
write_rds(comparaciones$periodo, "periodo_final.rds")
