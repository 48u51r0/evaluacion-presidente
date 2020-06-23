# --------------Instalar paquetes-------------------------------


rm(list = ls())
install_pack <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, library, character.only = TRUE)
}
packages <- (c("haven", "sandwich", "multcomp", "emmeans", "tidyverse", "conflicted"))
install_pack(packages)
# resolvemos los conflictos de funciones entre dplyr y MASS
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
rm("packages", "install_pack")

# --------------Importar datos-------------------------------------

backup_3269 <- read_sav("Data_Sources/3269_Postelectoral.sav")
backup_3271 <- read_sav("Data_Sources/3271_BaromEnero.sav")
backup_3273 <- read_sav("Data_Sources/3273_BaromFebrero.sav")
backup_3277 <- read_sav("Data_Sources/3277.sav")
backup_3279 <- read_sav("Data_Sources/3279.sav")
backup_3281 <- read_sav("Data_Sources/3281.sav")

# guardar los archivos de datos originales
write_rds(backup_3269, "backup_3269.rds")
write_rds(backup_3271, "backup_3271.rds")
write_rds(backup_3273, "backup_3273.rds")
write_rds(backup_3277, "backup_3277.rds")
write_rds(backup_3279, "backup_3279.rds")
write_rds(backup_3281, "backup_3281.rds")


#----------Generar los dataframe de cada estudio-----------------------------------------

# 1. Poselectoral------------------------------------------------------------------------
df_3269 <- read_rds("backup_3269.rds") %>% 
  mutate(id = as.numeric( 
    paste0(as.character(ESTU), 
           as.character(CUES))
  ),                                              #(id)entificación exclusiva de cada caso
  Periodo = 1,
  RV = case_when(B22R %in% c(5, 6, 21,67) ~ 5, #UP
                 B22R %in% c(7, 50) ~ 7,       #+P
                 B22R == 1 ~ 1, #PP
                 B22R == 2 ~ 2, #PSOE
                 B22R == 4 ~ 4, #Cs
                 B22R == 18 ~ 18, #VOX
                 B22R >= 96 ~ NA_real_, #
                 TRUE ~ 99) %>% #otros
    factor(levels = c(1, 2, 4, 5, 7, 18, 99),
           labels = c("PP", "PSOE", "Cs", "UP", "MP","VOX", "Otros")
           ) %>% 
    relevel(2),
  eval_pres = case_when(as.numeric(B29_1)  %in% c(97, 98, 99) ~ NA_real_,
                          TRUE ~ as.numeric(B29_1)),
  evalpres_GMC = eval_pres - mean(eval_pres, na.rm = TRUE),
  B29_2 = case_when(as.numeric(B29_2) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(B29_2)),
  B29_3 = case_when(as.numeric(B29_3) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(B29_3)),
  B29_4 = case_when(as.numeric(B29_4) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(B29_4)),
  B29_5 = case_when(as.numeric(B29_5) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(B29_5)),
  B29_6 = case_when(as.numeric(B29_6) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(B29_6)),
  B29_7 = case_when(as.numeric(B29_7) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(B29_7)),
  eval_opos = pmax(B29_2, B29_3, B29_4, B29_5, B29_6, B29_7, na.rm = TRUE),
  ideol_pers = case_when(as.numeric(C3)  %in% c(97, 98, 99) ~ NA_real_,
                         TRUE ~ as.numeric(C3)),
  ideolpers_GMC = ideol_pers - mean(ideol_pers, na.rm = TRUE),
  ideol_pres = case_when(as.numeric(C5_1)  %in% c(97, 98, 99) ~ NA_real_,
                         TRUE ~ as.numeric(C5_1)),
  ideolpres_GMC = ideol_pres - mean(ideol_pres, na.rm = TRUE),
  dist_ideo = ideol_pers - ideol_pres,
  distideo_GMC= dist_ideo - mean(dist_ideo, na.rm = TRUE),
  dist_eval = eval_pres - eval_opos,
  ideol_2 = ideolpers_GMC^2,
  ideol_3 = ideolpers_GMC^3,
  distideo_2 = distideo_GMC^2,
  distideo_3 = distideo_GMC^3,
  man = case_when(C9 == 1 ~ 1L,
                  C9 == 2 ~ 0L,
                  TRUE ~ NA_integer_) %>%  
    factor(levels = c(0, 1),
           labels = c("Mujer", "Hombre")
           ) %>% 
    relevel(1),
  higher_educ = case_when(C11A %in% c(1:7, 16) ~ 0L,
                          C11A %in% 8:15 ~ 1L,
                          TRUE ~ NA_integer_) %>% 
    factor(levels = c(0, 1),
           labels = c("Universitario", "No-Universitario")
           ) %>% 
    relevel(1),
  welloff = case_when(C21 %in% 1:3 ~ 1L, #high, middle-high, middle-middle
                      C21 %in% 4:12 ~ 0L,
                      TRUE ~ NA_integer_) %>% 
    factor(levels = c(0, 1),
           labels = c("Medio-alto", "Bajo")
           ) %>% 
    relevel(1)
  ) %>% 
  select(id, Periodo, dist_eval, eval_pres, evalpres_GMC, eval_opos, ideol_pers,ideolpers_GMC, ideol_2, ideol_3, distideo_GMC, distideo_2, distideo_3, RV, man, higher_educ, welloff, PESO) %>% 
  drop_na() %>% 
  write_rds("df_3269.rds")
# 2. Enero---------------------------------------------------------------------
df_3271 <- read_rds("backup_3271.rds") %>% 
  mutate(id = as.numeric(
    paste0(as.character(ESTU), 
           as.character(CUES))
  ),
  PESO = 1,
  Periodo = 2,
  RV = case_when(RECUVOTOGR %in% c(5, 6, 21,67) ~ 5, #UP
                 RECUVOTOGR %in% c(7, 50) ~ 7,       #MPais
                 RECUVOTOGR == 1 ~ 1, #PP
                 RECUVOTOGR == 2 ~ 2, #PSOE
                 RECUVOTOGR == 4 ~ 4, #Cs
                 RECUVOTOGR == 18 ~ 18, #VOX
                 RECUVOTOGR >= 96 | RECUVOTOGR == 0 ~ NA_real_, #
                 TRUE ~ 99) %>% 
    factor(levels = c(1, 2, 4, 5, 7, 18, 99),
           labels = c("PP", "PSOE", "Cs", "UP", "MP", "VOX", "Otros")
           ) %>% 
    relevel(2),
  eval_pres = case_when(as.numeric(A16_1)  %in% c(97, 98, 99) ~ NA_real_,
                          TRUE ~ as.numeric(A16_1)),
  evalpres_GMC = eval_pres - mean(eval_pres, na.rm = TRUE),
  A16_2 = case_when(as.numeric(A16_2) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(A16_2)),
  A16_3 = case_when(as.numeric(A16_3) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(A16_3)),
  A16_4 = case_when(as.numeric(A16_4) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(A16_4)),
  A16_5 = case_when(as.numeric(A16_5) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(A16_5)),
  A16_6 = case_when(as.numeric(A16_6) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(A16_6)),
  A16_7 = case_when(as.numeric(A16_7) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(A16_7)),
  eval_opos = pmax(A16_2, A16_3, A16_4, A16_5, A16_6, A16_7, na.rm = TRUE),
  ideol_pers = case_when(as.numeric(ESCIDEOL)  %in% c(97, 98, 99) ~ NA_real_,
                         TRUE ~ as.numeric(ESCIDEOL)),
  ideolpers_GMC = ideol_pers - mean(ideol_pers, na.rm = TRUE),
  ideol_pres = case_when(as.numeric(ESCIDEOLPOLI_1)  %in% c(97, 98, 99) ~ NA_real_,
                         TRUE ~ as.numeric(ESCIDEOLPOLI_1)),
  dist_ideo = ideol_pers - ideol_pres,
  distideo_GMC= dist_ideo - mean(dist_ideo, na.rm = TRUE),
  dist_eval = eval_pres - eval_opos,
  ideol_2 = ideolpers_GMC^2,
  ideol_3 = ideolpers_GMC^3,
  distideo_2 = distideo_GMC^2,
  distideo_3 = distideo_GMC^3,
  man = case_when(SEXO == 1 ~ 1L,
                  SEXO == 2 ~ 0L,
                  TRUE ~ NA_integer_) %>% 
    factor(levels = c(0, 1),
           labels = c("Mujer", "Hombre")
           ) %>% 
    relevel(1),
  higher_educ = case_when(NIVELESTENTREV %in% c(1:7, 16) ~ 0L,
                          NIVELESTENTREV %in% 8:15 ~ 1L,
                          TRUE ~ NA_integer_) %>% 
    factor(levels = c(0, 1),
           labels = c("Universitario", "No-Universitario")
           ) %>% 
    relevel(1),
  welloff = case_when(CLASESOCIAL %in% 1:3 ~ 1L, #high, middle-high, middle-middle
                      CLASESOCIAL %in% 4:12 ~ 0L,
                      TRUE ~ NA_integer_) %>% 
    factor(levels = c(0, 1),
           labels = c("Medio-alto", "Bajo")
           ) %>% 
    relevel(1)
  ) %>% 
  select(id, Periodo, dist_eval, eval_pres, evalpres_GMC, eval_opos, ideol_pers,ideolpers_GMC, ideol_2, ideol_3, distideo_GMC, distideo_2, distideo_3, RV, man, higher_educ, welloff, PESO) %>% 
  drop_na() %>% 
  write_rds("df_3271.rds")


# 3. Febrero-------------------------------------------------------------------

df_3273 <- read_rds("backup_3273.rds") %>% 
  mutate(id = as.numeric(
    paste0(as.character(ESTU), 
           as.character(CUES))
  ),
  PESO = 1,
  Periodo = 3,
  RV = case_when(RECUVOTOGR %in% c(5, 6, 21,67) ~ 5, #UP
                 RECUVOTOGR %in% c(7, 50) ~ 7,       #MPais
                 RECUVOTOGR == 1 ~ 1, #PP
                 RECUVOTOGR == 2 ~ 2, #PSOE
                 RECUVOTOGR == 4 ~ 4, #Cs
                 RECUVOTOGR == 18 ~ 18, #VOX
                 RECUVOTOGR >= 96 | RECUVOTOGR == 0 ~ NA_real_, #
                 TRUE ~ 99) %>% 
    factor(levels = c(1, 2, 4, 5, 7, 18, 99),
           labels = c("PP", "PSOE", "Cs", "UP","MP", "VOX", "Otros")
    ) %>% 
    relevel(2),
  eval_pres = case_when(as.numeric(B15_1)  %in% c(97, 98, 99) ~ NA_real_,
                        TRUE ~ as.numeric(B15_1)),
  evalpres_GMC = eval_pres - mean(eval_pres, na.rm = TRUE),
  B15_2 = case_when(as.numeric(B15_2) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(B15_2)),
  B15_3 = case_when(as.numeric(B15_3) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(B15_3)),
  B15_4 = case_when(as.numeric(B15_4) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(B15_4)),
  B15_5 = case_when(as.numeric(B15_5) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(B15_5)),
  B15_6 = case_when(as.numeric(B15_6) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(B15_6)),
  B15_7 = case_when(as.numeric(B15_7) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(B15_7)),
  eval_opos = pmax(B15_2, B15_3, B15_4, B15_5, B15_6, B15_7, na.rm = TRUE),
  ideol_pers = case_when(as.numeric(ESCIDEOL)  %in% c(97, 98, 99) ~ NA_real_,
                         TRUE ~ as.numeric(ESCIDEOL)),
  ideolpers_GMC = ideol_pers - mean(ideol_pers, na.rm = TRUE),
  ideol_pres = case_when(as.numeric(ESCIDEOLPOLI_1)  %in% c(97, 98, 99) ~ NA_real_,
                         TRUE ~ as.numeric(ESCIDEOLPOLI_1)),
  dist_ideo = ideol_pers - ideol_pres,
  distideo_GMC= dist_ideo - mean(dist_ideo, na.rm = TRUE),
  dist_eval = eval_pres - eval_opos,
  ideol_2 = ideolpers_GMC^2,
  ideol_3 = ideolpers_GMC^3,
  distideo_2 = distideo_GMC^2,
  distideo_3 = distideo_GMC^3,
  man = case_when(SEXO == 1 ~ 1L,
                  SEXO == 2 ~ 0L,
                  TRUE ~ NA_integer_) %>% 
    factor(levels = c(0, 1),
           labels = c("Mujer", "Hombre")
    ) %>% 
    relevel(1),
  higher_educ = case_when(NIVELESTENTREV %in% c(1:7, 16) ~ 0L,
                          NIVELESTENTREV %in% 8:15 ~ 1L,
                          TRUE ~ NA_integer_) %>% 
    factor(levels = c(0, 1),
           labels = c("Universitario", "No-Universitario")
    ) %>% 
    relevel(1),
  welloff = case_when(CLASESOCIAL %in% 1:3 ~ 1L, #high, middle-high, middle-middle
                      CLASESOCIAL %in% 4:12 ~ 0L,
                      TRUE ~ NA_integer_) %>% 
    factor(levels = c(0, 1),
           labels = c("Medio-alto", "Bajo")
    ) %>% 
    relevel(1)
  ) %>% 
  select(id, Periodo, dist_eval, eval_pres, evalpres_GMC, eval_opos, ideol_pers,ideolpers_GMC, ideol_2, ideol_3, distideo_GMC, distideo_2, distideo_3, RV, man, higher_educ, welloff, PESO) %>% 
  drop_na() %>%
  write_rds("df_3273.rds")

# 3. Marzo---------------------------------------------------------------------

df_3277 <- read_rds("backup_3277.rds") %>% 
  mutate(id = as.numeric(
    paste0(as.character(ESTU), 
           as.character(CUES))
  ),
  Periodo = 4,
  RV = case_when(RECUVOTOGR %in% c(5, 6, 21,67) ~ 5, #UP
                 RECUVOTOGR %in% c(7, 50) ~ 7,       #MPais
                 RECUVOTOGR == 1 ~ 1, #PP
                 RECUVOTOGR == 2 ~ 2, #PSOE
                 RECUVOTOGR == 4 ~ 4, #Cs
                 RECUVOTOGR == 18 ~ 18, #VOX
                 RECUVOTOGR >= 96 | RECUVOTOGR == 0 ~ NA_real_, #
                 TRUE ~ 99) %>% 
    factor(levels = c(1, 2, 4, 5, 7, 18, 99),
           labels = c("PP", "PSOE", "Cs", "UP", "MP", "VOX", "Otros")
    ) %>% 
    relevel(2),
  eval_pres = case_when(as.numeric(A17_1)  %in% c(97, 98, 99) ~ NA_real_,
                        TRUE ~ as.numeric(A17_1)),
  evalpres_GMC = eval_pres - mean(eval_pres, na.rm = TRUE),
  A17_2 = case_when(as.numeric(A17_2) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(A17_2)),
  A17_3 = case_when(as.numeric(A17_3) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(A17_3)),
  A17_4 = case_when(as.numeric(A17_4) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(A17_4)),
  A17_5 = case_when(as.numeric(A17_5) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(A17_5)),
  A17_6 = case_when(as.numeric(A17_6) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(A17_6)),
  A17_7 = case_when(as.numeric(A17_7) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(A17_7)),
  eval_opos = pmax(A17_2, A17_3, A17_4, A17_5, A17_6, A17_7, na.rm = TRUE),
  ideol_pers = case_when(as.numeric(ESCIDEOL)  %in% c(97, 98, 99) ~ NA_real_,
                         TRUE ~ as.numeric(ESCIDEOL)),
  ideolpers_GMC = ideol_pers - mean(ideol_pers, na.rm = TRUE),
  ideol_pres = case_when(as.numeric(ESCIDEOLPOLI_1)  %in% c(97, 98, 99) ~ NA_real_,
                         TRUE ~ as.numeric(ESCIDEOLPOLI_1)),
  dist_ideo = ideol_pers - ideol_pres,
  distideo_GMC= dist_ideo - mean(dist_ideo, na.rm = TRUE),
  dist_eval = eval_pres - eval_opos,
  ideol_2 = ideolpers_GMC^2,
  ideol_3 = ideolpers_GMC^3,
  distideo_2 = distideo_GMC^2,
  distideo_3 = distideo_GMC^3,
  man = case_when(SEXO == 1 ~ 1L,
                  SEXO == 2 ~ 0L,
                  TRUE ~ NA_integer_) %>% 
    factor(levels = c(0, 1),
           labels = c("Mujer", "Hombre")
    ) %>% 
    relevel(1),
  higher_educ = case_when(NIVELESTENTREV %in% c(1:7, 16) ~ 0L,
                          NIVELESTENTREV %in% 8:15 ~ 1L,
                          TRUE ~ NA_integer_) %>% 
    factor(levels = c(0, 1),
           labels = c("Universitario", "No-Universitario")
    ) %>% 
    relevel(1),
  welloff = case_when(CLASESOCIAL %in% 1:3 ~ 1L, #high, middle-high, middle-middle
                      CLASESOCIAL %in% 4:12 ~ 0L,
                      TRUE ~ NA_integer_) %>% 
    factor(levels = c(0, 1),
           labels = c("Medio-alto", "Bajo")
    ) %>% 
    relevel(1)
  ) %>% 
  select(id, Periodo, dist_eval, eval_pres, evalpres_GMC, eval_opos, ideol_pers,ideolpers_GMC, ideol_2, ideol_3, distideo_GMC, distideo_2, distideo_3, RV, man, higher_educ, welloff, PESO) %>% 
  drop_na() %>%
  write_rds("df_3277.rds")

# 4. Abril---------------------------------------------------------------------

df_3279 <- read_rds("backup_3279.rds") %>% 
  mutate(id = as.numeric(
    paste0(as.character(ESTUDIO), 
           as.character(CUES))
  ),
  PESO = 1,
  Periodo = 5,
  RV = case_when(RECUVOTOGR %in% c(5, 6, 21,67) ~ 5, #UP
                 RECUVOTOGR %in% c(7, 50) ~ 7,       #MPais
                 RECUVOTOGR == 1 ~ 1, #PP
                 RECUVOTOGR == 2 ~ 2, #PSOE
                 RECUVOTOGR == 4 ~ 4, #Cs
                 RECUVOTOGR == 18 ~ 18, #VOX
                 RECUVOTOGR >= 96 | RECUVOTOGR == 0 ~ NA_real_, #
                 TRUE ~ 99) %>% 
    factor(levels = c(1, 2, 4, 5, 7, 18, 99),
           labels = c("PP", "PSOE", "Cs", "UP", "MP", "VOX", "Otros")
    ) %>% 
    relevel(2),
  eval_pres = case_when(as.numeric(P31_1)  %in% c(97, 98, 99) ~ NA_real_,
                        TRUE ~ as.numeric(P31_1)),
  evalpres_GMC = eval_pres - mean(eval_pres, na.rm = TRUE),
  P31_2 = case_when(as.numeric(P31_2) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(P31_2)),
  P31_3 = case_when(as.numeric(P31_3) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(P31_3)),
  P31_4 = case_when(as.numeric(P31_4) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(P31_4)),
  P31_5 = case_when(as.numeric(P31_5) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(P31_5)),
  P31_6 = case_when(as.numeric(P31_6) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(P31_6)),
  eval_opos = pmax(P31_2, P31_3, P31_4, P31_5, P31_6, na.rm = TRUE),
  ideol_pers = case_when(as.numeric(ESCIDEOL)  %in% c(97, 98, 99) ~ NA_real_,
                         TRUE ~ as.numeric(ESCIDEOL)),
  ideolpers_GMC = ideol_pers - mean(ideol_pers, na.rm = TRUE),
  ideol_pres = case_when(as.numeric(ESCIDEOLPOLI_1)  %in% c(97, 98, 99) ~ NA_real_,
                         TRUE ~ as.numeric(ESCIDEOLPOLI_1)),
  dist_ideo = ideol_pers - ideol_pres,
  distideo_GMC= dist_ideo - mean(dist_ideo, na.rm = TRUE),
  dist_eval = eval_pres - eval_opos,
  ideol_2 = ideolpers_GMC^2,
  ideol_3 = ideolpers_GMC^3,
  distideo_2 = distideo_GMC^2,
  distideo_3 = distideo_GMC^3,
  man = case_when(SEXO == 1 ~ 1L,
                  SEXO == 2 ~ 0L,
                  TRUE ~ NA_integer_) %>% 
    factor(levels = c(0, 1),
           labels = c("Mujer", "Hombre")
    ) %>% 
    relevel(1),
  higher_educ = case_when(NIVELESTENTREV %in% c(1:7, 16) ~ 0L,
                          NIVELESTENTREV %in% 8:15 ~ 1L,
                          TRUE ~ NA_integer_) %>% 
    factor(levels = c(0, 1),
           labels = c("Universitario", "No-Universitario")
    ) %>% 
    relevel(1),
  welloff = case_when(CLASESOCIAL %in% 1:3 ~ 1L, #high, middle-high, middle-middle
                      CLASESOCIAL %in% 4:12 ~ 0L,
                      TRUE ~ NA_integer_) %>% 
    factor(levels = c(0, 1),
           labels = c("Medio-alto", "Bajo")
    ) %>% 
    relevel(1)
  ) %>% 
  select(id, Periodo, dist_eval, eval_pres, evalpres_GMC, eval_opos, ideol_pers,ideolpers_GMC, ideol_2, ideol_3, distideo_GMC, distideo_2, distideo_3, RV, man, higher_educ, welloff, PESO) %>% 
  drop_na() %>%
  write_rds("df_3279.rds")
# 5. Mayo----------------------------------------------------------------------

df_3281 <- read_rds("backup_3281.rds") %>% 
  mutate(id = as.numeric(
    paste0(as.character(ESTUDIO), 
           as.character(CUES))
  ),
  PESO = 1,
  Periodo = 6,
  RV = case_when(RECUVOTOGR %in% c(5, 6, 21,67) ~ 5, #UP
                 RECUVOTOGR %in% c(7, 50) ~ 7,       #MPais
                 RECUVOTOGR == 1 ~ 1, #PP
                 RECUVOTOGR == 2 ~ 2, #PSOE
                 RECUVOTOGR == 4 ~ 4, #Cs
                 RECUVOTOGR == 18 ~ 18, #VOX
                 RECUVOTOGR >= 96 | RECUVOTOGR == 0 ~ NA_real_, #
                 TRUE ~ 99) %>% 
    factor(levels = c(1, 2, 4, 5, 7, 18, 99),
           labels = c("PP", "PSOE", "Cs", "UP", "MP", "VOX", "Otros")
    ) %>% 
    relevel(2),
  eval_pres = case_when(as.numeric(P33_1)  %in% c(97, 98, 99) ~ NA_real_,
                        TRUE ~ as.numeric(P33_1)),
  evalpres_GMC = eval_pres - mean(eval_pres, na.rm = TRUE),
  P33_2 = case_when(as.numeric(P33_2) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(P33_2)),
  P33_3 = case_when(as.numeric(P33_3) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(P33_3)),
  P33_4 = case_when(as.numeric(P33_4) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(P33_4)),
  P33_5 = case_when(as.numeric(P33_5) %in% c(97, 98, 99) ~ NA_real_,
                    TRUE ~ as.numeric(P33_5)),
  eval_opos = pmax(P33_2, P33_3, P33_4, P33_5, na.rm = TRUE),
  ideol_pers = case_when(as.numeric(ESCIDEOL)  %in% c(97, 98, 99) ~ NA_real_,
                         TRUE ~ as.numeric(ESCIDEOL)),
  ideolpers_GMC = ideol_pers - mean(ideol_pers, na.rm = TRUE),
  ideol_pres = case_when(as.numeric(ESCIDEOLPOLI_1)  %in% c(97, 98, 99) ~ NA_real_,
                         TRUE ~ as.numeric(ESCIDEOLPOLI_1)),
  dist_ideo = ideol_pers - ideol_pres,
  distideo_GMC= dist_ideo - mean(dist_ideo, na.rm = TRUE),
  dist_eval = eval_pres - eval_opos,
  ideol_2 = ideolpers_GMC^2,
  ideol_3 = ideolpers_GMC^3,
  distideo_2 = distideo_GMC^2,
  distideo_3 = distideo_GMC^3,
  man = case_when(SEXO == 1 ~ 1L,
                  SEXO == 2 ~ 0L,
                  TRUE ~ NA_integer_) %>% 
    factor(levels = c(0, 1),
           labels = c("Mujer", "Hombre")
    ) %>% 
    relevel(1),
  higher_educ = case_when(NIVELESTENTREV %in% c(1:8, 17) ~ 0L,
                          NIVELESTENTREV %in% 9:16 ~ 1L,
                          TRUE ~ NA_integer_) %>% 
    factor(levels = c(0, 1),
           labels = c("Universitario", "No-Universitario")
    ) %>% 
    relevel(1),
  welloff = case_when(CLASESOCIAL %in% 1:3 ~ 1L, #high, middle-high, middle-middle
                      CLASESOCIAL %in% 4:11 ~ 0L,
                      TRUE ~ NA_integer_) %>% 
    factor(levels = c(0, 1),
           labels = c("Medio-alto", "Bajo")
    ) %>% 
    relevel(1)
  ) %>% 
  select(id, Periodo, dist_eval, eval_pres, evalpres_GMC, eval_opos, ideol_pers,ideolpers_GMC, ideol_2, ideol_3, distideo_GMC, distideo_2, distideo_3, RV, man, higher_educ, welloff, PESO) %>% 
  drop_na() %>%
  write_rds("df_3281.rds")
#---------------Eliminamos objetos del Global Environment----------------------------------------

rm(list = ls())
         
