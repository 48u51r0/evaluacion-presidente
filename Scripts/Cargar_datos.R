
# loading library of functions 
source("Scripts/libs.R")

# Install all the necessary packages
packages <- (c("weights","haven", "sandwich", "multcomp", "emmeans", 
               "tidyverse", "conflicted", "ggeffects", "splines",
               "robustbase", "stargazer", "vistime",
               "ggrepel", "scales", "lmtest", "dotwhisker"))
install_pack(packages)

# Solve conflicts between packages 
masked_functions("stats")

# Load data sets the name of which starts with "df_"
load_data("df_")

#definimos las variables a utilizar
vars <- rlang::quo(c(id, Periodo, 
                     eval_pres, 
                     ideol_pers, ideol_GMC, ideol_2, ideol_3,
                     RV, partidismo_1, partidismo_2,
                     man, higher_educ, welloff,
                     PESO)
)

# recogemos los nombres de nuestros data frame nombrados según patron df_
dfs <- ls(pattern = "df_")

# para cada nombre lo asignamos a su objeto y seleccionamos las variables
for (i in seq_along(dfs)) {
  assign(dfs[i],
         # get toma un string nombre de un objeto y lo transforma en dicho objeto
         value = get(dfs[i]) %>% 
           # selecciona las vars
           select(!!vars) %>%  
           # elimina las columnas que solo contiene NA
           select_if(~!all(is.na(.))) %>%
           # elimina las filas con algún NA
           drop_na()
         )
  
}

# Clean auxiliary functions(lsf.str) and objects
rm(list = c("packages","vars", "dfs", "i"))

