rm(llist = ls())
# loading library of functions 
source("Scripts/libs.R")

# Specify and install all the necessary packages
packages <- (c("weights","haven", "sandwich", "multcomp", "emmeans", 
               "tidyverse", "conflicted", "ggeffects", "splines",
               "robustbase", "stargazer"))
install_pack(packages)

# Solve conflicts between packages 
masked_functions()

# Load data sets the name of which starts with "df_"
load_data("df_")

# Clean auxiliary functions(lsf.str) and objects
rm(list = c(lsf.str(), "packages"))
