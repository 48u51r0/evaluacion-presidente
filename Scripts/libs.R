#--------------------Install and load the necessary packages-------------------
install_pack <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, library, character.only = TRUE)
}
#--------------------Resolve conflicts between packages------------------------
#as an argument we can pass onto it all the packages names with conflicts
#for instance: masked_functions("ggplot2", "car")
masked_functions <- function(...){
  aux <- conflict_scout(c("dplyr", "plyr", "MASS", ...))
  
  for (i in 1:length(aux)){
    conflict_prefer(names(aux)[i], "dplyr")
  }
}
#--------------------Load all the datasets generated---------------------------

load_data <- function(start_w){
  # Quitamos la extensión de los nombres de archivos que buscamos
  aux <- str_remove(Sys.glob(paste0(start_w, "*")), ".rds")
  for (i in seq_along(aux)){
    # asignamos los archivos a los nombres de objetos anteriores
    assign(aux[i], 
           read_rds(Sys.glob('df_*')[i]),
           envir = .GlobalEnv)
  }
}
# Get Sandwich variance-covariance estimation----------------------------------
varcov_sandwich <- function(model, sandwich_type = "HC0"){
  aux <- bptest(model)$p.value
  if (aux <= .05) 
    coeftest(model,
             vcov. = (vcovHC(model,
                             type = sandwich_type)
                      )
    )
  else 
    cat("The model is Homoscedatic")
}