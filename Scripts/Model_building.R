#-----------------Start up------------------------------------------
#3269:Postelectoral
#3271:Enero
#3273:Febrero
#3277:Marzo
#3279:Abril
#3281:Mayo

#Loading all the necessary packages and datasets
source("Scripts/Cargar_datos.R")

#-----------------Visualising trends-----------------------------------------
ggplot(data = df_3273, 
       mapping = aes(x= ideol_pers, 
                     y = eval_pres, 
                     col= RV))+
  geom_jitter(width = 0.2, height = 0.2)+
  geom_smooth(method = "lm", se = FALSE, mapping = 
                aes(weight = PESO),
              formula = y~poly(x, 2), col= "black")+
  geom_smooth(method = "lm", se = FALSE, mapping = 
                aes(weight = PESO),
              formula = y~poly(x, 3), col= "blue")

ggplot(data = df_3269, 
       mapping = aes(x= distideo_GMC, 
                     y = eval_pres, 
                     col= RV))+
  geom_jitter(width = 0.2, height = 0.2)+
  geom_smooth(method = "lm", se = FALSE, mapping = 
                aes(weight = PESO),
              formula = y~poly(x, 2), col= "black")+
  geom_smooth(method = "lm", se = FALSE, mapping = 
                aes(weight = PESO),
              formula = y~poly(x, 3), col= "blue")
#Alternativas de modelo 
a<-lmrob(eval_pres~ideol_GMC+man+higher_educ+welloff,
      data= df_3269,
      weights = PESO)
b<-lmrob(eval_pres~RV+man+higher_educ+welloff,
      data= df_3269,
      weights = PESO)
c<-lmrob(eval_pres~ideol_GMC*RV+man+higher_educ+welloff,
      data= df_3269,
      weights = PESO)
#podemos hacer LRT para probar la conveniencia de añadir variables
stargazer(a, b, c, type = "text")
