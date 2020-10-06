#-----------------Loading datasets------------------------------------------

df_3269 <- read_rds("df_3269.rds")
df_3271 <- read_rds("df_3271.rds")
df_3273 <- read_rds("df_3273.rds")
df_3277 <- read_rds("df_3277.rds")
df_3281 <- read_rds("df_3279.rds")


#-----------------Visualising trends-----------------------------------------
ggplot(data = df_3273, 
       mapping = aes(x= ideol, 
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
a<-lm(eval_pres~distideo_GMC+distideo_2+man+higher_educ+welloff,
      data= df_3269,
      weights = PESO)
b<-lm(eval_pres~distideo_GMC+distideo_2+RV+man+higher_educ+welloff,
      data= df_3269,
      weights = PESO)
c<-lm(eval_pres~distideo_GMC*RV+distideo_2*RV+RV+man+higher_educ+welloff,
      data= df_3269,
      weights = PESO)
#podemos hacer LRT para probar la conveniencia de añadir variables
stargazer(a, b, c, type = "text")