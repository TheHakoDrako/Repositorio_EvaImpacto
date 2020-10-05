#Taller 2 - Evaluación de Impacto. 

#Vamos a cargar la base de datos:
library(readxl)
Taller2 = data.frame(read_excel(file.choose()))

#Visualizamos la base de datos
attach(Taller2)
View(Taller2)

#Separamos las observaciones, según sean tratamientos o controles.
library(dplyr)
Treated = subset(Taller2,treatment==1);Treated
Untreated = subset(Taller2,treatment==0);Untreated

#----------------------------

#Calculamos las medias de las variables de interés para ambos grupos, hacemos un 
#histograma y el test de diferencia de medias. 

#Escuela
mean(Treated$escuela)
mean(Untreated$escuela)
t.test(Treated$escuela,Untreated$escuela,var.equal = TRUE)

dat_ggplot <- data.frame(Escuela = c(Treated$escuela,Untreated$escuela),
Grupo = c(rep("1",108), rep("0", 92)))
ggplot(dat_ggplot) + aes(x = Grupo, y = Escuela) +geom_boxplot() +theme_minimal()

#Tamaño del hogar
mean(Treated$tamano_hogar)
mean(Untreated$tamano_hogar)
sd(Treated$tamano_hogar)
sd(Untreated$tamano_hogar)
t.test(Treated$tamano_hogar,Untreated$tamano_hogar,var.equal = TRUE)

dat_ggplot <- data.frame(Valor = c(Treated$tamano_hogar,Untreated$tamano_hogar),
                         Grupo= c(rep("1",108), rep("0", 92)))
ggplot(dat_ggplot) + aes(x = Grupo, y = Valor) +geom_boxplot() +theme_minimal()

#Residencia
mean(Treated$rural)
mean(Untreated$rural)
sd(Treated$rural)
sd(Untreated$rural)
t.test(Treated$rural,Untreated$rural,var.equal = TRUE)

#Mujer Cabeza de Hogar
mean(Treated$mujer_cabeza)
mean(Untreated$mujer_cabeza)
sd(Treated$mujer_cabeza)
sd(Untreated$mujer_cabeza)
t.test(Treated$mujer_cabeza,Untreated$mujer_cabeza,var.equal = TRUE)

dat_ggplot <- data.frame(Valor = c(Treated$mujer_cabeza,Untreated$mujer_cabeza),
                         Grupo= c(rep("1",108), rep("0", 92)))
ggplot(dat_ggplot) + aes(x = Grupo, y = Valor) +geom_boxplot() +theme_minimal()

View(Treated)
#Ingreso de los hogares
mean(Treated$ingreso_hogar)
mean(Untreated$ingreso_hogar)
sd(Treated$ingreso_hogar)
sd(Untreated$ingreso_hogar)
t.test(Treated$ingreso_hogar,Untreated$ingreso_hogar,var.equal = TRUE)

dat_ggplot <- data.frame(Valor = c(Treated$ingreso_hogar,Untreated$ingreso_hogar),
                         Grupo= c(rep("1",108), rep("0", 92)))
ggplot(dat_ggplot) + aes(x = Grupo, y = Valor) +geom_boxplot() +theme_minimal()

#-----------------------------------

#Calculamos la diferencia de medias y analizamos su significancia estadística
mean(Treated$outcome)-mean(Untreated$outcome)
t.test(Treated$outcome,Untreated$outcome,var.equal = TRUE)

#Hacemos una regresión tipo switching para obtener el mismo resultado
Reg1 = lm(outcome~treatment, data=Taller2)
summary(Reg1)

#Presentamos los resultados con stargazer
library(stargazer)
stargazer(Reg1, type="text",column.labels=c("Regresión 1"), keep.stat = c("n","rsq"))

#Visualizamos gráficamente los resultados
library(ggplot2)

x11()
data <- data.frame(
  x=c("No Tratados","Tratados"),
  y=c(1.9519,6.8257)
)

# Plot
ggplot(data, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y), color="grey") +
  geom_point( color="orange", size=5) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("Grupo.") +
  ylab("Valor del Índice.")+
  ggtitle("Estado de Salud Promedio.")


#---------------------------------

#Hacemos la misma regresión incluyendo algunos controles con objeto de incrementar la precisión de las estimaciones. 
Reg2= lm(outcome~treatment + ingreso_hogar + tamano_hogar + rural, data=Taller2)
summary(Reg2)

#Controlando por la escuela a la que va cada alumno (dejando la 1 como base)
Reg3= lm(outcome~treatment + ingreso_hogar + tamano_hogar + rural + as.factor(escuela), data=Taller2)
summary(Reg3)

#Presentamos los resultados
stargazer(Reg1,Reg2,Reg3, type="text",column.labels=c("Regresión 1","Regresión 2",
        "Regresión 3"), omit = c("escuela"), keep.stat = c("n","rsq","ser"))


#Vemos gráficamente el coeficiente de interés
install.packages("jtools") 
library(jtools)
install.packages("broom")
library(broom)
install.packages("ggstance")
library(ggstance)
install.packages("broom.mixed")
library(broom.mixed)

x11()
plot_summs(summ(Reg1), summ(Reg2), summ(Reg3), scale = TRUE, omit.coefs = c("(Intercept)",
"ingreso_hogar","tamano_hogar","rural","as.factor(escuela)2","as.factor(escuela)3",
"as.factor(escuela)4","as.factor(escuela)5","as.factor(escuela)6","as.factor(escuela)7",
"as.factor(escuela)8","as.factor(escuela)9","as.factor(escuela)10"),model.names = c("Modelo 1",
"Modelo 2", "Modelo 3"), legend.title = "Título", xlim=c(0,6))

#-----------------------------------

#Vamos a obtener la submuestra necesaria para estimar nuevamente el modelo

#Sutva

esc1= subset(Taller2,escuela==1);esc1
esc2= subset(Taller2,escuela==2);esc2
esc3= subset(Taller2,escuela==3);esc3
esc4= subset(Taller2,escuela==4);esc4
esc5= subset(Taller2,escuela==5);esc5
esc6= subset(Taller2,escuela==6);esc6 #D_i=0
esc7= subset(Taller2,escuela==7);esc7 #D_i=1
esc8= subset(Taller2,escuela==8);esc8 #D_i=0
esc9= subset(Taller2,escuela==9);esc9 #D_i=1
esc10= subset(Taller2,escuela==10);esc10 #D_i=0

#Definición de la submuestra de los tratados

Sub.Sample = subset(Taller2, escuela>=6)
Sub.Sample

Sub.Reg1 = lm(outcome~treatment, data=Sub.Sample)
summary(Sub.Reg1)

Sub.Reg2= lm(outcome~treatment + ingreso_hogar + tamano_hogar + rural, data=Sub.Sample)
summary(Sub.Reg2)


#----------------------------------

#Analizamos el impacto del tratamiento sobre los resultados escolares

#Sin variables control
Reg.Score.1 = lm(nota_hijos~treatment, data=Taller2)
summary(Reg.Score.1)
#Con variables control
Reg.Score.2= lm(nota_hijos~treatment + ingreso_hogar + rural, data=Taller2)
summary(Reg.Score.2)
#Controlando también por cada una de las escuelas (dejando la 1 como base)
Reg.Score.3= lm(nota_hijos~treatment + ingreso_hogar  + rural + as.factor(escuela), data=Taller2)
summary(Reg.Score.1)
#Hacemos la regresión 1 y 2 con la submuestra
#Sin variables control
Sub.Reg.Score.1 = lm(nota_hijos~treatment, data=Sub.Sample)
summary(Sub.Reg.Score.1)
#Con variables control
Sub.Reg.Score.2= lm(nota_hijos~treatment + ingreso_hogar + rural, data=Sub.Sample)
summary(Sub.Reg.Score.2)

#Presentamos los resultados
stargazer(Reg.Score.1,Reg.Score.2,Reg.Score.3,Sub.Reg.Score.1,Sub.Reg.Score.2,type="text",column.labels=c("Regresión 1","Regresión 2",
         "Regresión 3","Reg1- Sub","Reg2-Sub"), omit = c("escuela"), keep.stat = c("n","rsq","ser"))

#Vemos gráficamente los resultados
x11()
plot_summs(summ(Reg.Score.1), summ(Reg.Score.2), summ(Reg.Score.3), scale = TRUE, omit.coefs = c("(Intercept)",
"ingreso_hogar","tamano_hogar","rural","as.factor(escuela)2","as.factor(escuela)3",
"as.factor(escuela)4","as.factor(escuela)5","as.factor(escuela)6","as.factor(escuela)7",
"as.factor(escuela)8","as.factor(escuela)9","as.factor(escuela)10"),model.names = c("Modelo 1",
 "Modelo 2", "Modelo 3"), legend.title = "Título", xlim=c(0,6))

#----------------------------------------------


