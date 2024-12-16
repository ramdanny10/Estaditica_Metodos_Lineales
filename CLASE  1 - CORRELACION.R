attach(EJEMPLO_1)
attach(EJEMPLO_2)
attach(EJEMPLO_3)
attach(EJEMPLO_4)


## EJEMPLO1##
attach(EJEMPLO_1)
names(EJEMPLO_1)
summary(EJEMPLO_1)
plot(`Peso al nacer`~`semanas de gestacion`, col="red", pch=20, main="Diagrama Dispersion")
abline (h=3222, v=36.3, col="blue", lty=2)
# Covarianza
n <- nrow(EJEMPLO_1)
n
numerador <- sum((EJEMPLO_1$`semanas de gestacion` - mean(EJEMPLO_1$`semanas de gestacion`) ) * (EJEMPLO_1$`Peso al nacer` - mean(EJEMPLO_1$`Peso al nacer`)))
numerador
# numerador ; sum(tabla$prod)
denominador <- n - 1
denominador
covarianza <- numerador / denominador
covarianza


### PARTE 2 DEL EJEMPLO 1 ###

attach(EJEMPLO_1)
names(EJEMPLO_1)
## Reconocimineto de las variables (buscar normalidad)
#Histograma
tabla3<-hist(`semanas de gestacion`,plot=FALSE)
plot(tabla3,main="Histograma - Semanas de Gestacion ",ylab="Frecuencia",col="orange", ylim=c(0,20))

tabla4<-hist(`Peso al nacer`,plot=FALSE)
plot(tabla4,main="Histograma - Peso al nacer ",ylab="Frecuencia",col="orange")

#Diagrama de cajas y bigotes
boxplot(`Peso al nacer` ,col="yellow",main="Diagrama de cajas y bigotes \n variable Peso al nacer")

boxplot(`semanas de gestacion`, col="yellow",main="Diagrama de cajas y bigotes \n variable Semanas de gestacion")

#### Correlacion

pairs( `Peso al nacer`~`semanas de gestacion` )  #permite elaborar un plot de correlacion

library(PerformanceAnalytics)
# Nuestros datos es mejor tenerlos en un data.frame
dat1 <- data.frame(`Peso al nacer` , `semanas de gestacion`)
chart.Correlation(dat1)


cor.test(`Peso al nacer` , `semanas de gestacion`)  # cor.test analiza la significancia de la correlacion, utilizando el contraste t-student.

##### Normalidad
#Hipotesis estad?stica

#Ho: los datos siguen una distribucion normal

#H1: los datos no siguen una distribucion normal
shapiro.test(`Peso al nacer`)
shapiro.test(`semanas de gestacion`)


### Graficos
library(car)
qqPlot(`Peso al nacer`)
qqPlot(`semanas de gestacion`)

## Coeficiente de Speraman

cor(`Peso al nacer` , `semanas de gestacion`, method = "spearman")  # cor.test analiza la significancia de la correlacion, utilizando el contraste t-student.






## EJEMPLO2##
attach(EJEMPLO_2)
names(EJEMPLO_2)
summary(EJEMPLO_2)
plot(`variable x`~`variable y`, col="red", pch=20, main="Diagrama Dispersi?n")
abline (h=-13.74, v=28.4, col="blue", lty=2)
# Covarianza
n <- nrow(EJEMPLO_2)
n
numerador <- sum((EJEMPLO_2$`variable y` - mean(EJEMPLO_2$`variable y`) ) * (EJEMPLO_2$`variable x` - mean(EJEMPLO_2$`variable x`)))
numerador
# numerador ; sum(tabla$prod)
denominador <- n - 1
denominador
covarianza <- numerador / denominador
covarianza




## Reconocimineto de las variables (buscar normalidad)

attach(EJEMPLO_3)
names(EJEMPLO_3)
summary(EJEMPLO_3)

#### Correlaci?n

pairs( `Turbiedad`~`Color_Aparente` )  #permite elaborar un plot de correlacion


# Nuestros datos es mejor tenerlos en un data.frame
dat1 <- data.frame(`Turbiedad` , `Color_Aparente`)
chart.Correlation(dat1)


cor.test(`Turbiedad` , `Color_Aparente`, method = "spearman")  # cor.test analiza la significancia de la correlacion, utilizando el contraste t-student.

##### Test de Normalidad
#Hipotesis estad?stica

#Ho: los datos siguen una distribuci?n normal

#H1: los datos no siguen una distribuci?n normal
shapiro.test(`Turbiedad`)
shapiro.test(`Color_Aparente`)


### Gr?ficos

qqPlot(`Turbiedad`)
qqPlot(`Color_Aparente`)

## Coeficiente de Speraman

cor(`Turbiedad` , `Color_Aparente`, method = "pearson")  # cor.test analiza la significancia de la correlacion, utilizando el contraste t-student.

cor(`Turbiedad` , `Color_Aparente`, method = "spearman")  # cor.test analiza la significancia de la correlacion, utilizando el contraste t-student.















## EJEMPLO3##
attach(EJEMPLO_3)
names(EJEMPLO_3)
summary(EJEMPLO_3)
plot(`Turbiedad`~`Color_Aparente`, col="red", pch=20, main="Diagrama Dispersi?n")
abline (h=1.002, v=5.546, col="blue", lty=2)
# Covarianza
n <- nrow(EJEMPLO_3)
n
numerador <- sum((EJEMPLO_3$`Color_Aparente` - mean(EJEMPLO_3$`Color_Aparente`) ) * (EJEMPLO_3$`Turbiedad` - mean(EJEMPLO_3$`Turbiedad`)))
numerador
# numerador ; sum(tabla$prod)
denominador <- n - 1
denominador
covarianza <- numerador / denominador
covarianza

## EJEMPLO4##
attach(EJEMPLO_4xlsx)
names(EJEMPLO_4xlsx)
summary(EJEMPLO_4xlsx)
plot(`Analfabetismo`~`Ingreso`, col="red", pch=20, main="Diagrama Dispersi?n")
abline (h=1.17, v=2.688, col="blue", lty=2)
# Covarianza
n <- nrow(EJEMPLO_4xlsx)
n
numerador <- sum((EJEMPLO_4xlsx$`Ingreso` - mean(EJEMPLO_4xlsx$`Ingreso`) ) * (EJEMPLO_4xlsx$`Analfabetismo` - mean(EJEMPLO_4xlsx$`Analfabetismo`)))
numerador
# numerador ; sum(tabla$prod)
denominador <- n - 1
denominador
covarianza <- numerador / denominador
covarianza



## Reconocimineto de las variables (buscar normalidad)

attach(EJEMPLO_4)
names(EJEMPLO_4)
summary(EJEMPLO_4)

#### Correlaci?n

pairs( `Turbiedad`~`Color_Aparente` )  #permite elaborar un plot de correlacion


# Nuestros datos es mejor tenerlos en un data.frame
dat1 <- data.frame(`Turbiedad` , `Color_Aparente`)
chart.Correlation(dat1)


cor.test(`Turbiedad` , `Color_Aparente`, method = "spearman")  # cor.test analiza la significancia de la correlacion, utilizando el contraste t-student.

##### Test de Normalidad
#Hipotesis estad?stica

#Ho: los datos siguen una distribuci?n normal

#H1: los datos no siguen una distribuci?n normal
shapiro.test(`Turbiedad`)
shapiro.test(`Color_Aparente`)


### Gr?ficos

qqPlot(`Turbiedad`)
qqPlot(`Color_Aparente`)

## Coeficiente de Speraman

cor(`Turbiedad` , `Color_Aparente`, method = "pearson")  # cor.test analiza la significancia de la correlacion, utilizando el contraste t-student.

cor(`Turbiedad` , `Color_Aparente`, method = "spearman")  # cor.test analiza la significancia de la correlacion, utilizando el contraste t-student.










#Trabajo en clase - Base de datos ICFES - AMB2021
#ESTU_INSE_INDIVIDUAL vs  PUNT_GLOBAL  
attach(AMB_2021_depurada)
names(AMB_2021_depurada)
summary(ESTU_INSE_INDIVIDUAL)
summary(PUNT_GLOBAL)
plot(`ESTU_INSE_INDIVIDUAL`,`PUNT_GLOBAL`, col="red", pch=20, main="Diagrama Dispersi?n")
abline (h=271.2, v=53.85, col="blue", lty=2)
# Covarianza
n <- nrow(AMB_2021_depurada)
n
numerador <- sum((AMB_2021_depurada$`ESTU_INSE_INDIVIDUAL` - mean(AMB_2021_depurada$`ESTU_INSE_INDIVIDUAL`) ) * (AMB_2021_depurada$`PUNT_GLOBAL` - mean(AMB_2021_depurada$`PUNT_GLOBAL`)))
numerador
# numerador ; sum(tabla$prod)
denominador <- n - 1
denominador
covarianza <- numerador / denominador
covarianza

#ESTU_INSE_INDIVIDUAL vs  PUNT_INGLES  
attach(AMB_2021_depurada)
names(AMB_2021_depurada)
summary(ESTU_INSE_INDIVIDUAL)
summary(PUNT_INGLES)
plot(`ESTU_INSE_INDIVIDUAL`,`PUNT_INGLES`, col="blue", pch=20, main="Diagrama Dispersi?n")
abline (h=54.75, v=53.85, col="red", lty=2)
# Covarianza
n <- nrow(AMB_2021_depurada)
n
numerador <- sum((AMB_2021_depurada$`ESTU_INSE_INDIVIDUAL` - mean(AMB_2021_depurada$`ESTU_INSE_INDIVIDUAL`) ) * (AMB_2021_depurada$`PUNT_INGLES` - mean(AMB_2021_depurada$`PUNT_INGLES`)))
numerador
# numerador ; sum(tabla$prod)
denominador <- n - 1
denominador
covarianza <- numerador / denominador
covarianza

#ESTU_INSE_INDIVIDUAL vs  PUNT_C_NATURALES
attach(AMB_2021_depurada)
names(AMB_2021_depurada)
summary(ESTU_INSE_INDIVIDUAL)
summary(PUNT_C_NATURALES)
plot(`ESTU_INSE_INDIVIDUAL`,`PUNT_C_NATURALES`, col="orange", pch=20, main="Diagrama Dispersi?n")
abline (h=54.75, v=53.85, col="black", lty=2)
# Covarianza
n <- nrow(AMB_2021_depurada)
n
numerador <- sum((AMB_2021_depurada$`ESTU_INSE_INDIVIDUAL` - mean(AMB_2021_depurada$`ESTU_INSE_INDIVIDUAL`) ) * (AMB_2021_depurada$`PUNT_C_NATURALES` - mean(AMB_2021_depurada$`PUNT_C_NATURALES`)))
numerador
# numerador ; sum(tabla$prod)
denominador <- n - 1
denominador
covarianza <- numerador / denominador
covarianza

#ESTU_INSE_INDIVIDUAL vs  PUNT_MATEMATICAS
attach(AMB_2021_depurada)
names(AMB_2021_depurada)
summary(ESTU_INSE_INDIVIDUAL)
summary(PUNT_MATEMATICAS)
plot(`ESTU_INSE_INDIVIDUAL`,`PUNT_MATEMATICAS`, col="green", pch=20, main="Diagrama Dispersi?n")
abline (h=54.75, v=53.85, col="black", lty=2)
# Covarianza
n <- nrow(AMB_2021_depurada)
n
numerador <- sum((AMB_2021_depurada$`ESTU_INSE_INDIVIDUAL` - mean(AMB_2021_depurada$`ESTU_INSE_INDIVIDUAL`) ) * (AMB_2021_depurada$`PUNT_MATEMATICAS` - mean(AMB_2021_depurada$`PUNT_MATEMATICAS`)))
numerador
# numerador ; sum(tabla$prod)
denominador <- n - 1
denominador
covarianza <- numerador / denominador
covarianza



#### Codigo Mejorado ###

library(tidyverse)
glimp
EJEMPLO_4 <- read_excel("E:/METODOS LINEALES/CLASE 1-A/BASES DE DATOS/EJEMPLO_4xlsx.xlsx")
