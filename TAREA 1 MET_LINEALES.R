
### EJERCICIO 1 - TAREA 1####
attach(AMB_2021)
names(AMB_2021)

### ESTU_INSE_INDIVIDUAL vs PUNT_GLOBAL ###
summary(ESTU_INSE_INDIVIDUAL)
summary(PUNT_GLOBAL)
plot(`ESTU_INSE_INDIVIDUAL`,`PUNT_GLOBAL`, col="red", pch=20, main="Diagrama Dispersion")
abline (h=271.2, v=53.85, col="blue", lty=2)

###COVARIANZA ###
n <- nrow(AMB_2021)
n
numerador <- sum((AMB_2021$`ESTU_INSE_INDIVIDUAL` - mean(AMB_2021$`ESTU_INSE_INDIVIDUAL`) ) * (AMB_2021$`PUNT_GLOBAL` - mean(AMB_2021$`PUNT_GLOBAL`)))
numerador
# numerador ; sum(tabla$prod)
denominador <- n - 1
denominador
covarianza <- numerador / denominador
covarianza

## Validadar Normalidad

### graficamente ##

## Variable  1 ##
par (mfrow=c(1,3))

boxplot(ESTU_INSE_INDIVIDUAL)

hist(ESTU_INSE_INDIVIDUAL, prob=TRUE, main= "Histograma con curva normal", ylab="Densidad")
x<-seq(min(ESTU_INSE_INDIVIDUAL),max(ESTU_INSE_INDIVIDUAL),length=40)
f<-dnorm(x,mean=mean(ESTU_INSE_INDIVIDUAL),sd=sd(ESTU_INSE_INDIVIDUAL))
lines(x,f,col="red",lwd=2)
qqnorm(ESTU_INSE_INDIVIDUAL, pch = 1, frame = FALSE) 
qqline(ESTU_INSE_INDIVIDUAL, col = "steelblue", lwd = 2)

par (mfrow=c(1,1))

## Variable  2 ##
par (mfrow=c(1,3))

boxplot(PUNT_GLOBAL)

hist(PUNT_GLOBAL, prob=TRUE, main= "Histograma con curva normal", ylab="Densidad")
x<-seq(min(PUNT_GLOBAL),max(PUNT_GLOBAL),length=40)
f<-dnorm(x,mean=mean(PUNT_GLOBAL),sd=sd(PUNT_GLOBAL))
lines(x,f,col="red",lwd=2)
qqnorm(PUNT_GLOBAL, pch = 1, frame = FALSE) 
qqline(PUNT_GLOBAL, col = "steelblue", lwd = 2)

par (mfrow=c(1,1))

### Val. Norm NUmericamente ##

## Variable  1 - 2##
library(nortest)
ks.test(ESTU_INSE_INDIVIDUAL, "pnorm", mean=mean(ESTU_INSE_INDIVIDUAL), sd = sd(ESTU_INSE_INDIVIDUAL))
ks.test(PUNT_GLOBAL, "pnorm", mean=mean(PUNT_GLOBAL), sd = sd(PUNT_GLOBAL))

### CORRELACION ###
### PLOT DE CORRELACION##


# Nuestros datos es mejor tenerlos en un data.frame
library(PerformanceAnalytics)
dat1 <- data.frame(`ESTU_INSE_INDIVIDUAL` , `PUNT_GLOBAL`)

## Analisis Grafico - Correlacion ##
pairs( `ESTU_INSE_INDIVIDUAL`~`PUNT_GLOBAL` )
chart.Correlation(dat1, method="spearman")
## Analisis analitico - Correlacion ##
cor.test(`ESTU_INSE_INDIVIDUAL` , `PUNT_GLOBAL`, method = "spearman") 







### EJERCICIO 2 - TAREA 1####
attach(AMB_2021)
names(AMB_2021)

### ESTU_INSE_INDIVIDUAL vs PUNT_GLOBAL ###
summary(PUNT_INGLES)
plot(`ESTU_INSE_INDIVIDUAL`,`PUNT_INGLES`, col="yellow", pch=20, main="Diagrama Dispersion")
abline (h=54.75, v=53.85, col="blue", lty=2)

###COVARIANZA ###
n <- nrow(AMB_2021)
n
numerador <- sum((AMB_2021$`ESTU_INSE_INDIVIDUAL` - mean(AMB_2021$`ESTU_INSE_INDIVIDUAL`) ) * (AMB_2021$`PUNT_INGLES` - mean(AMB_2021$`PUNT_INGLES`)))
numerador
# numerador ; sum(tabla$prod)
denominador <- n - 1
denominador
covarianza <- numerador / denominador
covarianza

## Validadar Normalidad

### graficamente ##


## Variable  3 ##
par (mfrow=c(1,3))

boxplot(PUNT_GLOBAL)

hist(PUNT_INGLES, prob=TRUE, main= "Histograma con curva normal", ylab="Densidad")
x<-seq(min(PUNT_INGLES),max(PUNT_INGLES),length=40)
f<-dnorm(x,mean=mean(PUNT_INGLES),sd=sd(PUNT_INGLES))
lines(x,f,col="red",lwd=2)
qqnorm(PUNT_INGLES, pch = 1, frame = FALSE) 
qqline(PUNT_INGLES, col = "steelblue", lwd = 2)

par (mfrow=c(1,1))

### Val. Norm NUmericamente ##

## Variable  3##
library(nortest)
ks.test(PUNT_INGLES, "pnorm", mean=mean(PUNT_INGLES), sd = sd(PUNT_INGLES))

### CORRELACION ###
### PLOT DE CORRELACION##


# Nuestros datos es mejor tenerlos en un data.frame
library(PerformanceAnalytics)
dat1 <- data.frame(`ESTU_INSE_INDIVIDUAL` , `PUNT_INGLES`)

## Analisis Grafico - Correlacion ##
pairs( `ESTU_INSE_INDIVIDUAL`~`PUNT_INGLES` )
chart.Correlation(dat1, method="spearman")
## Analisis analitico - Correlacion ##
cor.test(`ESTU_INSE_INDIVIDUAL` , `PUNT_INGLES`, method = "spearman") 






### EJERCICIO 3 - TAREA 1####
attach(AMB_2021)
names(AMB_2021)

### ESTU_INSE_INDIVIDUAL vs PUNT_C_NATURALES ###
summary(PUNT_C_NATURALES)
plot(`ESTU_INSE_INDIVIDUAL`,`PUNT_C_NATURALES`, col="orange", pch=20, main="Diagrama Dispersion")
abline (h=52.94, v=53.85, col="blue", lty=2)

###COVARIANZA ###
n <- nrow(AMB_2021)
n
numerador <- sum((AMB_2021$`ESTU_INSE_INDIVIDUAL` - mean(AMB_2021$`ESTU_INSE_INDIVIDUAL`) ) * (AMB_2021$`PUNT_C_NATURALES` - mean(AMB_2021$`PUNT_C_NATURALES`)))
numerador
# numerador ; sum(tabla$prod)
denominador <- n - 1
denominador
covarianza <- numerador / denominador
covarianza

## Validadar Normalidad

### graficamente ##


## Variable  4 ##
par (mfrow=c(1,3))

boxplot(PUNT_C_NATURALES)

hist(PUNT_C_NATURALES, prob=TRUE, main= "Histograma con curva normal", ylab="Densidad")
x<-seq(min(PUNT_C_NATURALES),max(PUNT_C_NATURALES),length=40)
f<-dnorm(x,mean=mean(PUNT_C_NATURALES),sd=sd(PUNT_C_NATURALES))
lines(x,f,col="red",lwd=2)
qqnorm(PUNT_C_NATURALES, pch = 1, frame = FALSE) 
qqline(PUNT_C_NATURALES, col = "steelblue", lwd = 2)

par (mfrow=c(1,1))

### Val. Norm NUmericamente ##

## Variable  4##
library(nortest)
ks.test(PUNT_C_NATURALES, "pnorm", mean=mean(PUNT_C_NATURALES), sd = sd(PUNT_C_NATURALES))

### CORRELACION ###
### PLOT DE CORRELACION##


# Nuestros datos es mejor tenerlos en un data.frame
library(PerformanceAnalytics)
dat1 <- data.frame(`ESTU_INSE_INDIVIDUAL` , `PUNT_C_NATURALES`)

## Analisis Grafico - Correlacion ##
pairs( `ESTU_INSE_INDIVIDUAL`~`PUNT_C_NATURALES` )
chart.Correlation(dat1, method="spearman")
## Analisis analitico - Correlacion ##
cor.test(`ESTU_INSE_INDIVIDUAL` , `PUNT_C_NATURALES`, method = "spearman") 






### EJERCICIO 4 - TAREA 1####
attach(AMB_2021)
names(AMB_2021)

### ESTU_INSE_INDIVIDUAL vs PUNT_MATEMATICAS ###
summary(PUNT_MATEMATICAS)
plot(`ESTU_INSE_INDIVIDUAL`,`PUNT_MATEMATICAS`, col="green", pch=20, main="Diagrama Dispersion")
abline (h=55.21, v=53.85, col="black", lty=2)

###COVARIANZA ###
n <- nrow(AMB_2021)
n
numerador <- sum((AMB_2021$`ESTU_INSE_INDIVIDUAL` - mean(AMB_2021$`ESTU_INSE_INDIVIDUAL`) ) * (AMB_2021$`PUNT_MATEMATICAS` - mean(AMB_2021$`PUNT_MATEMATICAS`)))
numerador
# numerador ; sum(tabla$prod)
denominador <- n - 1
denominador
covarianza <- numerador / denominador
covarianza

## Validadar Normalidad

### graficamente ##


## Variable  5 ##
par (mfrow=c(1,3))

boxplot(PUNT_MATEMATICAS)

hist(PUNT_MATEMATICAS, prob=TRUE, main= "Histograma con curva normal", ylab="Densidad")
x<-seq(min(PUNT_MATEMATICAS),max(PUNT_MATEMATICAS),length=40)
f<-dnorm(x,mean=mean(PUNT_MATEMATICAS),sd=sd(PUNT_MATEMATICAS))
lines(x,f,col="red",lwd=2)
qqnorm(PUNT_MATEMATICAS, pch = 1, frame = FALSE) 
qqline(PUNT_MATEMATICAS, col = "steelblue", lwd = 2)

par (mfrow=c(1,1))

### Val. Norm NUmericamente ##

## Variable  5##
library(nortest)
ks.test(PUNT_MATEMATICAS, "pnorm", mean=mean(PUNT_MATEMATICAS), sd = sd(PUNT_MATEMATICAS))

### CORRELACION ###
### PLOT DE CORRELACION##


# Nuestros datos es mejor tenerlos en un data.frame
library(PerformanceAnalytics)
dat1 <- data.frame(`ESTU_INSE_INDIVIDUAL` , `PUNT_MATEMATICAS`)

## Analisis Grafico - Correlacion ##
pairs( `ESTU_INSE_INDIVIDUAL`~`PUNT_MATEMATICAS` )
chart.Correlation(dat1, method="spearman")
## Analisis analitico - Correlacion ##
cor.test(`ESTU_INSE_INDIVIDUAL` , `PUNT_MATEMATICAS`, method = "spearman") 




