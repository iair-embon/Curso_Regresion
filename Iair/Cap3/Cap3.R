#######################
#### cod. capitulo ####
#######################

# lee el .dta de chicos con iq. Del capitulo 3

library(tidyverse)
library(haven)

root <- rprojroot::is_rstudio_project
basename(getwd())
#read each line and convert

filepath = root$find_file("Iair/kidiq.dta")

ffc.stata <- read_dta(file = filepath)

fit.2 <- lm (ffc.stata$kid_score ~ ffc.stata$mom_iq)
plot (ffc.stata$mom_iq, ffc.stata$kid_score, xlab="Mother IQ score", ylab="Child test score")
curve (coef(fit.2)[1] + coef(fit.2)[2]*x, add=TRUE)

summary(fit.2)

## 

# segundo grafico

fit.3 <- lm (ffc.stata$kid_score ~ ffc.stata$mom_hs + ffc.stata$mom_iq) 
colors <- ifelse (ffc.stata$mom_hs==1, "black", "gray")
plot (ffc.stata$mom_iq, ffc.stata$kid_score, xlab="Mother IQ score", ylab="Child test score",
      col=colors, pch=20)
curve (cbind (1, 1, x) %*% coef(fit.3), add=TRUE, col="black")
curve (cbind (1, 0, x) %*% coef(fit.3), add=TRUE, col="gray")

# tercer grafico

fit.4 <- lm (ffc.stata$kid_score ~ ffc.stata$mom_hs + ffc.stata$mom_iq + ffc.stata$mom_hs:ffc.stata$mom_iq) 
colors <- ifelse (ffc.stata$mom_hs==1, "black", "gray")
plot (ffc.stata$mom_iq, ffc.stata$kid_score, xlab="Mother IQ score", ylab="Child test score",
      col=colors, pch=20)
curve (cbind (1, 1, x, 1*x) %*% coef(fit.4), add=TRUE, col="black")
curve (cbind (1, 0, x, 0*x) %*% coef(fit.4), add=TRUE, col="gray")


# cuarto: simula la incerteza 10 veces # me tira error en sim

fit.2.sim <- sim (fit.2)
plot (ffc.stata$mom_iq, ffc.stata$kid_score, xlab="Mother IQ score", ylab="Child test score")
for (i in 1:10){
  curve (fit.2.sim$beta[i,1] + fit.2.sim$beta[i,2]*x, add=TRUE,col="gray")
}
curve (coef(fit.2)[1] + coef(fit.2)[2]*x, add=TRUE, col="black")


####################
#### Ejercicios ####
####################

## 1)

## filepath
root <- rprojroot::is_rstudio_project
basename(getwd())
a <- read.table(root$find_file("Iair/exercise2.1.dat"), header = T)

# a
fit.ej1 <- lm (a$y ~ a$x1 + a$x2)
display(fit.ej1) 
summary(fit.ej1)

# b sale mal el plot cuando intento plotearlos juntos al x1 y x2
colors <- ifelse (a$x1==1, "black", "gray")
plot (a$x2, a$y, xlab="x2", ylab="y",
      col=colors, pch=20)
curve (cbind (1, 1, x) %*% coef(fit.3), add=TRUE, col="black")
curve (cbind (1, 0, x) %*% coef(fit.3), add=TRUE, col="gray")


# b intento 2, aca puedo plotear x1 o x2 por vez
fit.ej1b <- lm (a$y ~ a$x2)
plot (a$x2, a$y, xlab="X", ylab="Y")
curve (coef(fit.ej1b)[1] + coef(fit.ej1b)[2]*x, add=TRUE)


# c, no seria normal, no?
res <- resid(fit.ej1)
plot(fitted(fit.ej1), res)
abline(0,0)

# d no entendí este. 
x.new <- a[41:60, 2:3]
predict.lm(fit.ej1, x.new, interval="prediction", level=0.95)

## 2)
# falta el logaritmo. no lo tuve en cuenta. 
# earning.score = 29947.2 + height.score x 0.8

## 3) 

# a

# creo variables
var1 <- rnorm(1000,0,1)
var2 <- rnorm(1000,0,1)

# corro la regresion
fit.ej3 <- lm (var2 ~ var1)
plot (var1, var2, xlab="X", ylab="Y")
curve (coef(fit.ej3)[1] + coef(fit.ej3)[2]*x, add=TRUE)

summary(fit.ej3)
# no es significativa.

# b

z.scores <- rep (NA, 100)
for (k in 1:100) {
  var1 <- rnorm (1000,0,1)
  var2 <- rnorm (1000,0,1)
  fit <- lm (var2 ~ var1)
  z.scores[k] <- coef(fit)[2]/se.coef(fit)[2]
}

# veo cuales son significativos
significativos <- z.scores[z.scores > 1.96 | z.scores < -1.96]

# solo 7

## 4) 

# a

library(haven)

root <- rprojroot::is_rstudio_project
basename(getwd())

filepath = root$find_file("Iair/child.iq.dta")

library ("foreign")
iq.data <- read.dta (file = filepath) 

# creo variables
var1 <- iq.data$ppvt
var2 <- iq.data$momage

# corro la regresion
fit.ej4 <- lm (var2 ~ var1)
plot (var1, var2, xlab="X", ylab="Y")
curve (coef(fit.ej4)[1] + coef(fit.ej4)[2]*x, add=TRUE)

summary(fit.ej4)

# pareciera que a mayor edad mayor puntaje de test.

# como chequeo linealidad?

# como chequeo que las varianzas de los errores sean iguales? kolmovorov smirnov? 

# chequeo normalidad
res <- resid(fit.ej4)
plot(fitted(fit.ej4), res)
abline(0,0)
hist(res) # es normal pareciera

# b
# corro la regresion
library(arm)

fit.ej4b <- lm (iq.data$ppvt ~ iq.data$momage + iq.data$educ_cat)
summary(fit.ej4b)
display(fit.ej4b)

# c
# Asumo que las madres que fueron al hs son 3 y 4, y las que no fueron son 1 y 2
# ya que no sé significan las categorías.

hsMom <- rep(NaN, length(iq.data$educ_cat))
for (i in 1:length(hsMom)) {
  if (iq.data$educ_cat[i] == 1 | iq.data$educ_cat[i] == 2){
    hsMom[i] <- 0
  } else {
    hsMom[i] <- 1
  }
}
# meto la variable nueva en el df
iq.data$hsMom <- hsMom

# realizo la regresion con interaccion

fit.ej4c <- lm (iq.data$ppvt ~ iq.data$hsMom + iq.data$momage  
                + iq.data$hsMom:iq.data$momage)

display(fit.ej4c)
summary(fit.ej4c)

# plot # este plot me sale mal, o pienso que
colors <- ifelse (iq.data$hsMom==1, "black", "gray")
plot (iq.data$momage, iq.data$ppvt, xlab="Mother Age", ylab="Child test score",
      col=colors, pch=20)
curve (cbind (1, 1, x, 1*x) %*% coef(fit.4), add=TRUE, col="black")
curve (cbind (1, 0, x, 0*x) %*% coef(fit.4), add=TRUE, col="gray")

# d # fracase en este 
library(tidyverse)

# tomo los primeros 200 renglones
iq.data.first200 <- iq.data %>% slice(1:200)

# corro la regresion
fit.ej4d <- lm (iq.data.first200$ppvt ~ iq.data.first200$momage 
                + iq.data.first200$educ_cat)
display(fit.ej4d)

fit.ej4d.sim <- sim(fit.ej4d, n.sims= 200)

plot (iq.data.first200$momage, iq.data.first200$ppvt, xlab="Mother age", ylab="Child test score")
for (i in 1:10){
  curve (fit.ej4d.sim$beta[i,1] + fit.ej4d.sim$beta[i,2]*x, add=TRUE,col="gray")
}
curve (coef(fit.ej4d)[1] + coef(fit.ej4d)[2]*x, add=TRUE, col="black")


## 5)

# Read the data into R, including the variable names (headers)
library(haven)

root <- rprojroot::is_rstudio_project
basename(getwd())

filepath = root$find_file("Iair/ProfEvaltnsBeautyPublic.csv")

beauty.data <- read.table (file = filepath, header=T, sep=",")

# Attach the data so they are accessible using their variable names
library(R2WinBUGS)
attach.all (beauty.data) 

# a

# corro regresion
fit.ej5 <- lm (courseevaluation ~ btystdave) # a que se refiere con 
                                             #"controlling for various other inputs"
display(fit.ej5)

# ploteo
plot (btystdave, courseevaluation, xlab="beauty", ylab="course evaluations")
curve (coef(fit.ej5)[1] + coef(fit.ej5)[2]*x, add=TRUE)

# segundo plot, los valores residuales vs los fitted
res <- resid(fit.ej5)
plot(fitted(fit.ej5), res)
abline(0,0)
hist(res) # es normal pareciera

# b

# planteo un modelo con interaccion

fit.ej5b <- lm (courseevaluation ~ tenured + btystdave  
                + tenured:btystdave)
display(fit.ej5b)

# plot 
colors <- ifelse (tenured==1, "black", "gray")
plot (btystdave, courseevaluation, xlab="beauty", ylab="course evaluations",
      col=colors, pch=20)
curve (cbind (1, 1, x, 1*x) %*% coef(fit.ej5b), add=TRUE, col="black")
curve (cbind (1, 0, x, 0*x) %*% coef(fit.ej5b), add=TRUE, col="gray")