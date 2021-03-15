#######################
#### cod. capitulo ####
#######################

# lee el .dta de chicos con iq. Del capitulo 3

library(tidyverse)
library(haven)

filepath = "C:/Users/marcosembon/Google Drive/Investigacion iacho/maestría UBA/maestría pablo/papers/por leer/Regresion/Andrew Gelman/ARM_Data/child.iq/kidiq.dta"

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
# display(fit.ej1) no me encuentra display, tengo que instalar un paquete?
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
x.new <- data.frame (a$x1=5, a$x2=12)
predict (fit.ej1, x.new, interval="prediction", level=0.95)