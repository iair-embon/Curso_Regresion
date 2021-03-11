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


