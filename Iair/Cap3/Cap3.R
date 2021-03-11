# lee el .dta de chicos con iq. Del capitulo 3

filepath = "C:/Users/marcosembon/Google Drive/Investigacion iacho/maestría UBA/maestría pablo/papers/por leer/Regresion/Andrew Gelman/ARM_Data/child.iq/kidiq.dta"

ffc.stata <- read_dta(file = filepath)

fit.2 <- lm (ffc.stata$kid_score ~ ffc.stata$mom_iq)
plot (ffc.stata$mom_iq, ffc.stata$kid_score, xlab="Mother IQ score", ylab="Child test score")
curve (coef(fit.2)[1] + coef(fit.2)[2]*x, add=TRUE)

