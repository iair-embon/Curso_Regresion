#### EJERCICIO 4

### lineas para normalizar variables y hacer regresion 

library(arm)

root <- rprojroot::is_rstudio_project
basename(getwd())

## df_total:
filepath <- (root$find_file("Iair/d.sin.normalizar.solo.FyM.Rda"))
load(file= filepath)

filepath <- (root$find_file("Iair/df_total.solo.FyM.Rda"))
load(file= filepath)

# hago una regresion logistica para saber que tanto se puede predecir si acerto o no
# a partir de la confianza en general (no sujeto por sujeto)

fit.4.0 <- glm (discrimination_is_correct ~ confidence_key, 
                family=binomial(link="logit"), data = df_total.solo.FyM)
display(fit.4.0)

fit.4.1 <- glm (discrimination_is_correct ~ confidence_key + AQ + genero +
                  AQ:genero, family=binomial(link="logit"), data = df_total.solo.FyM)
display(fit.4.1)

# corro una regresion logistica por sujeto para obtener su coeficiente de confianza
# la idea es despues correlacionarla con su nivel de metacog
reg.coef.conf <- rep(NaN, length(unique(df_total.solo.FyM$sujetos)))
sujetos.existentes <- unique(df_total.solo.FyM$sujetos)

for (i in 1:length(unique(df_total.solo.FyM$sujetos))) {
  f <- sujetos.existentes[i] 
  df_total.subset <- df_total.solo.FyM[df_total.solo.FyM$sujetos==f,]
  fit.4 <- glm (discrimination_is_correct ~ confidence_key, 
                family=binomial(link="logit"), data = df_total.subset)
  b <- coef (fit.4)
  reg.coef.conf[i] <- b[2]
}

# agrego los coeficientes de la regresion al d.sin.normalizar.solo.FyM

d.sin.normalizar.solo.FyM$reg.coef.conf <- reg.coef.conf

plot(d.sin.normalizar.solo.FyM$mc, d.sin.normalizar.solo.FyM$reg.coef.conf, ylim = c(-3,3), xlim = c(-2.5,3.5))

# pruebo normalizar ambas medidas de metacognicion

d.metacog.normalizado <- data.frame(mc.curva.roc = d.sin.normalizar.solo.FyM$mc,
                                    mc.reg.log = d.sin.normalizar.solo.FyM$reg.coef.conf)

mean.mc <- mean(d.metacog.normalizado$mc.curva.roc) 
sd.mc <- sd(d.metacog.normalizado$mc.curva.roc)
d.metacog.normalizado$mc.curva.roc <- (d.metacog.normalizado$mc.curva.roc - mean.mc)/ sd.mc
# antes de normaliar la metacog sacada desde la regresion, saco al sujeto 42, que apreto siempre 4

library(tidyverse)

d.metacog.normalizado <- d.metacog.normalizado %>% drop_na()

mean.mc <- mean(d.metacog.normalizado$mc.reg.log)
sd.mc <- sd(d.metacog.normalizado$mc.reg.log)
d.metacog.normalizado$mc.reg.log <- (d.metacog.normalizado$mc.reg.log - mean.mc)/sd.mc

# vuelvo a plotear
plot(d.metacog.normalizado$mc.curva.roc, d.metacog.normalizado$mc.reg.log, ylim = c(-1,1), xlim = c(-2.5,3.5))

# hago una regresion entre metacog y reg.coef.conf PREGUNTAR DUDA, CAMBIO DE PREDICTOR
fit.4.2 = lm(d.metacog.normalizado$mc.curva.roc ~  d.metacog.normalizado$mc.reg.log)
display(fit.4.2)

plot (d.metacog.normalizado$mc.reg.log, d.metacog.normalizado$mc.curva.roc, xlab="mc.reg.log", ylab="mc.curva.roc")
curve (coef(fit.4.2)[1] + coef(fit.4.2)[2]*x, add=TRUE) 

# hay varios outliers en mc.reg.log, pruebo que pasa si los elimino

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

new.mc.reg.log <- remove_outliers (d.metacog.normalizado$mc.reg.log)
d.metacog.normalizado$new.mc.reg.log <- new.mc.reg.log
# saco los na

d.metacog.normalizado.NA.droped <- d.metacog.normalizado %>% drop_na()

# corro una nueva regresion
fit.4.3 = lm(d.metacog.normalizado$mc.curva.roc ~  d.metacog.normalizado$new.mc.reg.log)
display(fit.4.3)

# ploteo
plot (d.metacog.normalizado$new.mc.reg.log, d.metacog.normalizado$mc.curva.roc, xlab="new.mc.reg.log", ylab="mc.curva.roc")
curve (coef(fit.4.3)[1] + coef(fit.4.3)[2]*x, add=TRUE) 

