##########################
#### Ejercicios cap 5 ####
##########################
library(arm)

## 1) 
library(haven)

root <- rprojroot::is_rstudio_project
basename(getwd())
#read each line and convert

filepath = root$find_file("Iair/Cap5/nes/nes5200_processed_voters_realideo.dta")

d <- read_dta(file = filepath)

# limpio un poco el df
d <- data.frame(
  vote = d$vote,
  gender = d$gender,
  race = d$race,
  educ2 = d$educ2,
  partyid7 = d$partyid7,
  ideo_feel = d$ideo_feel
)

d$vote[d$vote == 1]<-0
d$vote[d$vote == 2]<-1

d <- d[complete.cases(d), ]

# ahora si, hago regresion logistica
fit.1.0 <- glm (vote ~ gender, family=binomial(link="logit"), data = d)
display(fit.1.0)

fit.1.1 <- glm (vote ~ gender + race + educ2 + partyid7 + ideo_feel, family=binomial(link="logit"), data = d)
display(fit.1.1)

# interetar coeficientes segun la diferencia predicha promedio de cambiar de 1
# a 2 en educ2
b <- coef (fit.1.1)
hi <- 2
lo <- 1
# le puse el df antes de cada variable, sino me daba error, esta bien?
delta <- invlogit (b[1] + b[2] *d$gender +  b [3] *d$race + b[4]*hi + b [5] *d$partyid7 + b[6]*d$ideo_feel) -
  invlogit (b [1] + + b [2] *d$gender +  b [3] *d$race + b[4]*lo + b [5] * d$partyid7 + b[6]*d$ideo_feel)
print (mean (delta))

# da 0.04739758
# esto seria que, en promedio, los que tienen educacion 2 
# tienen un 4.7% mas de probabilidad de votar a 1 (bush?) que los que tienen
# educacion 1.

## 2 (lo hago con compu)
# a
x <- rnorm(1000, mean = 10, sd= 1)
y <- invlogit(x)
plot(x,y)

# b
x <- rnorm(1000, mean = 10, sd= 1)
y <- invlogit(2+ x)
plot(x,y)

# c
x <- rnorm(1000, mean = 10, sd= 1)
y <- invlogit(2* x)
plot(x,y)

# d
x <- rnorm(1000, mean = 10, sd= 1)
y <- invlogit(2 + 2 * x)
plot(x,y)

# e
x <- rnorm(1000, mean = 10, sd= 1)
y <- invlogit(-2* x)
plot(x,y)

## 3

# incoming
parents.incoming.x10000 <- rnorm(1000, mean = 6, sd = 2)
# graduate 1 = yes, 0 = no
graduate <- invlogit(0.27 + 0.88 * parents.incoming.x10000)

d3 <- data.frame(
  p.i.x10000 = parents.incoming.x10000,
  g = graduate
)

fit.3.0 <- glm (graduate ~ p.i.x10000, family=binomial(link="logit"), data = d3)
display(fit.3.0)

plot(d3$p.i.x10000 , d3$g)

# interetar coeficientes segun la diferencia predicha promedio de cambiar de 0
# a 6 en incoming
b <- coef (fit.3.0)
hi <- 6
lo <- 0
# le puse el df antes de cada variable, sino me daba error, esta bien?
delta <- invlogit (b[1] +  b[2]*hi) -
  invlogit (b [1] + b[2]*lo)
print (mean (delta))

# da 0.4290347
# esto seria que, en promedio, los que tienen incresos de 60000 
# tienen un 4.2% mas de probabilidad de graduarse que los que tienen
# ingresos 0.

## 4


# cargo el df de tea y metacog
# preparo la variable a predecir
df_total$discrimination_is_correct[df_total$discrimination_is_correct == TRUE] <- 1 
df_total$discrimination_is_correct[df_total$discrimination_is_correct == FALSE] <- 0

df_total.solo.FyM <- df_total[df_total$genero == "Masculino" |df_total$genero == "Femenino",]

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

d.sin.normalizar.solo.FyM <- d.sin.normalizar[d.sin.normalizar$Im == "Masculino"
                                              |d.sin.normalizar$Im == "Femenino",]

plot(d.sin.normalizar.solo.FyM$mc, reg.coef.conf)
plot(reg.coef.conf, d.sin.normalizar.solo.FyM$mc)


d.sin.normalizar.solo.FyM$normal.mc <- (d.sin.normalizar.solo.FyM$mc - mean(d.sin.normalizar.solo.FyM$mc))/sd(d.sin.normalizar.solo.FyM$mc)

d.sin.normalizar.solo.FyM$normal.reg.coef.conf <- (d.sin.normalizar.solo.FyM$reg.coef.conf - mean(d.sin.normalizar.solo.FyM$reg.coef.conf))/sd(d.sin.normalizar.solo.FyM$reg.coef.conf)

plot(d.sin.normalizar.solo.FyM$normal.mc, reg.coef.conf)

# hago una regresion entre metacog y reg.coef.conf PREGUNTAR DUDA, CAMBIO DE PREDICTOR
fit.4.2 = lm(d.sin.normalizar.solo.FyM$mc ~  reg.coef.conf)
display(fit.4.2)

fit.4.3 = lm(reg.coef.conf~ d.sin.normalizar.solo.FyM$mc )
display(fit.4.3)

# corro una correlacion entre metacog area bajo la curva tipo 2 y la de regresion logistica
x <- reg.coef.conf 
y <- d.sin.normalizar.solo.FyM$mc
cor.test(x, y, method=c("pearson"))


# quiero predecir la metacog medido como la cuanto predice la confianza el acierto
fit.4.4 = lm(reg.coef.conf~ aq + Im + aq:Im , data = d.sin.normalizar.solo.FyM)
display(fit.4.4)

# C

# Elijo el modelo fit.4.1

# i

# El termino constante predice la probabilidad de acertar si todos los
# predictores estan en 0 (si es femenino, tiene 0 confianza?, AQ 0?)     DUDA CONFIANZA 0?
# como no tiene sentido no lo interpretamos

# El coeficiente para confianza 0.4, es significativo. Si aplicamos la regla de 
# dividir por 4: 0.4/4= 0.1, se puede ver que el modelo predice que agregar 1
# la confianza corresponde a una diferencia postiva en la probabilidad de acertar
# de 40% como maximo.

# El coeficiente para AQ 0.1, no es significativo. Si aplicamos la regla de 
# dividir por 4: 0.1/4= 0.025, se puede ver que el modelo predice que agregar 1
# al AQ corresponde a una diferencia postiva en la probabilidad de acertar
# de 2,5% como maximo.

# El coeficiente para genero 0.02, no es significativo. Si aplicamos la regla de    DUDA: esta bien usar genero?
# dividir por 4: 0.02/4= 0.005, se puede ver que el modelo predice que agregar ser masculino
# corresponde a una diferencia postiva en la probabilidad de acertar
# de 0.5% como maximo.

# Interpreto los coeficientes en base a la media

aq.mean <- mean(df_total.solo.FyM$AQ) # 25.58763
confidence_key.mean <- mean(df_total.solo.FyM$confidence_key) # 2.626384

# DUDA : como interpreto con genero las medias?

# mientras saco genero del modelo para interpretar los coeficientes en base a la media
# Corro el nuevo modelo:
fit.4.5 <- glm (discrimination_is_correct ~ confidence_key + AQ
                , family=binomial(link="logit"), data = df_total.solo.FyM)
display(fit.4.5)

# constant term en base a la media:
prob.acierto <- invlogit(-0.09 + 0.4 * 2.626384 + 0.01 * 25.58763)# = 0.7714347
# la probabilidad de acertar en la media de AQ y de confianza es de 0.77

# coef de confidence_key en base a la media:
# aca necesitaria la interaccion
# pero me da 0. Asi que no lo terpreto, igual con el AQ

# ii
# error rate:
new.aq <- rnorm(200, mean= 25.58763, sd= 3.398191)
new.confidence <- rnorm(200, mean = 2.626384, sd= 0.9980241)
new.df <- data.frame(AQ = new.aq,
                     confidence_key = new.confidence)

predicted <- predict(fit.4.5,newdata = new.df)

error.rate <- mean ((predicted>0.5 & df_total.solo.FyM$discrimination_is_correct==0) | 
                      (predicted<.5 & df_total.solo.FyM$discrimination_is_correct==1))




