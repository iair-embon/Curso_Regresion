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

# hago una regresion logistica para saber que tanto se puede predecir si acerto o no
# a partir de la confianza en general (no sujeto por sujeto)

fit.4.0 <- glm (discrimination_is_correct ~ confidence_key, family=binomial(link="logit"), data = df_total)
display(fit.4.0)

fit.4.0 <- glm (discrimination_is_correct ~ confidence_key + AQ + genero + AQ:genero, family=binomial(link="logit"), data = df_total)
display(fit.4.0)

fit.4.0

# corro una regresion logistica por sujeto para obtener su coeficiente de confianza
# la idea es despues correlacionarla con su nivel de metacog
reg.coef.conf <- rep(NaN, length(unique(df_total$sujetos)))
sujetos.existentes <- unique(df_total$sujetos)

for (i in 1:length(unique(df_total$sujetos))) {
  f <- sujetos.existentes[i] 
  df_total.subset <- df_total[df_total$sujetos==f,]
  fit.4.0 <- glm (discrimination_is_correct ~ confidence_key, family=binomial(link="logit"), data = df_total.subset)
  b <- coef (fit.4.0)
  reg.coef.conf[i] <- b[2]
}

plot(d.sin.normalizar$mc, reg.coef.conf)

# hago una regresion entre metacog y reg.coef.conf
fit.4.1 = lm(d.sin.normalizar$mc ~  reg.coef.conf)
display(fit.4.1)

fit.4.2 = lm(reg.coef.conf~ d.sin.normalizar$mc )
display(fit.4.2)

# falta el 4.c