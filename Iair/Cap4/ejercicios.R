##########################
#### Ejercicios cap 4 ####
##########################

## 1) # en pag 62 tira la posta para este ejercicio
# no entendi por que es negativo en la que tira ian y en
#  -(exp(0.25)) = -1.28 and exp(0.25) = 1.28

## 2) log(weight) = −3.5+2.0 log(height) + error   # no me queda claro si hay que transformar la variable predictora o no a log scale
library(arm)

height <- rnorm(500 ,mean = 63,sd = 5)
log_weight <- -3.5+2.0 * log(height) + 0.25 # xq ian pone en error: 
                                            # rnorm(n=length(heights), mean=0, sd=0.25)

df <- data.frame(h = height, log_w = log_weight)

fit.1 <- lm(df$log_w ~ df$h)
display(fit.1)

plot (df$h, df$log_w, xlab="height", ylab="weight")
curve (coef(fit.1)[1] + coef(fit.1)[2]*x, add=TRUE)

# ver a que se debe el error


## 2 # no pude levantar la base de datos wfw90.dat.txt que es la que pienso que se refiere este ej. 
# pero levante la heights.dta en su lugar, como hizo nico

# a

library(haven)

root <- rprojroot::is_rstudio_project
basename(getwd())
#read each line and convert

filepath = root$find_file("Iair/Cap4/heights.dta")

data <- read_dta(file = filepath)

# saco los casos incompletos
data <- data[complete.cases(data), ]

data2<- data 

fit.2 <- lm(data$earn ~ data$height) 
display(fit.2)

plot (data$height, data$earn, xlab="height", ylab="earn")
curve (coef(fit.2)[1] + coef(fit.2)[2]*x, add=TRUE) 

# b

# estandarizo usando z scores
data$earn <- (data$earn - mean(data$earn)) / sd(data$earn)
data$height <- (data$height - mean(data$height)) / sd(data$height)

# fiteo devuelta
fit.3 <- lm(data$earn ~ data$height) 
display(fit.3)

plot (data$height, data$earn, xlab="height", ylab="earn")
curve (coef(fit.3)[1] + coef(fit.3)[2]*x, add=TRUE) 

# c 

# estandarizo usando z scores, pero dividiendo por dos desvios
data$earn <- (data$earn - mean(data$earn)) / (2*sd(data$earn))
data$height <- (data$height - mean(data$height)) / (2*sd(data$height))

# fiteo devuelta
fit.3 <- lm(data$earn ~ data$height) 
display(fit.3)

plot (data$height, data$earn, xlab="height", ylab="earn")
curve (coef(fit.3)[1] + coef(fit.3)[2]*x, add=TRUE) 

# utilizo escala logaritmica - me tira error, hay algunos valores que tienen a infinito creo. 
data2$earn <- log(data2$earn)

# fiteo devuelta
fit.4 <- lm(data2$earn ~ data2$height) 
display(fit.4)

plot (data$height, data$earn, xlab="height", ylab="earn")
curve (coef(fit.4)[1] + coef(fit.4)[2]*x, add=TRUE) 

# estandarizo usando z scores pero con la variable sexo tambien
data$earn <- (data$earn - mean(data$earn)) / sd(data$earn)
data$height <- (data$height - mean(data$height)) / sd(data$height)


# fiteo devuelta
fit.5 <- lm(data$earn ~ data$height + data$sex) 
display(fit.5)

fit.ej5b <- lm (data$earn ~ data$height + data$sex  
                + data$height : data$sex)
display(fit.ej5b)

# plot 
colors <- ifelse (data$sex ==1, "black", "gray")
plot (data$height , data$earn , xlab="height", ylab="earn",
      col=colors, pch=20)
curve (cbind (1, 1, x, 1*x) %*% coef(fit.ej5b), add=TRUE, col="black")
curve (cbind (1, 0, x, 0*x) %*% coef(fit.ej5b), add=TRUE, col="gray")


## 3

# no encuentro la base de datos que usa.

## 4

# a

library(haven)

root <- rprojroot::is_rstudio_project
basename(getwd())
#read each line and convert

filepath = root$find_file("ARM_Data/pollution.dta")

data.pollution <- read_dta(file = filepath)

plot(data.pollution$nox ,data.pollution$mort , main="Scatterplot",
     xlab="nitric oxides ", ylab="mortality rate ", pch=19)

fit.4a <- lm(data.pollution$mort ~ data.pollution$nox)
display(fit.4a)

res <- resid(fit.4a)
plot(fitted(fit.4a), res)
abline(0,0)
hist(res) # es normal pareciera

# b

# utilizo escala logaritmica  
data.pollution.mort.log <- log(data.pollution$mort)
data.pollution.nox.log <- log(data.pollution$nox)

# fiteo devuelta
fit.4b <- lm(data.pollution.mort.log ~ data.pollution.nox.log) 
display(fit.4b)
summary(fit.4b)


plot (data.pollution.nox.log, data.pollution.mort.log, xlab="log nitric oxides",
      ylab="log mortality rate")
curve (coef(fit.4b)[1] + coef(fit.4b)[2]*x, add=TRUE)

# c
# el coef de nox.log me da 0.02. Esto significaria que una diferencia de 1 en nox.log 
# corresponde con una diferencia en 0.02 de mort.log

# d

# utilizo escala logaritmica para las variables que me faltan
data.pollution.so2.log <- log(data.pollution$so2)
data.pollution.hc.log <- log(data.pollution$hc)

# fiteo devuelta
fit.4d <- lm(data.pollution.mort.log ~ data.pollution.nox.log +data.pollution.so2.log +
               data.pollution.hc.log) 
display(fit.4d)
summary(fit.4d)

# no se me ocurre como graficar esto

# e
data.pollution.log.5 <- data.frame(mort.log = data.pollution.mort.log,
                                   nox.log = data.pollution.nox.log,
                                   so2.log = data.pollution.so2.log,
                                   hc.log = data.pollution.hc.log)

data.pollution.log.5.mitad1 <- data.pollution.log.5[1:(nrow(data.pollution.log.5)/2),]
data.pollution.log.5.mitad2 <- data.pollution.log.5[(nrow(data.pollution.log.5)/2+1):
                                                      nrow(data.pollution.log.5),]


fit.5 <- lm(data.pollution.log.5.mitad1$mort.log ~ data.pollution.log.5.mitad1$nox.log +
              data.pollution.log.5.mitad1$so2.log +
              data.pollution.log.5.mitad1$hc.log) 
display(fit.5)

prediccion <- predict(fit.5, data.pollution.log.5.mitad2)


