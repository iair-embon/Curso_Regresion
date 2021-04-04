########### CAP 3 EXERCISES ###########

################
## Exercise 1 ##
################

# The folder pyth contains outcome y and inputs x1,x2 for 40 data points, with a further 20 points with the
#inputs but no observed outcome. Save the file to your working directory and read it into R using the
# read.table() function

data <- read.table("C:/Users/nicol/Documents/conicet/cursos/regresiones multinivel/capitulo_3/exercise2.1.dat",
                   header = TRUE)

# Use R to fit a linear regression model predicting y from x1,x2,using the first 40 data points in the file.
# Summarize the inferences and check the fit of your model

data_40 <- data[seq(1,40,1),]

regresion1 <- lm(data_40$y ~ data_40$x1 + data_40$x2)
summary(regresion1)

# Display the estimated model graphically as in Figure 3.2.

plot(data_40$x1, data_40$y, xlab = 'x1 (grey) & x2 (black)', ylab = 'y', col = 'grey', xlim = c(0, 20))
points(data_40$x2, data_40$y, col = 'black')


# intento de plotear por separado...

fit.ej1a <- lm(data_40$y ~ data_40$x2)
plot(data_40$x2, data_40$y, ylim = c(0, 25))
curve(coef(fit.ej1a)[1] + coef(fit.ej1a)[2]*x, add = TRUE)

fit.ej1b <- lm(data_40$y ~ data_40$x1)
plot(data_40$x1, data_40$y, ylim = c(0, 25))
curve(coef(fit.ej1b)[1] + coef(fit.ej1b)[2]*x, add = TRUE)

# Make a residual plot for this model. Do the assumptions appear to be met?

plot(regresion1$residuals)

# Make predictions for the remaining 20 data points in the file.
x.new    <- data.frame(x1 = data$x1[seq(41,60,1)], x2 = data$x2[seq(41,60,1)])
predict.lm(object = regresion1, newdata = x.new, interval = 'prediction', level = 0.95) 

# How confident do you feel about these predictions? 95% ?

################
## Exercise 2 ##
################

# me base en https://github.com/IamGianluca/arm para los logaritmos

# encontrar la intercepcion
alpha <- log(30000) - (0.008/0.01)*log(66)

# ecuacion: log.earnings = alpha + (0.008/0.01) * log(66)

# residual standar deviation
sd <- 0.1 * .50 / .95

# R2 of the regression
sd.population = 0.05
R2 <- 1 - (sd^2 / sd.population^2) # puede dar negativo??

################
## Exercise 3 ##
################

# create independent variables
var1 <- rnorm(1000,0,1)
var2 <- rnorm(1000,0,1)

# run a regression on each other
reg1 <- lm(var1 ~ var2)
summary(reg1) # slope coefficient is not significant
plot(var2, var1)

reg2 <- lm(var2 ~ var1)
summary(reg2) # slope coefficient is not significant
plot(var1, var2)

# now run a simulation repeating this process 100 times, saving the z-score for each one. How many
# of them are significant (i.e. > 2 SE)?

z.scores <- rep (NA, 100) 
for (k in 1:100){ 
  var1 <- rnorm (1000,0,1) 
  var2 <- rnorm (1000,0,1) 
  fit <- lm (var2 ~ var1) 
  z.scores[k] <- coef(fit)[2]/se.coef(fit)[2] 
}

length(which(z.scores > 2)) # no tengo la funcion se.coef pero intuyo que asi deberia andar

################
## Exercise 4 ##
################

library ("foreign") 
iq.data <- read.dta ("child.iq.dta")

# fit a regression of child test scores on mother's age. Display data and fitted model
# check assumptions and interpret slope coefficient

fit.iq1 <- lm(iq.data$ppvt ~ iq.data$momage)
summary(fit.iq1)
plot(iq.data$momage, iq.data$ppvt)
curve(coef(fit.iq1)[1] + coef(fit.iq1)[2]*x, add = T)

# alfa indica que una mamá que da a luz a los 0 años (escenario imposible) predice un 
# puntaje en el test de iq de 67.7827; beta indica que cada año que se suma en la edad
# de la madre a la hora de dar a luz predice un aumento promedio de 0.8403 en el test de iq del hijo.
# Esta sería la interpretación predictiva del cap. 3, verdad?

# When do you recommend mothers should give birth? mientras más grandes, mejor

# What are you assuming in making these recommendations? que la inclinación de la curva del 
# modelo no va a cambiar a valores más altos de x.

# repeat the regression but incluiding mothers education

fit.iq2 <- lm(iq.data$ppvt ~ iq.data$momage + iq.data$educ_cat)
summary(fit.iq2)
plot(iq.data$momage, iq.data$ppvt, xlab = 'Mom age', ylab = 'Child test score')
colors <- c('grey', 'yellow', 'orange', 'red')
for(i in 1:4){
  curve(cbind(1, x, i) %*% coef(fit.iq2), add=TRUE, col=colors[i])
}

# interpret both coefficients: b1 predice que, manteniendo la categoría de educación de la 
# madre constante en su valor más bajo (1), un aumento de un año en la edad en la cual la 
# madre da a luz predice un aumento promedio de 0.34 en el rendimento del test. 
# b2 predice que, manteniendo constante la edad de la madre en su valor más bajo, un aumento 
# en la categoría de educación de la madre predice una suba promedio de 4.7113943 en el resultado
# del test de iq del hijo.

# have your conclussions about the time of birth changed? sí, ahora la edad parece tener un 
# efecto mínimo. Importa más la educación materna.

# create an indicator of whether moms have finished high school or not.
# asumo que 3 y 4 indican que finalizó el colegio y 1 y 2 indican que no

iq.data$momhs <- ifelse(iq.data$educ_cat <= 2, 0, 1)

# consider interactions between HS completion and mom age.
fit.iq3 <- lm(iq.data$ppvt ~ iq.data$momage * iq.data$momhs)
summary(fit.iq3)

# create a plot that shows the separate regression lines for each HS completion status group

plot(iq.data$momage, iq.data$ppvt) 
curve(cbind(1, 1, x, 1 * x) %*% coef(fit.iq3), add=TRUE)  # completed high school
curve(cbind(1, 0, x, 0 * x) %*% coef(fit.iq3), add=TRUE)  # didn't complete high school
# no puedo añadir las curvas, dan valores negativos 


#################
## EJERCICIO 5 ##
#################

data <- read.csv('ProfEvaltnsBeautyPublic.csv')

fit.beauty <- lm(data$courseevaluation ~ data$btystdave + data$female)
summary(fit.beauty)

plot(data$btystdave, data$courseevaluation) # no estoy seguro que este plot esté bien
curve(cbind(1, x, 1 * x) %*% coef(fit.beauty), col = 'blue', add=TRUE)  # female
curve(cbind(1, x, 0 * x) %*% coef(fit.beauty), col = 'red', add=TRUE)  # not-female

plot(data$btystdave, data$courseevaluation)
curve(coef(fit.beauty)[1] + coef(fit.beauty)[2]*x, add = T)

plot(data$female, data$courseevaluation)
curve(coef(fit.beauty)[1] + coef(fit.beauty)[3]*x, add = T)

plot(fit.beauty$fitted.values, fit.beauty$residuals)
abline(a = 0, b = 0)


# considering other models...

m2.beauty <- lm(data$courseevaluation ~ data$btystdave + data$female * data$age)
summary(m2.beauty)

par(mfrow = c(1,3))
plot(data$btystdave, data$courseevaluation)
curve(coef(m2.beauty)[1] + coef(m2.beauty)[2]*x, add = T)
plot(data$female, data$courseevaluation)
curve(coef(m2.beauty)[1] + coef(m2.beauty)[3]*x, add = T)
plot(data$age, data$courseevaluation)
curve(coef(m2.beauty)[1] + coef(m2.beauty)[4]*x, add = T)

