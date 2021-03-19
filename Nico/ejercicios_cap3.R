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
