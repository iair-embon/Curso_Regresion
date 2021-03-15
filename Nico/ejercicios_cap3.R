########### CAP 3 EXERCISES ###########

# Exercise 1

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

plot(data$x1, data$y, xlab = 'x1 (grey) & x2 (black)', ylab = 'y', col = 'grey', xlim = c(0, 20))
points(data$x2, data$y, col = 'black')


# intento de plotear por separado...

fit.ej1a <- lm(data$y ~ data$x2)
plot(data$x2, data$y, ylim = c(0,25))
curve(coef(fit.ej1a)[1] + coef(fit.ej1a)[2]*x, add = TRUE)

fit.ej1b <- lm(data$y ~ data$x1)
plot(data$x1, data$y)
curve(coef(fit.ej1b)[1] + coef(fit.ej1b)[2]*x, add = TRUE)

# Make a residual plot for this model. Do the assumptions appear to be met?

plot(regresion1$residuals)

# Make predictions for the remaining 20 data points in the file.



# How confident do you feel about these predictions?