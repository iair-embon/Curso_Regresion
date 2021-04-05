library('arm')
data <- read.dta('pollution.dta')

#delete outliers
data <- data[data$nox <= 50,]

plot(data$nox, data$mort, xlab="NOx concentration", ylab="Mortality rate")

#fit
fit <- lm(mort ~ nox, data=data)
display(fit)

# lm(formula = mort ~ nox, data = data)
# coef.est coef.se
# (Intercept) 913.30    11.69 
# nox           2.29     0.72 
# ---
#   n = 55, k = 2
# residual sd = 56.44, R-Squared = 0.16

residuals <- resid(fit)
predictions <- fitted(fit)
plot(predictions, residuals)
mean(residuals) #-2e-15
sd(residuals) #56

#transformation
log.mort <- log(data$mort)
log.nox <- log(data$nox)
fit.log <- lm(log.mort ~ log.nox)
display(fit.log)

log.residuals <- resid(fit.log)
log.predictions <- fitted(fit.log)
plot(log.predictions, log.residuals)
mean(log.residuals) #2e-18
sd(log.residuals) #0.06

#log.nox = 0.03 significa que por cada 1% que aumenta la
#concentracion de NOx, la tasa de mortalidad aumenta 0.03%

#extended model
log.so2 <- log(data$so2)
log.hc <- log(data$hc)
fit.2 <- lm(log.mort ~ log.so2 + log.hc + log.nox)
display(fit.2)
