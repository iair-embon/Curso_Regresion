#required libraries
library("arm")

#import and filtering
data <- read.table("exercise2.1.dat", header=TRUE)
test.set <- data[is.na(data$y),]
data <- data[!is.na(data$y),]

#linear regression
fit <- lm(y ~ x1 + x2, data)
display(fit)

#plot
fit.sim <- sim(fit)
plot(data$x1, data$y, xlab="x1", ylab="y")
for(i in 1:length(data$y)){
  curve(cbind(1, x, mean(data$x2)) %*% coef(fit.sim)[i,], col="grey", add=TRUE, lwd=0.5)
}
curve(cbind(1, x, mean(data$x2)) %*% coef(fit), col="black", add=TRUE, lwd=2)

plot(data$x2, data$y, xlab="x2", ylab="y")
for(i in 1:length(data$y)){
  curve(cbind(1, mean(data$x1), x) %*% coef(fit.sim)[i,], col="grey", add=TRUE, lwd=0.5)
}
curve(cbind(1, mean(data$x1), x) %*% coef(fit), col="black", add=TRUE, lwd=2)

#residual plot
#from: https://github.com/IamGianluca/arm/blob/master/ch3/ch3_1.ipynb
residuals <- resid(fit)
predictions <- fitted(fit)
plot(predictions, residuals)
mean(residuals)
sd(residuals)

#prediction
predict(fit, test.set, interval="prediction", level=0.95)