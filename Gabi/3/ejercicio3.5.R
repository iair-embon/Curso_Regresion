beauty.data <- read.csv(file = 'ProfEvaltnsBeautyPublic.csv')

#linear regression
fit <- lm(courseevaluation ~ btystdave, beauty.data)
display(fit)
plot(beauty.data$btystdave, beauty.data$courseevaluation, xlab="beauty", ylab="course evaluation")
curve(cbind(1, x) %*% coef(fit), col="black", add=TRUE, lwd=2)

#residuals
residuals <- resid(fit)
predictions <- fitted(fit)
print(sd(residuals))
plot(predictions, residuals)