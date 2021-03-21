library("foreign")
library("arm")

iq.data <- read.dta ("child.iq.dta")

#linear regression
fit <- lm(ppvt ~ momage, iq.data)
disp <- display(fit)
z.score <- abs(disp$coef[2]/disp$se[2]) #z-score ~= 2.2 and R^2 = 0.01 suggests bad fit?
#plot data and line
plot(iq.data$momage, iq.data$ppvt, xlab="mom age", ylab="score")
curve(cbind(1, x) %*% coef(fit), col="black", add=TRUE, lwd=2)
#plot residuals
residuals <- resid(fit)
predictions <- fitted(fit)
plot(predictions, residuals)
#slope coefficient: for every year old the mom is, the child scores 0.84 higher on average
#The moms should give birth as late as possible (we don't have information for 30 year old or more mothers, so we have to extrapolate?)

fit2 <- lm(ppvt ~ momage + educ_cat, iq.data)
disp2 <- display(fit2)
z.score.momage <- abs(disp2$coef[2]/disp2$se[2]) #0.862 not significant
z.score.educ_cat <- abs(disp2$coef[3]/disp2$se[3]) #3.58 significant!
#momage coefficient indicates how much the child's score changes if the mother gives birth one year later,
#all else being equal. educ_cat coefficient indicates how much the child's score changes if the mother 
#has a higher education level, all else being equal.
#conclusion about time of birth changed because the slope coefficient is no longer signifiacant
#Plus, mother's education is a more relevant predictor than age

#mom highschool
iq.data$highscool <- ifelse(iq.data$educ_cat >= 2, 1, 0)
fit.hs <- lm(ppvt ~ highscool * momage, iq.data)
display(fit.hs)

#taken from: https://github.com/IamGianluca/arm/blob/master/ch3/arm_ch3p4.md
colors <- ifelse(iq.data$highscool == 1, "#9ecae1", "#3182bd")
plot(iq.data$momage, iq.data$ppvt, xlab="Mother age", ylab="Child test score", col=colors, pch=20)
curve(cbind(1, 1, x, 1 * x) %*% coef(fit.hs), add=TRUE, col="#9ecae1") # completed high school
curve(cbind(1, 0, x, 0 * x) %*% coef(fit.hs), add=TRUE, col="#3182bd") # didn't complete high school

#training with 200 rows
iq.sub <- iq.data[1:200,]
fit.sub <- lm(ppvt ~ momage + educ_cat, iq.sub)
display(fit.sub)
predicted <- predict(fit.sub, iq.data[201:400,])
plot(iq.data[201:400,]$ppvt, predicted, xlab="actual", ylab="predicted")