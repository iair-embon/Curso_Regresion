library('arm')

data <- data.frame(
  y = rnorm(1000),
  x = rnorm(1000)
)

#linear regression
fit <- lm(y ~ x, data)
disp <- display(fit)
print(abs(disp$coef[2]/disp$se[2])) # ~= 0.1 -> not significant!

#repeat 100 times
z.scores <- c()
for(i in 1:100){
  data <- data.frame(
    y = rnorm(1000),
    x = rnorm(1000)
  )
  fit <- lm(y ~ x, data)
  disp <- display(fit)
  z.scores[i] <- abs(disp$coef[2]/disp$se[2])
}
stat.significant <- sum(z.scores > 2)
print(stat.significant) # ~= 5 (95% CI)