library("foreign")
library("arm")
#cleaning
raw <- read.dta("heights.dta")
data <- raw[raw$earn > 0 & !is.na(raw$earn),]

#transformation
earn <- data$earn
z.height <- (data$height - mean(data$height))/sd(data$height)

#fit
fit <- lm(earn ~ z.height)
display(fit)

#height, sex, education model
sex <- factor(data$sex, labels=c("male", "female"))
ed <- data$ed
ed[ed > 0 & ed <= 8] <- 1
ed[ed > 8 & ed <= 12] <- 2
ed[ed > 12 & ed <= 16] <- 3
ed[ed == 17] <- 4
ed[ed == 18] <- 5
fit.2 <- lm(earn ~ z.height + sex * ed)
display(fit.2)

#no interaction

# lm(formula = earn ~ z.height + sex + ed)
# coef.est  coef.se  
# (Intercept)   7781.19   1800.72
# z.height       736.62    707.85
# sexfemale   -10091.87   1426.51
# ed            7849.11    579.65
# ---
#   n = 1192, k = 4
# residual sd = 17318.31, R-Squared = 0.21

#interaction sex and education

# lm(formula = earn ~ z.height + sex * ed)
# coef.est coef.se 
# (Intercept)   2465.94  2465.41
# z.height       807.15   705.57
# sexfemale     -195.27  3454.53
# ed            9777.26   842.51
# sexfemale:ed -3621.93  1152.33
# ---
#   n = 1192, k = 5
# residual sd = 17253.96, R-Squared = 0.22

#z.height representa la diferencia en ganancias entre una persona 
#con altura 1 desviacion estandar mas alto que el promedio y una persona
#promedio asumiendo que es un hombre y no tiene educacion
#sexfemale es el efecto en ganancias entre hombre y mujer asumiendo que
#no tienen educacion y tienen altura promedio
#ed es el efecto de tener un nivel de educacion mas, asumiendo hombres
#de altura promedio 
#sexfemale:ed es la diferencia de efectos entre hombre y mujer 
#al tener un nivel de educacion mas. Es decir, un hombre recibe en promedio
#ed mas de ganancia por tener un nivel mas de educacion. Mientras que una 
#mujer recibe ed + sexfemale:ed mas de ganancias por tener un nivel mas de 
#educacion. Asumiendo altura promedio
#intercept: representa la ganancia promedio de un hombre con altura proemdio
# y sin educacion