##########################
#### Ejercicios cap 4 ####
##########################

## 1) # en pag 62 tira la posta para este ejercicio
# no entendi por que es negativo en la que tira ian y en
#  -(exp(0.25)) = -1.28 and exp(0.25) = 1.28

## 2) log(weight) = −3.5+2.0 log(height) + error
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


