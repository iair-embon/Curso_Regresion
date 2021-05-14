### lineas para normalizar variables y hacer regresion 

library(arm)

root <- rprojroot::is_rstudio_project
basename(getwd())

## adf_total:
filepath <- (root$find_file("Iair/d.sin.normalizar.solo.FyM.Rda"))
load(file= filepath)

save(d.sin.normalizar.solo.FyM,file = filepath)