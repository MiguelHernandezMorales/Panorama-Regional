######################################################################
# Creación de directorio de variables y match de las bases de datos 
######################################################################

t = proc.time() # Inicia el cronómetro

rm(list=ls())

library(foreign)
library(dplyr)

setwd("/home/hector/GoogleDrivePersonal/Observatorio Regional/Bases NENE/NENE/BasesCASEN/")

list.files()

casen2015 = read.spss("Casen 2015 SPSS.sav",use.value.labels = FALSE,
                      to.data.frame= FALSE, use.missings = TRUE)

etiquetas.casen = attr(casen2015, "label.table")
directorio.casen = attr(casen2015,"variable.labels")
directorio.casen = data.frame(names(directorio.casen),directorio.casen)

casen2015 = data.frame(casen2015)


library(survey)

diseño = svydesign(id = ~varunit, strata = ~varstrat,weights = ~expr, nest = TRUE, data = casen2015)


