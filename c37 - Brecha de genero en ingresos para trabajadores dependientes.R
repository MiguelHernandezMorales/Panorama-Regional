##############################################################################
# Cuadro 37. Brecha de género en ingresos para trabajadores dependientes,
# ingresos de los dependientes según sexo y nivel educacional de Ñuble, 2015.
##############################################################################

ingreso_educ = svyby(~ingreso_hora, design = subset(diseño, provincia==84),
                     by=~nivel_educ+sexo, svymean,na.rm.all = TRUE, na.rm=TRUE) %>% 
  mutate(cv = se/ingreso_hora, ingreso_hora=round(ingreso_hora,0))

temp = xtabs(~nivel_educ+sexo, data=subset(diseño$variables, provincia==84)) %>% data.frame() 

ingreso_educ$frecuencia = temp$Freq


ingreso_educ = svyby(~ingreso_hora, design = subset(diseño, provincia==84),
                     by=~sexo, svymean,na.rm.all = TRUE, na.rm=TRUE)
#################################################
# Contrastes de Hipótesis 
#################################################

niveles = names(table(diseño$variables$nivel_educ))

options(survey.lonely.psu = "certainty")
diseños = lapply(niveles,function(x) svydesign(id = ~varunit,
                                               strata = ~varstrat, weights = ~expr, nest = TRUE,
                                               data = subset(diseño$variables, provincia==84 & nivel_educ==x)))

contraste_ = list()
for (i in 1:length(diseños)){
  contraste_[[i]] = svyttest(ingreso_hora~sexo, diseños[[i]])
}

