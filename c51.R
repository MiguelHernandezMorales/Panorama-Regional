##############################################################################################################
# Cuadro 51. Ingresos de los trabajadores dependientes de la región según situación de discapacidad y nivel educacional
##############################################################################################################

diseño$variables$dependiente = recode_factor(diseño$variables$o15, `3` = 11, `4`=11,
                                             `5`=11, `6`=11, `7`=11)

diseño$variables = mutate(diseño$variables, nivel_educ = ifelse(educ==0 | educ==1,
                              "Básica incompleta o menor",
                              ifelse(educ==2, "Básica completa", 
                              ifelse(educ==3 | educ==4, "Básica completa", 
                              ifelse(educ==5 | educ==6, "Media completa", 
                              ifelse(educ==7, "Media completa", 
                              ifelse(educ==8, "Técnico Nivel Superior Completo", 
                              ifelse(educ==9, "Media completa", 
                              ifelse(educ==10, "Profesional Completo", 
                              ifelse(educ==11, "Profesional Completo", 
                              ifelse(educ==12, "Profesional Completo", NA)))))))))))
                              

diseño$variables =mutate(diseño$variables, discapacitado = ifelse(s34_1a!=1 |
                                            s34_1b!=1 |s34_1c!=1 |s34_1d!=1 |
                                            s34_1e!=1 |s34_1f!=1 |s34_1g!=1 |
                                            s34_1h!=1 |s34_1i!=1 |
                                            s34_1j!=1,1, 0))


ing_promedio = svyby(~yoprCor, by=~I(discapacitado==1)+I(dependiente==11)+nivel_educ,
                     subset(diseño, provincia==84), svymean, na.rm=TRUE,
                     na.rm.all=TRUE, multicore=TRUE, drop.empty.groups = FALSE) %>% 
  `colnames<-` (c("discapacitado", "dependiente", "educación", "yoprCor", "se")) %>% 
  mutate(cv = se/yoprCor) %>% filter(dependiente==TRUE)



numero = svyby(~I(dependiente==11), by=~I(discapacitado==1)+nivel_educ,
               subset(diseño, provincia==84),svytotal, 
               na.rm=TRUE, na.rm.all=TRUE, multicore=TRUE, drop.empty.groups = FALSE) %>% 
  `colnames<-` (c("indigena", "educación", "dependiente_false", "dependiente_true", 
                  "se_dependiente", "se_no")) %>% 
  select(indigena, educación, dependiente_true, se_dependiente) %>% 
  mutate(cv = se_dependiente/dependiente_true) 

freq = xtabs(~discapacitado+I(provincia==84)+I(dependiente==11)+nivel_educ, data=diseño$variables) %>% data.frame() %>% 
  `colnames<-` (c("indigena", "nuble", "dependiente", "educación", "Freq")) %>% filter(nuble==TRUE, dependiente==TRUE) 

ing_promedio$freq = freq$Freq

ing_promedio$frecuencia = freq$frecuencia

ing_promedio$confiable = ifelse(ing_promedio$cv>=0.25 | ing_promedio$freq<=50, "No confiable", "Confiable")

###############################################################
## Contraste de medias
###############################################################
niveles = names(table(diseño$variables$nivel_educ))[c(2,1,3,4,5,6)]
options(survey.lonely.psu = "certainty")
diseños = lapply(niveles,function(x) svydesign(id = ~varunit,
           strata = ~varstrat, weights = ~expr, nest = TRUE,
           data = subset(diseño$variables, provincia==84 & educ2==x)))

for (i in 1:length(diseños)){
  diseños[[i]]$variables$indigena = ifelse(diseños[[i]]$variables$r3!=10 & diseños[[i]]$variables$r3!=99, "Indigena",
                                           ifelse(diseños[[i]]$variables$r3==10, "No indigena",NA))
}

contraste_ = list()
for (i in 1:length(diseños)){
  contraste_[[i]] = svyttest(yoprCor~indigena,
                             diseños[[i]])
}

numero = svyby(~I(indigena==1), by=~I(provincia==84)+I(dependiente==11)+educ2, diseño, svytotal, 
               na.rm=TRUE, na.rm.all=TRUE, multicore=TRUE, drop.empty.groups = FALSE) %>% 
  `colnames<-` (c("nuble", "dependiente", "educación", "no indigena", "indigena", "se_FALSE", "se_TRUE")) %>% 
  mutate(cv_FALSE = se_FALSE/`no indigena`, cv_TRUE = se_TRUE/`indigena`) %>%
  filter(nuble==TRUE, dependiente==TRUE)

freq = xtabs(~indigena+I(provincia==84)+I(dependiente==11)+educ2, data=diseño$variables) %>%
  data.frame() %>% 
  `colnames<-` (c("indigena", "nuble", "dependiente", "educación", "Freq")) %>%
  filter(nuble==TRUE, dependiente==TRUE, indigena==1) 

freq1 = xtabs(~indigena+I(provincia==84)+I(dependiente==11)+educ2, data=diseño$variables) %>%
  data.frame() %>% 
  `colnames<-` (c("indigena", "nuble", "dependiente", "educación", "Freq")) %>%
  filter(nuble==TRUE, dependiente==TRUE, indigena==0) 

numero$freq_TRUE = freq$Freq
numero$freq_FALSE = freq1$Freq

numero$frecuencia = freq$frecuencia
