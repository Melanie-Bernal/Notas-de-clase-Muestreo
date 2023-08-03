rm(list=ls()) 

library(xlsx)
library(readxl)
library(survey)
library(dplyr)
library(sampling)
library(srvyr)
library(pps)

base::source("Funciones.R")

base=readRDS(file = "Marco.rds")

#Marco muestral de los municipios
municipios=base%>%group_by(COLE_COD_MCPIO_UBICACION,COLE_MCPIO_UBICACION)%>%
  summarise(x=n(),y=mean(PUNT_GLOBAL))
#Probabilidades de selección
municipios$pk=municipios$x/sum(municipios$x)

#Selección y estimación
m=50
set.seed(123)
s<-ppswr(municipios$x,m)
muestra_mun=municipios[s,]
muestra_mun=muestra_mun[order(-muestra_mun$x),]

#Especificación del diseño
dsgn_mun=svydesign(id=~1,data=muestra_mun,probs=~pk)

#Estimación de la media
(est=svymean(~y,dsgn_mun,deff=T))
alpha=0.05
(tabla=salida(est,alpha))
