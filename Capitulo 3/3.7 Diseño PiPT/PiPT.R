rm(list=ls()) 

library(xlsx)
library(readxl)
library(survey)
library(dplyr)
library(sampling)
library(srvyr)

base::source("Funciones.R")
base=readRDS(file = "Marco.rds")

#Marco muestral de los municipios
municipios=base%>%group_by(COLE_COD_MCPIO_UBICACION,COLE_MCPIO_UBICACION)%>%
  summarise(x=n(),y=mean(PUNT_GLOBAL))

#Probabilidades de inclusión
n=50
municipios$pk=inclusionprobabilities(municipios$x,n)
set.seed(123)
s<-UPopips(municipios$pk,type="pareto")
muestra_mun=municipios[s,]
muestra_mun=muestra_mun[order(-muestra_mun$x),]

#Especificación del diseño
dsgn_mun=svydesign(id=~1,data=muestra_mun,fpc=~pk,pps=HR())

#Estimación de la media
(est=svymean(~y,dsgn_mun,deff=T))
alpha=0.05
(tabla=salida(est,alpha))
