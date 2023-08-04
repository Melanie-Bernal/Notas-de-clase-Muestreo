rm(list=ls()) 

library(xlsx)
library(readxl)
library(survey)
library(dplyr)
library(sampling)
library(srvyr)

base::source("Funciones.R")

base=readRDS(file = "Marco.rds")

detp=base%>%group_by(COLE_COD_DEPTO_UBICACION)%>%
  summarise(x=n(),y=mean(PUNT_GLOBAL))

#Tamaño de la muestra
tipo="p"
N=33
p=0.5
e=0.1
alpha=0.05
n.mas(tipo,N,s=NULL,e,p,alpha)
n=25

#Selección de la muestra
seed=123
muestra=s.mas(detp,n,seed)

#Especificación del diseño
dsgn=svydesign(id=~1,data=muestra ,fpc=~rep(N,nrow(muestra)), weights=~1/pik)

#Estimación de la media
(est=svymean(~y,dsgn))
alpha=0.05
(tabla=salida(est,alpha))
