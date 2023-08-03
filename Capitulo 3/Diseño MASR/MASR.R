rm(list=ls()) 

library(xlsx)
library(readxl)
library(survey)
library(dplyr)
library(sampling)
library(srvyr)

base::source("Funciones.R")

base_bogota=readRDS(file = "Marco Bogota.RDS")

tipo="p"
N=nrow(base_bogota)
alpha=0.05 
p=0.5
e=0.05
deff=2
n.mas(tipo,N,s,e,p,alpha)*deff

# 1. Selección de la muestra
r=764
seed=123
N=nrow(base_bogota)
muestra=s.masr(base_bogota,r,seed)

# 2. Estimación
muestra$Fexp=1/muestra$pik
#Especificación del diseño
dsgn=svydesign(id=~1,data=muestra,weights=~Fexp)

#Estimación del total
(est=svytotal(~academico,dsgn,deff=T))
alpha=0.05
(tabla=salida(est,alpha))

#Estimación de la media
(est=svymean(~PUNT_GLOBAL,dsgn,deff=T))
alpha=0.05
(tabla=salida(est,alpha))

#Estimación de proporción
(est=svymean(~DESEMP_INGLES,dsgn,deff=T))
alpha=0.05
(tabla=salida(est,alpha))

#Estimación mas de una variable
(est=svymean(~PUNT_GLOBAL+PUNT_LECTURA_CRITICA+PUNT_MATEMATICAS+PUNT_C_NATURALES+
               PUNT_SOCIALES_CIUDADANAS+PUNT_INGLES
             ,dsgn))
alpha=0.05
(tabla=salida(est,alpha))
