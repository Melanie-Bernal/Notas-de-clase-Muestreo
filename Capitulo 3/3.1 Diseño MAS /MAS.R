rm(list=ls()) 

library(xlsx)
library(readxl)
library(survey)
library(dplyr)
library(sampling)
library(srvyr)

setwd("../Notas muestreo")
base::source("Funciones.R")

base_bogota=readRDS(file = "Marco Bogota.RDS")

# 1. tamaño de muestra
tipo="p"
N=nrow(base_bogota)
alpha=0.05
p=0.5
e=0.05
n.mas(tipo,N,s,e,p,alpha)

# 2. Selección de la muestra
n=382
seed=123
#Coordinado negativo
muestra=s.mas(base_bogota,n,seed)
#Selección y rechazo
m=Fan_Muller(base_bogota,n,seed)
muestra2=base_bogota[which(m==1),]

# 3. Estimación
N=nrow(base_bogota)
muestra$ind=rep(1,nrow(muestra))
muestra$Fexp=1/muestra$pik

#Especificación del diseño
dsgn=svydesign(id=~1,fpc=~rep(N,n),data=muestra,weights=~Fexp)

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

#Estimación tamaño de un dominio  
(est=svyby(~ind,~COLE_NATURALEZA,dsgn,svytotal))
est=as.data.frame(est)
est=est[,-1]
alpha=0.05
(tabla=salida(est,alpha))

#Estimación de una variable en dominios 
#Estimación del total
(est=svyby(~academico,~COLE_NATURALEZA,dsgn,deff=T,svytotal))
est=as.data.frame(est)
est=est[,-1]
alpha=0.05
(tabla=salida(est,alpha))
#Estimación de la media
(est=svyby(~PUNT_GLOBAL,~COLE_NATURALEZA,dsgn,svymean))
est=as.data.frame(est)
est=est[,-1]
alpha=0.05
(tabla=salida(est,alpha))

#Estimación de proporción
est=svyby(~DESEMP_INGLES,~COLE_NATURALEZA,dsgn,svymean)
est=as.data.frame(est)
est=est[,-1]
(tabla=salida.p(est))
