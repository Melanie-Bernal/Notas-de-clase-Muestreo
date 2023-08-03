rm(list=ls()) 

library(xlsx)
library(readxl)
library(survey)
library(dplyr)
library(sampling)
library(srvyr)

base::source("Funciones.R")

base_bogota=readRDS(file = "Marco Bogota.RDS")

#1. Selección de la muestra
n=700
N=nrow(base_bogota)
seed=123
muestra=s.sis(base_bogota,n,seed)

# 2. Estimación
muestra$Fexp=1/muestra$pik
#Especificación del diseño
dsgn=svydesign(id=~1,data=muestra,weights=~Fexp,fpc=~rep(N,n))

#Estimación del total
#Survey
(est=svytotal(~academico,dsgn,deff=T))
#Aproximación por el MAS
f=n/N
total=sum(muestra$academico*muestra$Fexp)
se=sqrt(N^2*(1-f)/n*var(muestra$academico)*n/(n-1))
est=as.data.frame(est)
data.frame(metodo=c("Survey","Aproximación MAS"),t=c(est[1,1],total),se=c(est[1,2],se))

#Estimación de la media
#Survey
(est=svymean(~PUNT_GLOBAL,dsgn,deff=T))
#Aproximación por el MAS
media=sum(muestra$PUNT_GLOBAL*muestra$Fexp)/N
se=1/N*sqrt(N^2*(1-f)/n*var(muestra$PUNT_GLOBAL)*n/(n-1))
est=as.data.frame(est)
data.frame(metodo=c("Survey","Aproximación MAS"),media=c(est[1,1],media),se=c(est[1,2],se))

#Estimación de proporción
(est=svymean(~DESEMP_INGLES,dsgn))
#Aproximación por el MAS
variable="DESEMP_INGLES"
(est2=e.sis.prop(variable,muestra,N))

#Tabla de comparación
est=as.data.frame(est)
est$Categoria=row.names(est)
est$SE=round(est$SE,4)
names(est)=c("P_survey","se_survey","Categoria")
names(est2)[c(2,3)]=c("P_A.MAS","se_A.MAS")
merge(est,est2,by="Categoria")
