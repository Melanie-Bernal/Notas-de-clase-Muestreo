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
n=2000
N=nrow(base_bogota)
seed=123
pi=n/N
muestra=s.ber(base_bogota,pi,seed)

# 2. Estimación
muestra$Fexp=1/muestra$pik
#Especificación del diseño
dsgn=svydesign(id=~1,data=muestra,weights=~Fexp,fpc=~rep(N,nrow(muestra)))

#Estimación del total
#Survey
(est=svytotal(~academico,dsgn,deff=T))
#Pi-estimador
total=sum(muestra$academico*muestra$Fexp)
se=sqrt((1/pi)*((1/pi)-1)*sum(muestra$academico^2))
#Estimador alternativo
t_a=N*sum(muestra$academico)/nrow(muestra)
se_a=sqrt(N^2*((1-pi)/nrow(muestra))*(1-(1/nrow(muestra)))*var(muestra$academico))
est=as.data.frame(est)
data.frame(metodo=c("Survey","t_pi","t.altenativo"),t=c(est[1,1],total,t_a),se=c(est[1,2],se,se_a))

#Estimación de la media
#Survey
(est=svymean(~PUNT_GLOBAL,dsgn,deff=T))
#Pi-estimador
media=sum(muestra$PUNT_GLOBAL*muestra$Fexp)/N
se=1/N*sqrt((1/pi)*((1/pi)-1)*sum(muestra$PUNT_GLOBAL^2))
#Estimador alternativo
m_a=sum(muestra$academico)/nrow(muestra)
se_a=1/N*sqrt(N^2*((1-pi)/nrow(muestra))*(1-(1/nrow(muestra)))*var(muestra$academico))
est=as.data.frame(est)
data.frame(metodo=c("Survey","t_pi","t.altenativo"),m=c(est[1,1],total,m_a),se=c(est[1,2],se,se_a))

#Estimación de proporción
(est=svymean(~DESEMP_INGLES,dsgn))
#Pi-estimador
variable="DESEMP_INGLES"
(est2=e.ber.prop(variable,muestra,N))
#El estimador alternativo
(est3=e.ber.a.prop(variable,muestra,N))

#Tabla de comparación
est=as.data.frame(est)
est=round(est,4)
est$Categoria=row.names(est)
est$SE=round(est$SE,4)
names(est)=c("P_survey","se_survey","Categoria")
merge(est,cbind(est2,est3[c(2,3)]),by="Categoria")
