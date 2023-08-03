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
x=rnorm(nrow(base_bogota),10,1)
pi_k=n*x/sum(x)
muestra=s.pois(base_bogota,pi_k,seed)

# 2. Estimación
muestra$Fexp=1/muestra$pik
#Especificación del diseño
dsgn=svydesign(id=~1,data=muestra,weights=~Fexp,fpc=~rep(N,nrow(muestra)))

#Estimación del total
#Survey
(est=svytotal(~academico,dsgn,deff=T))
#Pi-estimador
total=sum(muestra$academico*muestra$Fexp)
se=sqrt(sum(1/muestra$pik*(1/muestra$pik-1)*muestra$academico^2))
#Estimador alternativo
t_a=N*sum(muestra$academico/muestra$pik)/sum(muestra$Fexp)
ym=total/sum(muestra$Fexp)
se_a=sqrt((N/sum(muestra$Fexp))^2*sum((1-muestra$pik)*(muestra$academico-ym)^2))
est=as.data.frame(est)
data.frame(metodo=c("Survey","t_pi","t.altenativo"),t=c(est[1,1],total,t_a),se=c(est[1,2],se,se_a))

#Estimación de la media
#Survey
(est=svymean(~PUNT_GLOBAL,dsgn,deff=T))
#Pi-estimador
media=sum(muestra$PUNT_GLOBAL*muestra$Fexp)/N
se=1/N*sqrt(sum(1/muestra$pik*(1/muestra$pik-1)*muestra$PUNT_GLOBAL^2))
#Estimador alternativo
t_a=sum(muestra$PUNT_GLOBAL/muestra$pik)/sum(muestra$Fexp)
ym=total/sum(muestra$Fexp)
se_a=1/N*sqrt((N/sum(muestra$Fexp))^2*sum((1-muestra$pik)*(muestra$PUNT_GLOBAL-ym)^2))
est=as.data.frame(est)
data.frame(metodo=c("Survey","t_pi","t.altenativo"),t=c(est[1,1],media,t_a),se=c(est[1,2],se,se_a))

#Estimación de proporción
(est=svymean(~DESEMP_INGLES,dsgn))
#Con la formula de la estimación de la varianza
variable="DESEMP_INGLES"
(est2=e.pois.prop(variable,muestra,N))
#con el estimador alternativo
(est3=e.pois.a.prop(variable,muestra,N))

#Tabla de comparación
est=as.data.frame(est)
est=round(est,4)
est$Categoria=row.names(est)
est$SE=round(est$SE,4)
names(est)=c("P_survey","se_survey","Categoria")
merge(est,cbind(est2,est3[c(2,3)]),by="Categoria")
