rm(list=ls()) 

library(xlsx)
library(readxl)
library(survey)
library(dplyr)
library(sampling)
library(srvyr)

base::source("Funciones.R")
base=readRDS(file = "Marco.rds")

#Marco de colegios
colegios=base%>%group_by(COLE_AREA_UBICACION,COLE_COD_MCPIO_UBICACION,COLE_COD_DANE_ESTABLECIMIENTO)%>%
  summarise(x=n(),y=mean(PUNT_GLOBAL))
table(colegios$COLE_AREA_UBICACION)
#ordenamos el marco por estrato
colegios=colegios[order(colegios$COLE_AREA_UBICACION),]

#Tamaño de muestra
n=500

#Asignación Óptima - Neyman 
(sy=colegios%>%group_by(COLE_AREA_UBICACION)%>%summarise(sy=sd(x)))
(ne1=round(n*nrow(colegios)*sy$sy/sum(nrow(colegios)*sy$sy),0))
sy$neyman=ne1
#Asignación proporcional
(Ne=colegios%>%group_by(COLE_AREA_UBICACION)%>%summarise(Ne=n()))
(ne2=round(n*Ne$Ne/nrow(colegios),0))
sy$Ne=Ne$Ne
sy$prop=ne2
#Asignación proporcional al total
(te=colegios%>%group_by(COLE_AREA_UBICACION)%>%summarise(te=sum(x)))
(ne3=round(n*te$te/sum(te$te),0))
sy$te=te$te
sy$stotal=ne3
sy

# Diseño EST-MAS
# Estratificamos por Zona (urbana y rural)
colegios$Ne=ifelse(colegios$COLE_AREA_UBICACION=="RURAL",3375,6935)

set.seed(123)
s1=strata(colegios, c("COLE_AREA_UBICACION"), ne1, method="srswor")
s2=strata(colegios, c("COLE_AREA_UBICACION"), ne2, method="srswor")
s3=strata(colegios, c("COLE_AREA_UBICACION"), ne3, method="srswor")

m1=colegios[s1$ID_unit,]
m1$pik1=s1$Prob

#Especificación del diseño
dsgn=svydesign(id=~1,data=m1, strata=~COLE_AREA_UBICACION ,fpc=~Ne, weights=~1/pik1)

#Estimación de la media
(est=svymean(~y,dsgn))
alpha=0.05
(tabla=salida(est,alpha))

#estimación en estratos usando la función de dominios
(est=svyby(~y,~COLE_AREA_UBICACION,dsgn,svymean))
est=as.data.frame(est)
est=est[,-1]
alpha=0.05
(tabla=salida(est,alpha))

# Estratificamos por Caracter
#Marco de los colegios
colegios=base%>%group_by(COLE_CARACTER,COLE_COD_MCPIO_UBICACION,COLE_COD_DANE_ESTABLECIMIENTO)%>%
  summarise(x=n(),y=mean(PUNT_GLOBAL))
(t=as.data.frame(table(colegios$COLE_CARACTER)))
names(t)=c("COLE_CARACTER","Ne")
colegios=merge(colegios,t)

colegios=subset(colegios,colegios$COLE_CARACTER=="ACADÉMICO"|
                  colegios$COLE_CARACTER=="TÉCNICO"|
                  colegios$COLE_CARACTER=="TÉCNICO/ACADÉMICO")
#ordenamos el marco por estrato
colegios=colegios[order(colegios$COLE_CARACTER),]
colegios=colegios[!is.na(colegios$COLE_CARACTER),]
#Asignación de la muestra
n=400

#Asignación Óptima - Neyman 
(sy=colegios%>%group_by(COLE_CARACTER)%>%summarise(sy=sd(x)))
(ne1=round(n*nrow(colegios)*sy$sy/sum(nrow(colegios)*sy$sy),0))

s1=strata(colegios, c("COLE_CARACTER"), ne1, method="srswor")

m1=colegios[s1$ID_unit,]
m1$pik1=s1$Prob
#Especificación del diseño
dsgn=svydesign(id=~1,data=m1, strata=~COLE_CARACTER ,fpc=~Ne, weights=~1/pik1)

#Estimación de la media
(est=svymean(~y,dsgn))
alpha=0.05
(tabla=salida(est,alpha))

#estimación en estratos usando la función de dominios
(est=svyby(~y,~COLE_CARACTER,dsgn,svymean))
est=as.data.frame(est)
est=est[,-1]
alpha=0.05
(tabla=salida(est,alpha))

# Estratificamos por Jornada
#creamos el marco de los colegios
colegios=base%>%group_by(COLE_JORNADA,COLE_COD_MCPIO_UBICACION,COLE_COD_DANE_ESTABLECIMIENTO)%>%
  summarise(x=n(),y=mean(PUNT_GLOBAL))
(t=as.data.frame(table(colegios$COLE_JORNADA)))
names(t)=c("COLE_JORNADA","Ne")
colegios=merge(colegios,t)

#ordenamos el marco por estrato
colegios=colegios[order(colegios$COLE_JORNADA),]
#Asignación de la muestra
n=400

#Asignación Óptima - Neyman 
(sy=colegios%>%group_by(COLE_JORNADA)%>%summarise(sy=sd(x)))
(ne1=round(n*nrow(colegios)*sy$sy/sum(nrow(colegios)*sy$sy),0))

s1=strata(colegios, c("COLE_JORNADA"), ne1, method="srswor")

m1=colegios[s1$ID_unit,]
m1$pik1=s1$Prob
#Especificación del diseño
dsgn=svydesign(id=~1,data=m1, strata=~COLE_JORNADA ,fpc=~Ne, weights=~1/pik1)

#Estimación de la media
(est=svymean(~y,dsgn))
alpha=0.05
(tabla=salida(est,alpha))

#estimación en estratos usando la función de dominios
(est=svyby(~y,~COLE_JORNADA,dsgn,svymean))
est=as.data.frame(est)
est=est[,-1]
alpha=0.05
(tabla=salida(est,alpha))

### Estratificamos por Jornada y zona
#creamos el marco de los colegios
colegios=base%>%group_by(COLE_JORNADA,COLE_AREA_UBICACION,
                         COLE_COD_MCPIO_UBICACION,COLE_COD_DANE_ESTABLECIMIENTO)%>%
  summarise(x=n(),y=mean(PUNT_GLOBAL))
colegios$estrato=paste(colegios$COLE_JORNADA,colegios$COLE_AREA_UBICACION,sep="-")
(t=as.data.frame(table(colegios$estrato)))
names(t)=c("estrato","Ne")
colegios=merge(colegios,t)

#Tamaño de la muestra
n=c(rep(40,12))

s1=strata(colegios, c("COLE_JORNADA","COLE_AREA_UBICACION"), n, method="srswor")

m1=colegios[s1$ID_unit,]
m1$pik1=s1$Prob
#Especificación del diseño
dsgn=svydesign(id=~1,data=m1, strata=~estrato ,fpc=~Ne, weights=~1/pik1)

#Estimación de la media
(est=svymean(~y,dsgn))
alpha=0.05
(tabla=salida(est,alpha))

#estimación en estratos usando la función de dominios
(est=svyby(~y,~estrato,dsgn,svymean))
est=as.data.frame(est)
est=est[,-1]
alpha=0.05
(tabla=salida(est,alpha))
