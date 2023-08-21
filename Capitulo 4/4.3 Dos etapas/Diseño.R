rm(list=ls()) 

library(xlsx)
library(readxl)
library(survey)
library(dplyr)
library(sampling)
library(srvyr)

base::source("Funciones.R")
base=readRDS(file = "Marco.rds")

#Primera etapa selección de municipios por pipt
marco=base%>%group_by(COLE_COD_MCPIO_UBICACION,COLE_MCPIO_UBICACION,COLE_AREA_UBICACION,
                      COLE_COD_DANE_ESTABLECIMIENTO)%>%
  summarise(x=n(),y=mean(PUNT_GLOBAL))

#Primera etapa
marcoI=marco%>%group_by(COLE_COD_MCPIO_UBICACION,COLE_MCPIO_UBICACION)%>%
  summarise(x=sum(x),y=mean(y))


nI=60
marcoI$pkI=inclusionprobabilities(marcoI$x,nI)
set.seed(123)
s<-UPopips(marcoI$pkI,type="pareto")
muestra_mun=marcoI[s,]
muestra_mun=muestra_mun[order(-muestra_mun$x),]
muestra_mun$s=1

#segunda etapa selección de conglomerados de colegios por EST-MAS 
#estrato: área urbana o rural

#marco colegios en los municipios seleccionados
marcoII=merge(marco,muestra_mun[,c(1,5,6)],by="COLE_COD_MCPIO_UBICACION",all.x=T)
marcoII=subset(marcoII,marcoII$s==1)
table(marcoII$COLE_AREA_UBICACION)
marcoII$estrato=paste(marcoII$COLE_COD_MCPIO_UBICACION,marcoII$COLE_AREA_UBICACION)
t=as.data.frame(table(marcoII$estrato))
names(t)[c(1,2)]=c("estrato","Nh")
marcoII=merge(marcoII,t[,c(1:2)],all.x = T)

nII=400 
#tamaño de muestra por estrato
t$n=round((nII-118)*t$Nh/sum(t$Nh)+1,0)

marcoII=marcoII[order(marcoII$estrato),]
s1=strata(marcoII, c("estrato"), t$n, method="srswor")

m=marcoII[s1$ID_unit,]
m$pikII=s1$Prob
m$Fexp=1/(m$pkI*m$pikII)
sum(m$Fexp)
m$NI=1113

#Especificación del diseño
options(survey.lonely.psu = "adjust")
dsgn=svydesign(id=~COLE_COD_MCPIO_UBICACION+COLE_COD_DANE_ESTABLECIMIENTO,
               data=m, strata=~estrato ,fpc=~NI+Nh, nest=TRUE,weights=~Fexp)

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
