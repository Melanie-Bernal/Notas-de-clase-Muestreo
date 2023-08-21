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
  summarise(x_est=n())

#Primera etapa
marcoI=marco%>%group_by(COLE_COD_MCPIO_UBICACION,COLE_MCPIO_UBICACION)%>%
  summarise(x=n())

nI=60
marcoI$pI=inclusionprobabilities(marcoI$x,nI)
#Selección pagina 61 sampling
set.seed(123)
s<-UPopips(marcoI$pI,type="pareto")
muestra_mun=marcoI[s,]
muestra_mun=muestra_mun[order(-muestra_mun$x),]
muestra_mun$s=1

#segunda etapa selección de colegios por EST-MAS 
#estrato: área urbana o rural

#marco colegios en los municipios seleccionados
marcoII=merge(marco,muestra_mun[,c(1,5)],by="COLE_COD_MCPIO_UBICACION",all.x=T)
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
m$pII=s1$Prob

#Tercera etapa selección de estudiantes por MAS en cada colegio
marcoIII=merge(base,m[,c(1,5:9)],by="COLE_COD_DANE_ESTABLECIMIENTO",all.x=T)
marcoIII=subset(marcoIII,marcoIII$s==1)

tabla=marcoIII%>%group_by(COLE_MCPIO_UBICACION)%>%summarise(minimo=min(x_est),
                                                            maximo=max(x_est),
                                                            media=mean(x_est))
n_min=1 #Numero minimo de muestra por colegio
n_est=1000
marcoIII$id_col=paste(marcoIII$COLE_COD_DANE_ESTABLECIMIENTO,marcoIII$COLE_NOMBRE_ESTABLECIMIENTO,sep="-")

tabla2=marcoIII%>%group_by(id_col)%>%
  summarise(nest=round(n_min+(n_est-(n_min*nrow(m)))*x_est/nrow(marcoIII),0))
tabla2=tabla2[!duplicated(tabla2$id_col),]

marcoIII=merge(marcoIII,tabla2,by="id_col")
marcoIII=marcoIII[order(marcoIII$id_col),]

s=strata(marcoIII, c("id_col"), tabla2$nest, method="srswor")
m=marcoIII[s$ID_unit,]
m$pIII=s$Prob
m=merge(m,marcoI[,c(1,4)],by="COLE_COD_MCPIO_UBICACION",all.x = T)

m$Fexp=1/(m$pI*m$pII*m$pIII)
sum(m$Fexp)
m$NI=1113

#Indicadoras para el estimador de razon
m$ind=1
m$rural=ifelse(m$COLE_AREA_UBICACION=="RURAL",1,0)
m$urbano=ifelse(m$COLE_AREA_UBICACION=="URBANO",1,0)

#Especificación del diseño
options(survey.lonely.psu = "adjust")
dsgn=svydesign(id=~COLE_COD_MCPIO_UBICACION+COLE_COD_DANE_ESTABLECIMIENTO+ESTU_CONSECUTIVO,
               data=m, strata=~COLE_AREA_UBICACION ,fpc=~NI+Nh+nest, nest=TRUE,weights=~Fexp)

#Estimación de la media
(est=svymean(~PUNT_GLOBAL,dsgn))
alpha=0.05
(tabla=salida(est,alpha))

#estimación en estratos usando la función de dominios
(est=svyby(~PUNT_GLOBAL,~COLE_AREA_UBICACION,dsgn,svymean))
est=as.data.frame(est)
est=est[,-1]
alpha=0.05
(tabla=salida(est,alpha))

#numero de estudiantes en el área rural / numero de estudiantes
sum(m$Fexp)
m%>%group_by(COLE_AREA_UBICACION)%>%summarise(est=sum(Fexp)/sum(m$Fexp))

svyratio(~rural,~ind,dsgn)
svyratio(~urbano,~ind,dsgn)

