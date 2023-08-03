U=c(1:5)
y=c(60,65,67,72,77)
pi=0.7
N=length(U)
for(i in 0:N){
  muestras.i<-as.data.frame(t(combn(U,i)))
  muestras.i$ind.1<-rowSums(I(muestras.i==1))*y[1]
  muestras.i$ind.2<-rowSums(I(muestras.i==2))*y[2]
  muestras.i$ind.3<-rowSums(I(muestras.i==3))*y[3]
  muestras.i$ind.4<-rowSums(I(muestras.i==4))*y[4]
  muestras.i$ind.5<-rowSums(I(muestras.i==5))*y[5]
  muestras.i$nm<-rep(i,nrow(muestras.i))
  muestras.i$p<-rep(dbinom(i,N,pi)/nrow(muestras.i),nrow(muestras.i))
  muestras.i=muestras.i[,c("ind.1","ind.2","ind.3","ind.4","ind.5","nm","p")]
  ifelse(i==0,(muestras=muestras.i),(muestras=rbind(muestras,muestras.i)))
}

#suma de las probabilidades de selección
sum(muestras$p)

#Parametro
sum(y)

#Calculo de la esperanza del pi-estimador
muestras$sum=rowSums(muestras[,1:5])
muestras$t=1/pi*muestras$sum
sum(muestras$t*muestras$p)
