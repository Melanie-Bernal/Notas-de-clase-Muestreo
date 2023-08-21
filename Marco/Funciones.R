###############################################################################
#                               Generales
###############################################################################

salida=function(est,alpha){
  est=as.data.frame(est)
  names(est)[2]="se"
  est$cv=100*(est$se/est[,1])
  est$ic_low=est[,1]-qnorm(1-alpha/2)*est$se 
  est$ic_upp=est[,1]+qnorm(1-alpha/2)*est$se
  return(round(est,2))
}

salida.p=function(est){
  for(i in 1:length(est)){
    a=as.data.frame(est[1,i])
    categoria=rownames(a)[1:(nrow(a)/2)]
    dominio=rep(rownames(est)[i],(nrow(a)/2))
    p=a[1:(nrow(a)/2),]
    se=a[(nrow(a)/2+1):nrow(a),]
    tab=data.frame(dominio,categoria,p,se)
    ifelse(i==1,(tab2=tab),(tab2=rbind(tab2,tab)))
  }
  return(tab2)
}
###############################################################################
#                               Diseño MAS
###############################################################################

n.mas=function(tipo,N,s,e,p,alpha){
  if(tipo=="t"){n=round((qnorm(1-alpha/2)^2*N^2*s^2)/(e^2+qnorm(1-alpha/2)^2*N*s^2),0)}
  if(tipo=="t"){return(n)}
  if(tipo=="m"){n=round((qnorm(1-alpha/2)^2*s^2)/(e^2+(qnorm(1-alpha/2)^2*s^2/N)),0)}
  if(tipo=="m"){return(n)}
  if(tipo=="p"){n=round((qnorm(1-alpha/2)^2*(N/(N-1))*p*(1-p))/(e^2+(qnorm(1-alpha/2)^2*(N/(N-1))*p*(1-p)*(1/N))),0)
  if(tipo=="p"){return(n)}
  }
}

Fan_Muller=function(base,n,seed){
  N=nrow(base)
  j = 0
  m = numeric(N)
  set.seed(seed)
  for (k in 1:N) if (runif(1) < (n - j)/(N - k + 1)) {
    j = j + 1
    m[k] = 1}
  return(m)
}

s.mas=function(base,n,seed){
  N=nrow(base)
  set.seed(seed)
  base$u=runif(nrow(base))
  base=base[with(base,order(base$u)),]
  base=base[1:n,]
  base$pik=n/N
  return(base)
}

###############################################################################
#                               Diseño MASCR
###############################################################################

s.masr=function(base,r,seed){
  N=nrow(base)
  set.seed(seed)
  base=base[sample(c(1:nrow(base_bogota)),r,replace = T),]
  base$pik=1-(1-1/N)^r
  return(base)
}

###############################################################################
#                               Diseño Sistematico
###############################################################################

s.sis=function(base,n,seed){
  N=nrow(base)
  a=round(N/n,0)
  set.seed(seed)
  u=round(runif(1,1,a),0)
  s=seq(u,u+a*(n-1),a) 
  base=base[s,]
  base$pik=1/a
  return(base)
}

e.sis.prop=function(variable,muestra,N){
  for(i in 1:length(table(muestra[,c(variable)]))){
    muestra$ind<-ifelse(muestra[,c(variable)]==unique(muestra[,c(variable)])[i],1,0)
    muestra.i=subset(muestra,muestra[,c(variable)]==unique(muestra[,c(variable)])[i])
    p.i=sum(muestra$ind*muestra$Fexp)/N
    se.i=1/N*sqrt(N^2*(1-f)/n*var(muestra$ind)*n/(n-1))
    tab.i=data.frame(Categoria=paste0(variable,unique(muestra[,c(variable)])[i]),p=round(p.i,4),se=round(se.i,4))
    ifelse(i==1,(tabla=tab.i),(tabla=rbind(tabla,tab.i)))
  }
  return(tabla)
}
###############################################################################
#                               Diseño Bernoulli
###############################################################################

s.ber=function(base,pi,seed){
  set.seed(seed)
  base$u=runif(N)
  base$m=ifelse(base$u<pi,1,0)
  base=subset(base,base$m==1)
  base$pik=rep(pi,nrow(base))
  return(base)
}

e.ber.prop=function(variable,muestra,N){
  for(i in 1:length(table(muestra[,c(variable)]))){
    muestra$ind<-ifelse(muestra[,c(variable)]==unique(muestra[,c(variable)])[i],1,0)
    muestra.i=subset(muestra,muestra[,c(variable)]==unique(muestra[,c(variable)])[i])
    p.i=sum(muestra$ind*muestra$Fexp)/N
    se.i=1/N*sqrt((1/pi)*((1/pi)-1)*sum(muestra$ind^2))
    tab.i=data.frame(Categoria=paste0(variable,unique(muestra[,c(variable)])[i]),p_ber=round(p.i,4),se_ber=round(se.i,4))
    ifelse(i==1,(tabla=tab.i),(tabla=rbind(tabla,tab.i)))
  }
  return(tabla)
}

e.ber.a.prop=function(variable,muestra,N){
  for(i in 1:length(table(muestra[,c(variable)]))){
    muestra$ind<-ifelse(muestra[,c(variable)]==unique(muestra[,c(variable)])[i],1,0)
    muestra.i=subset(muestra,muestra[,c(variable)]==unique(muestra[,c(variable)])[i])
    p.i=sum(muestra$ind)/nrow(muestra)
    se.i=1/N*sqrt(N^2*((1-pi)/nrow(muestra))*(1-(1/nrow(muestra)))*var(muestra$ind))
    tab.i=data.frame(Categoria=paste0(variable,unique(muestra[,c(variable)])[i]),p_ber.a=round(p.i,4),se_ber.a=round(se.i,4))
    ifelse(i==1,(tabla=tab.i),(tabla=rbind(tabla,tab.i)))
  }
  return(tabla)
}

###############################################################################
#                               Diseño Poisson
###############################################################################

s.pois=function(base,pi_k,seed){
  set.seed(seed)
  base$u=runif(N)
  base$pik=pi_k
  base$m=ifelse(base$u<base$pik,1,0)
  base=subset(base,base$m==1)
  return(base)
}

e.pois.prop=function(variable,muestra,N){
  for(i in 1:length(table(muestra[,c(variable)]))){
    muestra$ind<-ifelse(muestra[,c(variable)]==unique(muestra[,c(variable)])[i],1,0)
    muestra.i=subset(muestra,muestra[,c(variable)]==unique(muestra[,c(variable)])[i])
    p.i=sum(muestra$ind*muestra$Fexp)/N
    se.i=1/N*sqrt(sum(1/muestra$pik*(1/muestra$pik-1)*muestra$ind^2))
    tab.i=data.frame(Categoria=paste0(variable,unique(muestra[,c(variable)])[i]),p_ber=round(p.i,4),se_ber=round(se.i,4))
    ifelse(i==1,(tabla=tab.i),(tabla=rbind(tabla,tab.i)))
  }
  return(tabla)
}

e.pois.a.prop=function(variable,muestra,N){
  for(i in 1:length(table(muestra[,c(variable)]))){
    muestra$ind<-ifelse(muestra[,c(variable)]==unique(muestra[,c(variable)])[i],1,0)
    muestra.i=subset(muestra,muestra[,c(variable)]==unique(muestra[,c(variable)])[i])
    p.i=sum(muestra$ind*muestra$Fexp)/sum(muestra$Fexp)
    ym=sum(muestra$ind*muestra$Fexp)/sum(muestra$Fexp)
    se.i=1/N*sqrt((N/sum(muestra$Fexp))^2*sum((1-muestra$pik)*(muestra$ind-ym)^2))
    tab.i=data.frame(Categoria=paste0(variable,unique(muestra[,c(variable)])[i]),p_ber.a=round(p.i,4),se_ber.a=round(se.i,4))
    ifelse(i==1,(tabla=tab.i),(tabla=rbind(tabla,tab.i)))
  }
  return(tabla)
}

