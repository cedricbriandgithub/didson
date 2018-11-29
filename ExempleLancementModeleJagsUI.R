
library(jagsUI)

setwd("D:/Résultats modèles/3ProbaDetec/200/1pics5Mois/Aout")
for(i in 1:8){
  tryCatch({
    ### Modele 1
    source(paste("inits200pic1Pd35MAOcJAGS",i,".txt",sep=""))
    
    inits<-function(){list(zd=zd,zvc=zvc,zv=zv, E.d=E.d, CV=CV, N1=N1)}
    
    source(paste("donnees200pic1Pd35MAOcJAGS",i,".txt",sep=""))
    
    bugs.data<-list(K=K,m=m,Yvv=Yvv,Y2vd=Y2vd, Y1vs=Y1vs,Y2vs=Y2vs, Y1vd=Y1vd)
    Ypv<-list(Ypv=Ypv)
    
    parameters<-c("NstB","pd","pvc","pv","Ypv","Nst","E.d","CV","mu.E","sd.E")
    
    length(na.omit(Y1vd))
    sum(na.omit(Y1vd))
    sum(na.omit(Y1vs))
    sum(na.omit(Y2vs))
    sum(na.omit(unlist(Ypv)))
    
    ni<-300000
    nt<-1
    nb<-270000
    nc<-3
    
    
    out1<-jags(bugs.data,inits,parameters,"ModeleMelangeTouquesYvv.txt", n.chains=nc,n.thin=nt,n.iter=ni,n.burnin=nb)
    
    print(out1)
    
    names(out1)
    out1$parameters
    names<-row.names(out1$summary)
    mean<-unlist(out1$mean, recursive = TRUE, use.names = F)
    sd<-unlist(out1$sd, recursive = TRUE, use.names = FALSE)
    q2.5<-unlist(out1$q2.5, recursive = TRUE, use.names = FALSE)
    q25<-unlist(out1$q25, recursive = TRUE, use.names = FALSE)
    q50<-unlist(out1$q50, recursive = TRUE, use.names = FALSE)
    q75<-unlist(out1$q75, recursive = TRUE, use.names = FALSE)
    q97.5<-unlist(out1$q97.5, recursive = TRUE, use.names = FALSE)
    Rhat<-unlist(out1$Rhat, recursive = TRUE, use.names = FALSE)
    t<-data.frame(names, mean,sd,q2.5,q25,q50,q75,q97.5,Rhat)
    write.table(t,file=paste("resultats200pic1Pd33MAOc",i,".txt",sep=""))
    save(out1,file=paste("resultats200pic1Pd33MAOc",i,".Rdata"))
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}



