# TODO: Add comment
# 
# Author: cedric.briand
###############################################################################

require(gtools)

x11()
# efficacite
vdelta<-(1:6-mean(1:6))
vdelta<-vdelta/max(vdelta)
vgamma<-(1:4-mean(1:4))
vgamma<-vgamma/max(vgamma)

a<-0
b<--1.5
c<-+1	
efficacite<-matrix(NA,nrow=4,ncol=6)
for (tau in 1:(n_tau-1)){	
	for (delta in 1:n_delta){ 
		efficacite[tau,delta]<-inv.logit(a[1]+b[1]*vdelta[delta]+c[1]*vtau[tau])
	}
}
# Pour les plus grandes
efficacite[4,1]<-1
for (delta in 2:n_delta){ 
	efficacite[4,delta]<-inv.logit(a[1]+b[1]*vdelta[delta]+c[1]*vtau[4])
	
}



# effectif
mu_gamma <- 100000
#mu_gamma <- runif(1000,1000,100000)
sigma_gamma <- rgamma(1000,shape=1,rate=0.00001)
gamma_a <- mu_gamma^2 / sigma_gamma^2
gamma_b <- mu_gamma / sigma_gamma^2	
N_jour<-vector()
N_t_continu<-vector()
N4_t_continu<-vector()
N4_t<-vector()
p_surface[rowSums(p_surface)==0,7]<-1
surface_tau<-matrix(NA,nrow=n_tranche,ncol=25)
N4_t_tau<-matrix(NA,nrow=n_tranche,ncol=25)
#for (i in 1:n_jour){
#	N_jour[i] <- rgamma(1,shape=gamma_a[i], rate=gamma_b[i]) # nombre d'anguilles du jour
#}
N_jour<-init_N_jour
hist(N_jour)
p_t<-rdirichlet(1, p_dirichlet_t)
p_tau<-rdirichlet(1,p_dirichlet_tau)
N_t_continu <- N_jour[jour] * p_t[horaire] #nombre arrivant au droit du barrage a chaque 30 minutes
hist(N_t_continu,100)
N4_t_continu<- N_t_continu * pdebit4 # nombre se presentant a la vanne 4
N4_t <- mapply(rpois,n=1,lambda=N4_t_continu) # discretisation et tirage processus aléatoire
	for (tau in 1:n_tau){	
		for (delta in 1:n_delta){ 
			surface_tau[,(tau-1)*(n_delta)+delta]<-p_tau[tau]*p_surface[,delta]
		}
	}			
	surface_tau[,25]<-p_surface[,7] # surface restante non analysée toutes tailles considérées
	
	#N4_t_tau[,1:25]<-mapply(rmultinom,n=1,prob=surface_tau[,1:25],size=N4_t) # nombre d'anguilles par classe de taille matrice (i*4)			
	for (i in 1:n_tranche){
		N4_t_tau[i,1:25]<-t(rmultinom(n=1,prob=surface_tau[i,1:25],size=N4_t[i]))
	}
	matplot(N4_t_tau[,1:24],cex=0.6)
	for (tau in 1:n_tau){			     
		for (delta in 1:n_delta){ 
			N04prim_t_tau_delta[,(tau-1)*n_delta+delta] <-mapply(rbinom,n=1,prob=efficacite[tau,delta],size= N4_t_tau[,(tau-1)*n_delta+delta]) # je ne rentre pas dans la 25 ème
			
			#N04prim_t_tau_delta[t,(tau-1)*n_delta+delta] ~ dbin(efficacite[tau,delta], N04_t_tau_delta[t,(tau-1)*n_delta+delta])	
		}# delta
	}# tau	
	matplot(N04prim_t_tau_delta[,1:24],cex=0.6)
	hist(N04prim_t_tau_delta,100)


