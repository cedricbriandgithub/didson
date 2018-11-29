# script chargement tau delta
# 
# Author: Clarisse et CÈdric
############################################################################

rm(list=ls(all=TRUE)) # nettoyage complet
getUsername <- function(){
	name <- Sys.info()[["user"]]
	return(name)
}
require(stringr)
set.seed(1235)
if(getUsername() == 'cedric.briand') setwd("F:/workspace/p/didson/bayesien")
datawd<-"F:/workspace/pdata/didson/bayesien/"
load(str_c(datawd,"donnees/d3ejbm.Rdata"))
load(str_c(datawd,"donnees/d3ejb.Rdata"))
load(str_c(datawd,"donnees/dj.Rdata"))
load(str_c(datawd,"donnees/dat_h.Rdata"))
#normalement il ne peut pas y avoir d'anguilles quand pdebit4=0
d3ejbm[d3ejbm$pdebit4==0&!is.na(d3ejbm$number),"number"]<-NA
# pour prendre un subset
d3ejbm<-d3ejbm[d3ejbm$jour%in%c(1:50),]
d3ejb<-d3ejb[d3ejb$jour%in%c(1:50),]
##################################
# VARIABLES TIREES DE d3ejbm
##################################
n_i=nrow(d3ejbm)
n_jour = max(d3ejbm$jour)
n_tranche = nrow(d3ejb)
pdebit4 = d3ejbm$pdebit4
jour = d3ejbm$jour
horaire = d3ejbm$hourm
# p_surface represente pour chaque petit polygone decoupee sur le faisceau la proportion de surface
#echantillonnee par le didson
# il y a des valeurs manquantes qui sont remises.
# somme des surfaces

p_surface<-d3ejbm$surface/d3ejbm$area_migration_frame
p_surface[is.na(p_surface)]<-0
tranche<-d3ejbm$tranche
cdt<-match(d3ejbm$tau,unique(d3ejbm$tau))
cdd<-match(d3ejbm$delta,unique(d3ejbm$delta))


##################################
# PROPORTION HORAIRE DES ANGUILLES PRIOR
##################################
# on prend la moyenne des effectifs de nuit
hh<-dat_h[c(as.character(18:23),as.character(0:9)),1]
p_dirichlet_t <- numeric(32)
# on applique les effectifs horaires ‡ chaque demie-heure
for (i in 1:16){
	p_dirichlet_t[i*2-1]<-hh[i]
	p_dirichlet_t[i*2]<-hh[i]
}


#######################################################
# STRUCTURE EN TAILLE 
#  "(0,45]"   "(45,60]"  "(60,80]"  "(80,150]"
# TODO : proportion des tailles mois <decembre decembre >decembre ( a tester)
# pour l'instant proportion constante
# voir essai_prior_efficacite.R pour param√®trage inital
######################################"
p_dirichlet_tau <- c(280,516,753,335)
n_tau<-4
n_delta<-5
N04prim_t<-d3ejbm$number

################""
# inits
#############"
# RETROCALCUL DES NOMBRES
#require(dplyr)
n_jour_init<-matrix(1000,nrow=n_jour,ncol=n_tau)
#n_jour_init[30,]<-100000
# max(d3ejbm$number,na.rm=TRUE)

N4_t_init<-matrix(100,nrow=n_tranche,ncol=n_tau)
mm<-matrix(rep(pdebit4,n_tau),nrow=n_tranche,ncol=n_tau,byrow=TRUE)
mm[mm==0]<-1
N_t_init<-round(N4_t_init/mm)
efficacite_init<-matrix(0.8,nrow=n_tau,ncol=n_delta)

