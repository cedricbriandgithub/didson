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
if(getUsername() == 'cedric.briand') setwd("C:/Users/cedric.briand/Documents/GitHub/didson/") 
if(getUsername() == 'cboulenger') setwd("C:/Users/cedric.briand/Documents/GitHub/didson/") 

source("script/fonctions.R")
if(getUsername() == 'cedric.briand') datawd<-"C:/Users/cedric.briand/Documents/GitHub/didson/data/" 
if(getUsername() == 'cboulenger')    datawd<-"C:/Users/cedric.briand/Documents/GitHub/didson/data/" 

load(str_c(datawd,"d3ejb.Rdata"))
load(str_c(datawd,"dj.Rdata"))
load(str_c(datawd,"dat_h.Rdata"))
load(str_c(datawd,"d3ejbmtau.Rdata"))


#normalement il ne peut pas y avoir d'anguilles quand pdebit4=0
d3ejbmtau[d3ejbmtau$pdebit4==0&!is.na(d3ejbmtau$number),"number"]<-NA
# pour prendre un subset
#d3ejbmtau<-d3ejbmtau[d3ejbmtau$jour%in%c(1:20),]
#d3ejb<-d3ejb[d3ejb$jour%in%c(1:20),]
##################################
# VARIABLES TIREES DE d3ejbmtau
##################################
n_i=nrow(d3ejbmtau)
n_jour = max(d3ejbmtau$jour)
n_tranche = nrow(d3ejb)
pdebit4 = d3ejbmtau$pdebit4
jour = d3ejbmtau$jour
horaire = d3ejbmtau$hourm
# p_surface represente pour chaque petit polygone decoupee sur le faisceau la proportion de surface
#echantillonnee par le didson
# il y a des valeurs manquantes qui sont remises.
# somme des surfaces

p_surface<-d3ejbmtau$area_intersect/d3ejbmtau$area_migration_frame
p_surface[is.na(p_surface)]<-0
tranche<-d3ejbmtau$tranche
cdt<-match(d3ejbmtau$tau,unique(d3ejbmtau$tau))
cdd<-match(d3ejbmtau$delta,unique(d3ejbmtau$delta))


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
N04prim_t<-d3ejbmtau$number

################""
# inits
#############"
# RETROCALCUL DES NOMBRES
#require(dplyr)
n_jour_init<-matrix(1000,nrow=n_jour,ncol=n_tau)
#n_jour_init[30,]<-100000
# max(d3ejbmtau$number,na.rm=TRUE)

N4_t_init<-matrix(100,nrow=n_tranche,ncol=n_tau)
mm<-matrix(rep(pdebit4,n_tau),nrow=n_tranche,ncol=n_tau,byrow=TRUE)
mm[mm==0]<-1
N_t_init<-round(N4_t_init/mm)
efficacite_init<-matrix(0.8,nrow=n_tau,ncol=n_delta)

