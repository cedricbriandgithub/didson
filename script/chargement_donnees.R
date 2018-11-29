# Analyse bay√©sienne des donn√©es DIDSON de la Vilaine
# Chargement des donn√©es
# 
# Author: Beaulaton Laurent (Onema), Briand C√©dric (IAV)
###############################################################################

rm(list=ls(all=TRUE)) # nettoyage complet
getUsername <- function(){
	name <- Sys.info()[["user"]]
	return(name)
}
require(stringr)
set.seed(1235)
if(getUsername() == 'cedric.briand') setwd("C:/Users/cedric.briand/Documents/GitHub/didson/") 
source("script/fonctions.R")
datawd<-"C:/Users/cedric.briand/Documents/GitHub/didson/data/" 
load(str_c(datawd,"d3ejb.Rdata"))
load(str_c(datawd,"dj.Rdata"))
load(str_c(datawd,"dat_h.Rdata"))

##################################
# VARIABLES TIREES DE d3ejb
##################################

n_jour = max(d3ejb$jour)
n_tranche = nrow(d3ejb)
pdebit4 = d3ejb$pdebit4
jour = d3ejb$jour
horaire = d3ejb$hourm
# p_surface represente pour chaque petit polygone decoupee sur le faisceau la proportion de surface
#echantillonnee par le didson
# il y a des valeurs manquantes qui sont remises.
surface = d3ejb[,c("(2,5]","(5,7]","(7,9]","(9,11]","(11,13]","(13,15]","area_migration_frame")] 
# somme des surfaces
surface[,7]<-surface[,7]-rowSums(surface[,c(1:6)])
p_surface<-surface/d3ejb$area_migration_frame
p_surface<-as.matrix(p_surface)
p_surface[is.na(p_surface)]<-0
p_surface[is.na(d3ejb$area_migration_frame)&d3ejb$pdebit4>0,7]<-1 # passage par la vanne fen√™tres de visu =0 
#################""
# simplification
###############
surface2<-cbind(d3ejb$area_intersect,d3ejb$area_migration_frame)
p_surface2<-surface2[,1]/surface2[,2]
p_surface2[is.na(p_surface2)]<-0
#p_surface[,]<-1
#efficacite<-matrix(1,nrow=4,ncol=6)
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

#for(t in 1:32)
#{
#	p_dirichlet_t[t] <- 1
#}
#######################################################
# STRUCTURE EN TAILLE 
#  "(0,45]"   "(45,60]"  "(60,80]"  "(80,150]"
# TODO : proportion des tailles mois <decembre decembre >decembre ( a tester)
# pour l'instant proportion constante
# voir essai_prior_efficacite.R pour param√®trage inital
######################################"
p_dirichlet_tau <- c(280,516,753,335)


#vtau<-0:3
#vtau<-vtau/max(vtau)
#n_tau<-4
vtau<-(1:4-mean(1:4))
vtau<-vtau/max(vtau)
n_tau<-4
#######################################################
# STRUCTURE DE DISTANCE AU DIDSON
# "(2,5]"   "(5,7]"   "(7,9]"   "(9,11]"  "(11,13]" "(13,15]"
######################################"
vdelta<-(1:6-mean(1:6))
vdelta<-vdelta/max(vdelta)
n_delta<-6
#vdelta<-0:5
#vdelta<-vdelta/max(vdelta)


#######################################################
# MATRICES DES EFFECTIFS
######################################################
# je cherche les effectifs qui apparaissent comme des parenthËses dans d3ejb
N04prim_t_tau_delta = d3ejb[,grep("]\\(",colnames(d3ejb))]
#sum(N04prim_t_tau_delta,na.rm=TRUE) # 1885 ??? plus que 1880 sum(N4obs ?)=> 1743
#sum(N04prim_t_tau_delta[!d3ejb$indexOK,],na.rm=TRUE) # 142 eels
N04prim_t_tau_delta[!d3ejb$indexOK,]<-NA
#N04prim_t_tau_delta[pdebit4==0,]<-NA
N04prim_t_tau_delta<-as.matrix(N04prim_t_tau_delta)

load(file=str_c("F:/workspace/pdata/didson/rapport/data/2014/d3ej.Rdata"))
d3ej$Nprimo4<-rowSums(d3ej[,c("drr_eelplus","drr_eelminus")],na.rm=TRUE)
indexOK<-d3ej$dsf_fls_id==0&d3ej$ex==1 # effectifs utilisÈs pour extrapolation plus loin
indexOK[is.na(indexOK)]<-FALSE
d3ej$indexOK<-indexOK
d3ej$Nprimo4[!indexOK]<-NA
# Essai Je repasse √† des sommes en ligne et colonnes pour ajustement du mod√®le
N04prim_tau_deltasum<-colSums(N04prim_t_tau_delta,na.rm=TRUE)
N04prim_t_sum<-rowSums(N04prim_t_tau_delta,na.rm=TRUE)
N04prim_t_sum[apply(N04prim_t_tau_delta,1,function(X)all(is.na(X)))]<-NA

#N4_t_tau<-N04prim_t_tau_delta
#N4_t_tau<-cbind(N4_t_tau,N4_t_tau[,1])
#rm(N04prim_t_tau_delta)
#######################################################
# TODO SURFACE ET FOND
######################################################
#surface=d3ejb$position=="s"
#fond=!surface
#N4_obstaudeltafond<-N4_obstaudelta
#N4_obstaudeltafond[surface,]<-NA
#N4_obstaudeltasurface<-N4_obstaudelta
#N4_obstaudeltasurface[fond,]<-NA
#-----------------------------------
# Essais avec Clarisse
#N4_t_tau<-N04prim_t_tau_delta+1
#N4_t_tau[is.na(N4_t_tau)]<-0
#N4_t_tau_init<-cbind(N4_t_tau,N4_t_init-rowSums(N4_t_tau,na.rm=TRUE))
#N4_t_init=rowSums(N4_t_tau_init)+1
#-------------------------------------
#
init_N_jour<-round(abs(rowSums(cbind(dj[dj$type=="odots","N"],dj[dj$type=="otimes","N"],dj[dj$type=="oplus","N"]),na.rm=TRUE)[1:182]))
init_N_jour[init_N_jour==0]<-50



# RETROCALCUL DES NOMBRES
N4_t_init=d3ej$Nprimo4
N4_t_init=round(N04prim_t_sum/p_surface2)+1
N4_t_init[is.na(N4_t_init)]<-1 # les binomiales n'aiment pas les p=0 en init
# proportions thÈoriques par heure
#pp_dirichlet_t<-p_dirichlet_t/sum(p_dirichlet_t)
str(d3ejb)
N_t_init<-round(N4_t_init/pdebit4)+1
N_t_init[is.infinite(N_t_init)]<-1
init_N_jour<-round(tapply(N_t_init,d3ejb$jour,sum,na.rm=TRUE))+1  # *pp_dirichlet_t[horaire]
#problËmes de dÈbit
N04prim_t_sum[pdebit4==0&!is.na(N04prim_t_sum)]<-0
#pdebit4[pdebit4==0]<-0.001
#N04prim_t_tau_delta_init<-N04prim_t_tau_delta
#N04prim_t_tau_delta_init[!is.na(N04prim_t_tau_delta_init)]<-NA
#N04prim_t_tau_delta_init[is.na(N04prim_t_tau_delta_init)]<-1