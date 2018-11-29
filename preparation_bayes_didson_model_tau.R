# script de préparation du jeu de données à partir des analyses précédentes 
# Author: cedric.briand
###############################################################################


rm(list=ls(all=TRUE)) # nettoyage complet
getUsername <- function(){
  name <- Sys.info()[["user"]]
  return(name)
}
load_library=function(necessary) {
	if(!all(necessary %in% installed.packages()[, 'Package']))
		install.packages(necessary[!necessary %in% installed.packages()[, 'Package']], dep = T)
	for(i in 1:length(necessary))
		library(necessary[i], character.only = TRUE)
}
load_library('stringr') # text handling
CY=2014
debut<-str_c(CY-1,"-09-01 00:00:00")
fin<-str_c(CY,"-05-01 00:00:00")
datedebut<-str_c(CY-1,"-09-01")
datefin<-str_c(CY,"-05-01")
graphics.off()

if(getUsername() == 'cedric.briand') setwd("C:/Users/cedric.briand/Documents/GitHub/didson/") 
if(getUsername() == 'cedric.briand')  datawd<-"C:/Users/cedric.briand/Documents/GitHub/didson/data/" 

if(getUsername() == 'clarisse') setwd("C:/Users/cedric.briand/Documents/GitHub/didson/") 
if(getUsername() == 'clarisse')  datawd<-"C:/Users/cedric.briand/Documents/GitHub/didson/data/" 


load(file=str_c(datawd,"d3ej.Rdata"))
load(file=str_c(datawd,"d.Rdata"))
# dans le cas 3.75, la dernière longueur est de 2.75 et ça fout la zone
# en fait ce cas correspond à une seule anguille vue (sur toutes les 3.75 et encore figurez vous qu'elle descend la bougresse)
# je supprime la dernière distance, et la surface de la dernière aire de détection est dans ce cas un peu plus grande. 
# la dernière aire s'arrêtera à 13.75 au lieu de 13. Nous pourrons vivre avec ce poids sur la conscience...
# j'ai vérifié ce cas ne correspond qu'à 3.75 # d3ej$dsf_distancestart[which(unlist(lapply(d,function(X) length(X$det_frames_int_area)))==6)]
#$det_frames_range
#[1] "3.75-5"      "5-7"         "7-9"         "9-11"        "11-13.75"   
#[6] "13.75-13.75"
for (i in which(unlist(lapply(d,function(X) length(X$det_frames_int_area)))==6)){
	d[[i]]$det_frames_int_area<-d[[i]]$det_frames_int_area[1:5]
	d[[i]]$det_frames_range<-d[[i]]$det_frames_range[1:5]
}
det_frames_int_area<-matrix(unlist(lapply(d,function(X) X$det_frames_int_area)),nrow=length(d),ncol=5,byrow=TRUE)
# certains commencent à 5 d'autres avant, pour des raisons de manipulation de matrices
# Je commence par rajouter une colonne puis je décalle les colonnes quand le premier enregistrement commence à moins de 5 m
det_frames_int_area<-cbind(NA,det_frames_int_area)
det_frames_int_area[d3ej$dsf_distancestart==5,1]<-0
det_frames_int_area[d3ej$dsf_distancestart<5,1:5]<-det_frames_int_area[d3ej$dsf_distancestart<5,2:6]
det_frames_int_area[d3ej$dsf_distancestart<5,6]<-0

#nombre d'anguilles
#str(d3ej)

#d3ej$ex # 1 ok 2 pas de lecture 3 mauvais positionnement
indexOK<-d3ej$dsf_fls_id==0&d3ej$ex==1 # effectifs utilisés pour extrapolation plus loin
indexOK[is.na(indexOK)]<-FALSE
d3ej$indexOK<-indexOK

d3ej$Nprimo4<-rowSums(d3ej[,c("drr_eelplus","drr_eelminus")],na.rm=TRUE)
# attention ci dessous lancer debit sinon manque pdebit4 dans d3ej
d3ej$pdebit4<-d3ej$pvol4
d3ejb<-d3ej[,c("dsf_id","dsf_timeinit","dsf_timeend","position","Date","indexOK","area_intersect","area_migration_frame","pdebit4","Nprimo4","mois")]
d3ejb<-cbind(d3ejb,det_frames_int_area)
colnames(d3ejb)[(ncol(d3ejb)-5):ncol(d3ejb)]<-c("(2,5]","(5,7]","(7,9]","(9,11]","(11,13]","(13,15]")
horaires<-function(data){
	data$Hdeb<-as.numeric(strftime(data$dsf_timeinit,"%H"))+as.numeric(strftime(data$dsf_timeinit,"%M"))/60
	data$Hfin<-as.numeric(strftime(data$dsf_timeend,"%H"))+round(as.numeric(strftime(data$dsf_timeend,"%M"))/60,2)
	data$Hfin[data$Hfin==0]<-24
	indx<-data$Hfin==24&data$Hdeb==0
	data[indx,"Hfin"]<-24.0
	data[indx,"Hdeb"]<-23.5
	data$xmin<-strptime(str_c(as.character(data$Date)," 09:00"),"%Y-%m-%d %H:%M") # pour les graphiques en rectangle
	data$xmax<-strptime(str_c(as.character(data$Date)," 17:00"),"%Y-%m-%d %H:%M")
	return(data)
}
d3ejb<-horaires(d3ejb)
d3ejb$jour=as.integer(d3ejb$Date-min(d3ejb$Date)+1)
d3ejb$heure<-d3ejb$Hdeb

d3ejb$hourm<-d3ejb$Hdeb
# heures de 0 à 32
d3ejb$hourm[d3ejb$Hdeb<10]<-d3ejb$hourm[d3ejb$Hdeb<10]+7  # jusqu'à 20
d3ejb$hourm[d3ejb$Hdeb>=18]<-d3ejb$hourm[d3ejb$Hdeb>=18]-17 # commence à 1
d3ejb$hourm<-(d3ejb$hourm*2)-1
#plot(table(d3ejb$jour,d3ejb$hourm))
d3ejb[d3ejb$jour==152,"hourm"]
# il manque le changement d'heure

# la journée commence à 18 h00 (1) 
# il faut enlever 1 aux jours des heures >15
d3ejb$jour[d3ejb$hourm>=15]<-d3ejb$jour[d3ejb$hourm>=15]-1
head(d3ejb[,c("jour","hourm","Hdeb")],100)
# option bof pour la structure en taille par mois
# ne semble pas constant d'une annee sur l'autre
d3ejb$mois2<-d3ej$mois
levels(d3ejb$mois2)<-c("11","11","11","12","01","01","01","01")
jours_incomplets <-unique(d3ejb$jour)[table(d3ejb$jour)<32]
d3ejb<-d3ejb[!d3ejb$jour%in%c(0,183,184),]

#########################################
require(gtools)
load(file=str_c(datawd,"dddp.Rdata"))
correct<-function(data,var,value,newvalue){
	data[data[,var]==value&!is.na(data[,var]),var]<-newvalue
	return(data)
}
dddp<-correct(dddp,"dsf_distancestart",2,2.08)
dddp$psf_l_cm[dddp$psf_l_cm==0]<-60 # 3 cas je ne sais pas ce qui s'est passé
dddp$psf_l_cl2<-cut(dddp$psf_l_cm,c(0,45,60,80,150)) # classes de taille

dddp$c_radius2<-cut(dddp$psf_radius_m,c(2,5,7,9,11,13,15)) # classes de distance
dddp$N<-rowSums(dddp[,c("drr_eelplus","drr_eelminus")],na.rm=TRUE)

res<-as.array(xtabs(psf_file~dsf_id+psf_l_cl2,data=dddp)) #psf_file = 1 tout le temps
res2<-cbind(as.numeric(dimnames(res)[[1]]),res)
res2<-as.data.frame(res2)
colnames(res2)=c("dsf_id",str_c(dimnames(res)[[2]][1]),
		str_c(dimnames(res)[[2]][2]),
		str_c(dimnames(res)[[2]][3]),
		str_c(dimnames(res)[[2]][4])
)

d3ejbtau<-merge(d3ejb,res2,by="dsf_id",all.x=TRUE,all.y=FALSE)
# on observe pas d'anguilles quand il n'y a pas d'écoulement sur la vanne 4
d3ejbtau[d3ejbtau$pdebit4==0,c("(0,45]","(45,60]", "(60,80]","(80,150]")]<-0
d3ejbtau[!d3ejbtau$indexOK,c("(0,45]","(45,60]", "(60,80]","(80,150]")]<-NA




open_in_excel <- function(some_df){
	tFile<-paste("C:/temp/",gsub("\\\\","",tempfile(fileext=paste0(substitute(some_df), ".csv"),tmpdir="")),sep="")
	write.table(some_df, tFile, row.names=F, sep=";", quote=F)
	system(paste('open -a \"/ProgramData/Microsoft/Windows/Start Menu/Programs/Microsoft Office/Microsoft Excel 2010\"', tFile))
}
#open_in_excel(d3ejbtau)
save(d3ejbtau,file=str_c(datawd,"d3ejbtau.Rdata"))

#plot(table(d3ejb$jour))
#plot(table(d3ejb$hourm))

#############"""
# passage au formal long
###################
require(reshape2)
#ë celui là est juste plus petit et plus gentil
d3ejb2tau<-d3ejbtau[,c("position","Date","indexOK","area_intersect","area_migration_frame","pdebit4",
				"Nprimo4","jour","mois","heure","hourm",
				"(0,45]","(45,60]", "(60,80]","(80,150]")]
d3ejb2tau$tranche<-1:nrow(d3ejb2tau)

d3ejbmtau<-melt(data=d3ejb2tau, 
		id.var=c("tranche","position","Date","indexOK","area_intersect","area_migration_frame","pdebit4",
				"Nprimo4","jour","mois","heure","hourm"
				),
		measure.vars=c("(0,45]","(45,60]", "(60,80]","(80,150]"),
		variable.name = "var",
		value.name = "number"
)


d3ejbmtau$tau <- as.character(d3ejbmtau$var)
d3ejbmtau<-d3ejbmtau[,-match("var",colnames(d3ejbmtau))]
d3ejbmtau<-d3ejbmtau[order(d3ejbmtau$jour,d3ejbmtau$hourm),]
save(d3ejbmtau,file=str_c(datawd,"d3ejbmtau.Rdata"))



