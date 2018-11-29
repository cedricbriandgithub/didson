# script de préparation du jeu de données à partir des analyses précédentes (rapport 2014)
# Author: cedric.briand
###############################################################################


rm(list=ls(all=TRUE)) # nettoyage complet
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
setwd("F:/workspace/p/didson/")
datawd<-"F:/workspace/pdata/didson/rapport/data/" 
datawdb<-"F:/workspace/pdata/didson/bayesien/donnees/" 
datawdy<-str_c(datawd,CY,"/")
imgwd<-"F:/workspace/p/didson/image/"
imgwdy<-str_c(imgwd,CY,"/")
tabwdy<-str_c("F:/workspace/p/didson/table/",CY,"/")
load(file=str_c(datawdy,"d3ej.Rdata"))
load(file=str_c(datawdy,"d.Rdata"))
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
load(file=str_c(datawdy,"dddp.Rdata"))
correct<-function(data,var,value,newvalue){
	data[data[,var]==value&!is.na(data[,var]),var]<-newvalue
	return(data)
}
dddp<-correct(dddp,"dsf_distancestart",2,2.08)
dddp$psf_l_cm[dddp$psf_l_cm==0]<-60 # 3 cas je ne sais pas ce qui s'est passé
dddp$psf_l_cl2<-cut(dddp$psf_l_cm,c(0,45,60,80,150)) # classes de taille

dddp$c_radius2<-cut(dddp$psf_radius_m,c(2,5,7,9,11,13,15)) # classes de distance
dddp$N<-rowSums(dddp[,c("drr_eelplus","drr_eelminus")],na.rm=TRUE)

res<-as.array(xtabs(psf_file~dsf_id+psf_l_cl2+c_radius2,data=dddp)) #psf_file = 1 tout le temps
res2<-cbind(as.numeric(dimnames(res)[[1]]),res[,1,],res[,2,],res[,3,],res[,4,])
res2<-as.data.frame(res2)
colnames(res2)=c("dsf_id",str_c(dimnames(res)[[2]][1],dimnames(res)[[3]]),
		str_c(dimnames(res)[[2]][2],dimnames(res)[[3]]),
		str_c(dimnames(res)[[2]][3],dimnames(res)[[3]]),
		str_c(dimnames(res)[[2]][4],dimnames(res)[[3]])
)

d3ejb<-merge(d3ejb,res2,by="dsf_id",all.x=TRUE,all.y=FALSE)
# on observe pas d'anguilles quand il n'y a pas d'écoulement sur la vanne 4
d3ejb[!d3ejb$indexOK,c("(5,7]","(7,9]","(9,11]","(11,13]","(13,15]")]<-0
d3ejb[!d3ejb$indexOK,c(26:49)]<-NA
# on remet les zéro dans les colonnes de détails
d3ejb[d3ejb$"(5,7]"==0,grep('5,7',colnames(d3ejb))[2:5]]<-NA
d3ejb[d3ejb$"(7,9]"==0,grep('7,9',colnames(d3ejb))[2:5]]<-NA
d3ejb[d3ejb$"(9,11]"==0,grep('9,11',colnames(d3ejb))[2:5]]<-NA
d3ejb[d3ejb$"(11,13]"==0,grep('11,13',colnames(d3ejb))[2:5]]<-NA
d3ejb[d3ejb$"(13,15]"==0,grep('13,15',colnames(d3ejb))[2:5]]<-NA
d3ejb[d3ejb$indexOK&d3ejb$Nprimo4==0,grep("]\\(",colnames(d3ejb))]<-0



open_in_excel <- function(some_df){
	tFile<-paste("C:/temp/",gsub("\\\\","",tempfile(fileext=paste0(substitute(some_df), ".csv"),tmpdir="")),sep="")
	write.table(some_df, tFile, row.names=F, sep=";", quote=F)
	system(paste('open -a \"/ProgramData/Microsoft/Windows/Start Menu/Programs/Microsoft Office/Microsoft Excel 2010\"', tFile))
}
#open_in_excel(d3ejb)
save(d3ejb,file=str_c(datawdb,"d3ejb.Rdata"))
setwd("F:/workspace/p/didson/bayesien")
#plot(table(d3ejb$jour))
#plot(table(d3ejb$hourm))

#############"""
# passage au formal long
###################
require(reshape2)
d3ejb2<-d3ejb[,c("position","Date","indexOK","area_intersect","area_migration_frame","pdebit4",
				"Nprimo4","jour","mois","heure","hourm",
				"(5,7]","(7,9]","(9,11]","(11,13]","(13,15]",colnames(d3ejb)[grep("]\\(",colnames(d3ejb))])]
d3ejb2$tranche<-1:nrow(d3ejb2)
d3ejbm<-melt(data=d3ejb2, 
		id.var=c("tranche","position","Date","indexOK","area_intersect","area_migration_frame","pdebit4",
				"Nprimo4","jour","mois","heure","hourm",
				"(5,7]","(7,9]","(9,11]","(11,13]","(13,15]"),
		measure.vars=colnames(d3ejb)[grep("]\\(",colnames(d3ejb))],
		variable.name = "var",
		value.name = "number"
)


d3ejbm$tau<-str_c(lapply(strsplit(as.character(d3ejbm$var),"\\]"),function(X)X[[1]]),"]")
d3ejbm$delta<-str_c(lapply(strsplit(as.character(d3ejbm$var),"\\]"),function(X)X[[2]]),"]")
d3ejbm<-d3ejbm[,-match("var",colnames(d3ejbm))]
d3ejbm<-d3ejbm[order(d3ejbm$jour,d3ejbm$hourm),]
save(d3ejbm,file=str_c(datawdb,"d3ejbm.Rdata"))
open_in_excel(d3ejbm)
d3ejbm$surface<-NA
d3ejbm$surface[d3ejbm$delta=="(5,7]"]<-d3ejbm$"(5,7]"[d3ejbm$delta=="(5,7]"]
d3ejbm$surface[d3ejbm$delta=="(7,9]"]<-d3ejbm$"(7,9]"[d3ejbm$delta=="(7,9]"]
d3ejbm$surface[d3ejbm$delta=="(9,11]"]<-d3ejbm$"(9,11]"[d3ejbm$delta=="(9,11]"]
d3ejbm$surface[d3ejbm$delta=="(11,13]"]<-d3ejbm$"(11,13]"[d3ejbm$delta=="(11,13]"]
d3ejbm$surface[d3ejbm$delta=="(13,15]"]<-d3ejbm$"(13,15]"[d3ejbm$delta=="(13,15]"]
d3ejbm<-d3ejbm[d3ejbm$delta!="(2,5]",]
d3ejbm$tranche
save(d3ejbm,file=str_c(datawdb,"d3ejbm.Rdata"))