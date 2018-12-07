# Analyse bayÃ©sienne des donnÃ©es DIDSON de la Vilaine
# fonction d'intÃ©rÃªt gÃ©nÃ©rale
# 
# Author: Beaulaton Laurent (Onema), Briand CÃ©dric (IAV)
###############################################################################


##########
# fonction pour Bayes
f_var_indice = function(var.names, n)
	paste(var.names, "[", n, "]", sep = "")

f_varmat_indice = function(var.names, l,c)
	paste(var.names, "[", l,",",c, "]", sep = "")

  open_in_excel <- function(some_df){
	tFile<-paste("C:/temp/",gsub("\\\\","",tempfile(fileext=paste0(substitute(some_df), ".csv"),tmpdir="")),sep="")
	write.table(some_df, tFile, row.names=F, sep=";", quote=F)
	system(paste('open -a \"/ProgramData/Microsoft/Windows/Start Menu/Programs/Microsoft Office/Microsoft Excel 2010\"', tFile))
  }
