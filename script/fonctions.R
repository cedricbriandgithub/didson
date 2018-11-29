# Analyse bayésienne des données DIDSON de la Vilaine
# fonction d'intérêt générale
# 
# Author: Beaulaton Laurent (Onema), Briand Cédric (IAV)
###############################################################################


##########
# fonction pour Bayes
f_var_indice = function(var.names, n)
	paste(var.names, "[", n, "]", sep = "")

f_varmat_indice = function(var.names, l,c)
	paste(var.names, "[", l,",",c, "]", sep = "")

