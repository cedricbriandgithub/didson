# Analyse bayesienne des donnees DIDSON de la Vilaine
# 
# Author: Beaulaton Laurent (Onema), Briand Cedric (IAV), Clarisse Boulenger (INRA)
###############################################################################


# chargement des donneees et du modele
source("script/chargement_donnees.R")

# chargement des bibliotheques, donnees et fonctions
require(jagsUI)
require(ggmcmc) #graphique bayesien




# Lancement du modele JAGS
#model.inits <-
#		list("N_jour"=init_N_jour,
#				"N4_t"=N4_t_init)		

data_list<-list(n_jour = n_jour,
		n_tranche = n_tranche,
		pdebit4 = pdebit4,
		jour = jour,
		horaire = horaire,
		N04prim_t_sum= N04prim_t_sum,
		p_surface2=p_surface2)

parameters<-c("N_total","N_jour","N4_t","N04prim_t_sum","p_t","mu_gamma","sigma_gamma")
model = jags(data=data_list,
		#inits = model.inits,
		model.file = "script/modele_clarisse.jags",
		n.chains = 3, #1
		n.thin = 5, # nombre d'itérations enregistrée
		n.burnin = 40000  , #  160000
		n.iter = 60000, # 200000
		parameters.to.save=parameters)
#save(model,file=str_c(datawd,"modele_clarisse1.Rdata"))

print(model)
summary(model)
traceplot(model,parameter="N_total") # converge rhat < 1.1 mettre plus d'itérations
whiskerplot(model, parameters="N_jour")
sum(model$q50$N_jour)
model$q50$mu_gamma
model$q2.5$mu_gamma
model$q97.5$mu_gamma
model$q50$sigma_gamma
model$q2.5$sigma_gamma
model$q97.5$sigma_gamma


# Essai pour comprendre ce qui ne va pas certains jours (estimation importante)
open_in_excel <- function(some_df){
  tFile<-paste("C:/temp/",gsub("\\\\","",tempfile(fileext=paste0(substitute(some_df), ".csv"),tmpdir="")),sep="")
  write.table(some_df, tFile, row.names=F, sep=";", quote=F)
  system(paste('open -a \"/ProgramData/Microsoft/Windows/Start Menu/Programs/Microsoft Office/Microsoft Excel 2010\"', tFile))
}
baddays<-which(model$q50$N_jour>15000)
sss<-data.frame("tranche"=1:n_tranche,pdebit4,jour,horaire,p_surface2,N04prim_t_sum)
par(mfrow=c(2,1))
with(sss,plot(tranche,pdebit4))
with(subset(sss,sss$jour%in%baddays),points(tranche,pdebit4,col="red"))
with(sss,plot(tranche,p_surface2))
with(subset(sss,sss$jour%in%baddays),points(tranche,p_surface2,col="red"))
#with(sss,plot(tranche,horaire))
x11()
par(mfrow=c(2,1))
with(subset(sss,sss$jour%in%baddays),plot(tranche,horaire,col=rainbow(max(d3ejb$hourm))))

with(sss,plot(tranche,N04prim_t_sum,col="blue"))
with(subset(sss,sss$jour%in%baddays),points(tranche,N04prim_t_sum,col="red"))
#################################

whiskerplot(model, parameters="p_t")
#update(model, 1000) # pour bruler des valeurs supplementaires

# "efficacite_fond","N_jour", "p_h", "p_tau"
summary(result[, c("N_jour[1]")])

j1 = unlist(result[, "N_jour[1]"])

test = matrix(unlist(result), nrow = 1000, byrow = FALSE)
dimnames(test) = attr(result[[1]], "dimnames")



sum(j1 == test[,1])

plot(test)
mean(test)
ggs_traceplot(ggs(result[,c("af","bf","cf")]))
colMeans(as.matrix(result[, c("af","bf","cf")]))

ggs_traceplot(ggs(result[,f_var_indice("N_jour", 1:3)]))
ggs_density(ggs(result[,c("efficacite_fond", "N_jour[1]")]))
ggs_density(ggs(result[,f_var_indice("p_t", 1:3)]))
ggs_density(ggs(result[,f_var_indice("N_jour", 1:3)]))
ggs_running(ggs(result[,f_var_indice("N_jour", 1:3)]))


boxplot(as.matrix(result[, f_var_indice("p_t", 1:32)]))
boxplot(as.matrix(result[, f_var_indice("p_tau", 1:4)]))
boxplot(as.matrix(result[, f_varmat_indice("efficacite_fond",1:4,1)]))
boxplot(as.matrix(result[, f_varmat_indice("efficacite_fond",2,1:6)]))
boxplot(as.matrix(result[, f_var_indice("N_jour", 1:n_jour)]))
#boxplot(as.matrix(result[, f_var_indice("N4_obs", 1:n_tranche)]))

N_jour_tire = as.matrix(result[, f_var_indice("N_jour", 1:n_jour)])
hist(rowSums(N_jour_tire))
boxplot(rowSums(N_jour_tire))
plot(colMeans(N_jour_tire), type = "l", col= "red")
