# Analyse bayesienne des donnees DIDSON de la Vilaine
# 
# Author: Beaulaton Laurent (Onema), Briand Cedric (IAV), Clarisse Boulenger (INRA)
###############################################################################

# chargement des bibliotheques, donnees et fonctions
require(jagsUI)
require(ggmcmc) #graphique bayesien

# chargement des donneees et du modele
#source("script/chargement_donnees.R")
source("script/fonctions.R")

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

parameters<-c("N_total","N_jour","p_t","mu_gamma","sigma_gamma")
model = jags(data=data_list,
		#inits = model.inits,
		model.file = "script/modele_clarisse.jags",
		n.chains = 3, #1
		n.thin = 5, # nombre d'itérations enregistrée
		n.burnin =  40000 , # 40000 
		n.iter = 60000, # 60000
		parameters.to.save=parameters)
#save(model,file=str_c(datawd,"modele_clarisse.Rdata"))

print(model)
summary(model)
traceplot(model,parameter="N_total")
whiskerplot(model, parameters="N_jour")
sum(model$q50$N_jour)
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
