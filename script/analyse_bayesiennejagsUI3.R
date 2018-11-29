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
model.inits <-
		function(){list("af"=0,
				alphaprim=c(0,0,0,0,0,0),
				betaprim=c(0,0,0,0),
				"N_jour"=init_N_jour,
				"N4_t"=N4_t_init,
				"N4_t_tau"=N4_t_tau_init)}		

data_list<-list(n_jour = n_jour,
		n_tranche = n_tranche,
		pdebit4 = pdebit4,
		jour = jour,
		horaire = horaire,
		n_tau=n_tau,
		n_delta=n_delta,
		p_dirichlet_tau=p_dirichlet_tau,
		p_dirichlet_t=p_dirichlet_t,
		p_surface=p_surface,
		N04prim_t_tau_delta=N04prim_t_tau_delta)


model = jags(data=data_list,
		inits = model.inits,
		model.file = "script/model.jags",
		n.chains = 1, #3
		n.thin = 1, # nombre d'itérations enregistrée
		n.burnin = 1000,
		n.iter = 2000,
		parameters.to.save="N_jour")
#update(model, 1000) # pour bruler des valeurs supplementaires
result = coda.samples(model, c("N_jour","af","p_t","p_tau"), 1000, progress.bar = "gui", thin = 1)
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
