# Analyse bayesienne des donnees DIDSON de la Vilaine
# 
# Author: Beaulaton Laurent (Onema), Briand Cedric (IAV), Clarisse Boulenger (INRA)
###############################################################################

# chargement des bibliotheques, donnees et fonctions
require(jagsUI)
require(ggmcmc) #graphique bayesien


# Lancement du modele JAGS
model.inits <-
		function(){list(
			#"a"=rep(0.37,32)#,
			#"N_jour"=n_jour_init#,
		  "N4_tprim"=N4_tprim,
			"N_t"=N_t_init,
			"N4_t"=N4_t_init,
			
			#"alphaprim"=c(0,0,0,0,0)#,
			"betaprim"=c(0,0,0,0),
			#"sigma_gamma"=c(0.5,0.5,0.5,0.5),
			#"mu_gamma"=c(1,1,1,1),
			"af"=1
	)
}

data_list<-list(n_jour = n_jour,
		n_tranche = n_tranche,
		pdebit4 = pdebit4,
		n_tau=n_tau,
		jour = jour,
		horaire = horaire,
		p_surface=p_surface,
		N04prim_t= N04prim_t,
		n_i=n_i,
		tranche=tranche,
		cdt=cdt#,
		#cdd=cdd,
		#n_delta=n_delta
		)

parameters<-c("N_total","N_jour","N4_t","p_t","mu_gamma","sigma_gamma","efficacite","af","betaprim")
model = jags(data=data_list,
		inits = model.inits,
		model.file = "script/modele_clarissetau.jags",
		n.chains = 1, #3
		n.thin = 5, # nombre d'it?rations enregistr?e
		n.burnin =  400 , # 40000 
		n.iter = 600, # 60000
		parameters.to.save=parameters)
plot(model)