require(gtools)		
#	The Gamma distribution with parameters shape = a and scale = s has density 
#	f(x)= 1/(s^a Gamma(a)) x^(a-1) e^-(x/s) 
#	and rate=1/scale 

#	From jags user manual dgamma(r,λ)
#	____________________ 
#	dgamma(r, lambda) has a density of 
#	 λ^r*x^(r−1)*exp(− λ x) 
#	-------------------- 
#			Gamma(r) 
#	
#	 λ=1/s then  λ in jags is the rate=1/s parameter of dgamma 
#	and r in jags is the shape=a parameter of dgamma r=a
# E[X] = alpha/beta
#Var[X] = alpha/beta^2
#beta=E[X]/Var[X]
#alpha=E[X]^2/Var[X]
#beta=E[X]/sigma[X]^2
#alpha=E[X]^2/sigma[X]^2
#hist(rgamma(1000,1,0.001))		
#hist(rgamma(1000,shape=gamma_a[i], rate=gamma_b[i]))

		mu_gamma <- rgamma(1000,1,0.0001)
		#mu_gamma <- runif(1000,1000,100000)
		sigma_gamma <- rgamma(1000,1,0.0001)
		gamma_a <- mu_gamma^2 / sigma_gamma^2
		gamma_b <- mu_gamma / sigma_gamma^2	
		V<-vector()
		for (i in 1:1000){
		V[i] <- rgamma(1,shape=gamma_a[i], rate=gamma_b[i]) # nombre d'anguilles du jour
		}
		hist(V)
		

