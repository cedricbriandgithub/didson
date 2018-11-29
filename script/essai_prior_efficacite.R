# TODO: Add comment
# 
# Author: cedric.briand
###############################################################################
require(gtools)
CY=2014

datawdy<-str_c("F:/workspace/pdata/didson/rapport/data/" ,CY,"/")

load(file=str_c(datawdy,"dddp.Rdata"))
correct<-function(data,var,value,newvalue){
	data[data[,var]==value&!is.na(data[,var]),var]<-newvalue
	return(data)
}
dddp<-correct(dddp,"dsf_distancestart",2,2.08)
dddp$psf_l_cl2<-cut(dddp$psf_l_cm,c(0,45,60,80,150)) # classes de taille
dddp$c_radius2<-cut(dddp$psf_radius_m,c(2,5,7,9,11,13,15)) # classes de distance
levels(dddp$c_radius2)
#===================================================
# "(2,5]"   "(5,7]"   "(7,9]"   "(9,11]"  "(11,13]" "(13,15]"


# On veut tester si un prior non informatif
# peut donner des valeurs correctes de l'efficiacité

fn_test_betaprim<-function(precision_a,precision_betaprim,precision_alphaprim){
	sigma_a=sqrt(1/precision_a)
	sigma_betaprim=sqrt(1/precision_betaprim)
	sigma_alphaprim=sqrt(1/precision_alphaprim)
#========================
# valeurs possibles pour vdelta
# vecteur des classes de distance
	vdelta<-(1:5-mean(1:5))
	vdelta<-vdelta/max(vdelta)
	vdelta
	stopifnot(sum(vdelta)==0)
# valeurs possibles pour vtau
# vecteur des classes de taille
	vtau<-(1:4-mean(1:4))
	vtau<-vtau/max(vtau)
	a<-rnorm(1000,0,sigma_a)
	b<-rnorm(1000,0,sigma_betaprim)
	c<-rnorm(1000,0,sigma_alphaprim)
	delta<-sample(vdelta,size=1000,replace=TRUE)		
	tau<-sample(vtau,size=1000,replace=TRUE)		
	ee<-a+delta*b+tau*c
	e<-gtools::inv.logit(ee)
	hist(e)
}
fn_test_betaprim(precision_a=10,precision_betaprim=10,precision_alphaprim=10)

# Pour certaines valeurs de départ ça semble OK
a=0
b=-1.5
c=+1
ee<-a+vdelta*b
ee<-matrix(rep(ee,4),byrow=TRUE,ncol=6)

for (i in 1:nrow(ee)){
	ee[i,]<-ee[i,]+(vtau*c)[i]
}
ee<-inv.logit(ee)
round(ee,2)


#===================================================
# "(2,5]"   "(5,7]"   "(7,9]"   "(9,11]"  "(11,13]" "(13,15]"
delta<-0:5
vdelta<-delta/max(delta)
delta
#===================================================
#  "(0,45]"   "(45,60]"  "(60,80]"  "(80,150]"
tau<-0:3
vtau<-tau/max(tau)


#> vdelta
#[1] 0.0 0.2 0.4 0.6 0.8 1.0
#> vtau
#[1] 0.0000000 0.3333333 0.6666667 1.0000000
# f(delta,tau)=a+b*delta+c*tau
# Au plus pres pour les grandes l'efficacite est fixee a  1
#f(0,1)<1 =>  a=1-c
#=> f(delta,tau)=1+b*delta+(c*(tau-1)
# Au plus loin pour les petites l'efficacite est >0
# f(1,0)>0 => b>c-1

# Verification la valeur se trouve bien entre 0 et 1
c<-runif(1000,0,1)
for (j in 1:1000){
	b[j]<-runif(1,c[j]-1,0)
}
delta<-sample(vdelta,size=1000,replace=TRUE)		
tau<-sample(vtau,size=1000,replace=TRUE)		
ee<-1+delta*b+(tau-1)*c
hist(ee)

# un exemple
b<--0.4
c<-0.5
a=1-c
ee<-a+vdelta*b
ee<-matrix(rep(ee,4),byrow=TRUE,ncol=6)

for (i in 1:nrow(ee)){
	ee[i,]<-ee[i,]+vtau[i]*c
}
ee
