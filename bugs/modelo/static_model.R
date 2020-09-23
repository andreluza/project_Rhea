##Codigo da implementacao do Modelo Estático
require(reshape); require(reshape2); require(ggplot2); require(rgdal);
require(raster); require(rgeos); require (spdep); library(dclone); 
library(rjags); library(snow); library(fields); require(R2WinBUGS); require(vegan)

##  carregar os dados de ocorrencia
y2 <- readRDS("aedes_aegypti_evidence")
y2 <- apply (y2,c (1,2,3), function (i) ifelse (i >0 ,1,0))

## mudando as dimensoes do array
#y2 <- aperm (y2, c(1,3,2))
#y2 <- apply(y2,c(1,3),sum,na.rm=F)# matriz ocorrencia municipio/semana ep

## se diferente de NA em alguma das visitas, soma e remove NA. Se só tem NA em todas as visitas, o valor será NA
y2_list <- lapply (as.list(seq(1, dim(y2)[1])), function (m)
	unlist (lapply (as.list(seq (1,dim(y2)[2])), function (k)
		ifelse (any (unlist(lapply (as.list(seq(1,dim(y2)[3])), function (i) y2[m,k,i])) != "NA"), 
			sum(unlist(lapply (as.list(seq(1,dim(y2)[3])), function (i) y2[m,k,i])), na.rm=T),"NA"))))

y2 <- as.matrix (do.call (rbind.data.frame, y2_list))
colnames (y2) <- NULL

# soma as linhas a cada 13 colunas
y2 <-as.matrix (do.call (rbind.data.frame, lapply (as.list(seq (1,dim(y2)[1])), function (k)
	sapply(1:(ncol(y2)/13),function(x)
	{ifelse (any (y2[k,1:13+(x-1)*13] != "NA"), sum(y2[k,1:13+(x-1)*13],na.rm=T), sum(y2[k,1:13+(x-1)*13],na.rm=F))}))))
colnames (y2) <-NULL
y2 [y2 > 1]<- 1

## carregando dados de esfor?o amostral
y2_effort <- readRDS("aedes_effort")
y2_effort <- aperm (y2_effort, c(1,3,2)) ## mudando as dimens?es do array
y2_effort[is.na(y2_effort)] <- 0
# matriz esforço municipio/semana ep
y2_effort <- apply(y2_effort,c(1,3),sum,na.rm=T) 
y2_effort<-sapply(1:(ncol(y2_effort)/13),function(x){rowSums(y2_effort[,1:13+(x-1)*13])}) # soma as linhas a cada 13 colunas
y2_effort <- decostand (sqrt (y2_effort),"standardize")



# dados espaciais
## escolha o layer "43MUE250GC_SIR.shp"
munRS<- readOGR(dsn=getwd(),layer="43MUE250GC_SIR", encoding = "UTF-8")
coord_xy <- gCentroid (munRS, byid=T)@coords 

# Generate the neighbours (8 direct neighbours)
neigh <- poly2nb (munRS)
plot(neigh,cbind(coord_xy [,"x"], coord_xy [,"y"]))
#neigh <-dnearneigh(cbind(coord_xy [,"x"], coord_xy [,"y"]),longlat = T,d1=0, d2=30)
#plot(munRS[knearneigh(cbind(coord_xy [,"x"], coord_xy [,"y"]), k=10, longlat = T)$nn[1,],],col="red")

# Number of neighbours ## quantos sitios estão conectados a n distancia
table(card(neigh))
# Convert the neighbourhood
winnb <- nb2WB(neigh)

#### data
str(win.data2<- list(y=y2, M=dim(y2)[1], J= dim (y2)[2],effort= y2_effort, 
	num = winnb$num, adj = winnb$adj, weights = winnb$weights))

sink("StaticModCAR.txt")
cat("
    
model {
	# priors
	alpha.p ~ dunif (-10,10)
	beta.p ~ dunif (-10,10)	 
		
	# CAR prior distribution for spatial random effects:
	rho[1:M] ~ car.normal(adj[], weights[], num[], tau)
	
	# Ecological submodel: Define state conditional on parameters
	for(i in 1:M){    ## occupancy with spatial autocorrelation
		z [i] ~ dbern(psi[i])
		logit(psi [i]) <- alpha + rho [i]
			for (j in 1:J){ 		## loop over replicated surveys
			   y [i,j] ~ dbern(eff.p[i,j])
			   eff.p[i,j] <- z[i]*p[i,j] ## detection/non-det at i in j
			   logit(p[i,j]) <- 0.5+beta.p*effort [i,j]
    	     	   
			### bayesian p-value
			#Presi [i,j] <- abs(y[i,j]-p[i,j])# residuos (valores absolutos)
			#y.new [i,j] ~ dbern (eff.p [i,j])
			#Presi.new [i,j] <- abs(y.new[i,j]-p[i,j])
		}
	}
	
	# model fit
	#fit <- sum (Presi [i,j]) ##discrepancia para os dados atuais
	#fit.new <- sum (Presi.new [i,j]) ## discrepancia para os dados replicados
	
	# other priors
	alpha ~ dunif(-10,10)
	vrho ~ dnorm(0, 0.2) I(0,)
	tau  <- 1/vrho
		
	# Derived parameters: Sample and population occupancy, growth rate and turnover
	n.occ <- sum(z[])#/M
	mutot <- sum(psi[])
	
	} # end of the model
    ",fill = TRUE)
sink()

zst <- apply(y2, 1, max, na.rm = TRUE)	# Observed occurrence as inits for z
zst[zst == '-Inf'] <- 1 # max of c(NA,NA,NA) with na.rm = TRUE returns -Inf, change to 1
inits <- function(){list(z = zst, alpha.p=runif(1), beta.p= runif(1), alpha = runif(1), rho = rep(0, nrow(y2)))}#psi=runif(1),
	
## Parameters to monitor
params <- c("psi","n.occ","z", "alpha.p","beta.p","mutot","rho", "alpha","vrho")
# MCMC settings
ni <- 20000
nt <- 1
nb <- 5000
nc <- 4
na <- 3000

#######

#library (jagsUI)
#fm2<- jags (win.data2, inits, params, "StaticModCAR.txt", n.chains=nc, 
#	n.thin=nt, n.iter=ni, n.burnin=nb,parallel=F)

samples <- bugs.fit(data = win.data2, params, model = "StaticModCAR.txt", inits = inits,
	 n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
	DIC = F, bugs.directory = "C:/Program Files (x86)/winbugs14_unrestricted/WinBUGS14", save.history = FALSE,
	debug=T)

saveRDS (samples, "results_autoc.rda")

summary (samples)
gelman.diag(samples,multivariate=F)
gelman.plot(samples)

g <- matrix(NA, nrow=nvar(samples), ncol=2)
 for (v in 1:nvar(samples)) {
    g[v,] <- gelman.diag(samples[,v])$psrf
 }

# Compute posterior mean of the predictions
predsMean <- apply(as.matrix(samples[,paste0("psi[",1:nrow(y2),"]")]), 2, mean)
# Compute posterior sd of the predictions
predsSd <- apply(as.matrix(samples[,paste0("psi[",1:nrow(y2),"]")]), 2, sd)
# Compute posterior quantiles of the predictions
predsQuantiles <- apply(as.matrix(samples[,paste0("psi[",1:nrow(y2),"]")]), 2, quantile, c(0.025, 0.975))
rho <- apply(as.matrix(samples[,paste0("rho[",1:nrow(y2),"]")]), 2, mean)

##
# shapefile south america
southAme<- readOGR(dsn=choose.files(),layer="South_America")

BR_AR_URU<- southAme [southAme@data$COUNTRY == "Paraguay" | southAme@data$COUNTRY == "Brazil" | southAme@data$COUNTRY == "Argentina" | southAme@data$COUNTRY == "Uruguay", ]
crs(munRS)<-crs(BR_AR_URU)

#dados para o mapa
cores <- data.frame (cores=rho,NM_MUNICIP=munRS@data$NM_MUNICIP)
f.mun<-fortify(munRS, region="NM_MUNICIP")
f.mun<- cbind (f.mun, valor_z= cores [match (f.mun$id, cores$NM_MUNICIP),]$cores)

png(file="sp_autoc_strength.png", width=20, height=20, units="cm", res=600, family="serif")
	a <- ggplot() + geom_polygon (data=BR_AR_URU, aes(x=long, y=lat, group=group),size = 0.3, fill="gray85", colour="gray35",alpha=0.5) +
	coord_fixed (xlim = c(-58, -47.5),  ylim = c(-34, -27), ratio = 1) 

	b<- a+  geom_polygon(data=f.mun, aes(x=long, y=lat, group=group, color=valor_z, fill=valor_z), colour = "black", size=0.6) + 
      coord_fixed (xlim = c(-58, -49),  ylim = c(-34, -27), ratio = 1) +
	ggtitle (expression (paste ("Spatial autocorrelation strength"," (", Psi,")"))) +
	#scale_fill_manual(values = c("white", "gray35"), name="Ao menos uma amostra?")
	scale_fill_gradient2 (low='yellow',mid='orange',high='red', midpoint = 0.5,name=NULL, na.value="white",limits=c(-4.5,4.5))## para continuo
 
	c<-b + annotate(geom="text", x=-56, y=-32, label="URUGUAI",color="black",size=4) +
	annotate(geom="text", x=-57.25, y=-28.5, label="ARGENTINA",color="black",size=4)+
	annotate(geom="text", x=-57., y=-27, label="PARAGUAI",color="black",size=4)+
	annotate(geom="text", x=-51, y=-27, label="Santa Catarina",color="black",size=4)+
	annotate(geom="text", x=-50.5, y=-32.5, label="OCEANO ATLÂNTICO",color="black",size=4)

	d<-c + ggsn::scalebar(f.mun, dist = 100, st.dist=0.02,st.size=3, height=0.01, dd2km = TRUE, model = 'WGS84', location = "bottomleft")

	e<-d + theme_bw() + xlab("Longitude (graus decimais)") + ylab("Latitude (graus decimais)") +
  		theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey80"), panel.grid.minor = element_blank())
	
e
dev.off()


