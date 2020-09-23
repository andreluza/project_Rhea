
#setwd("G:/Meu Drive/Data Integration Models/data2019/Data Models/Results/Data")
#setwd("C:/Users/vivia/Dropbox/Viviane Parrots/Data")

setwd("G:/Meu Drive/Data Integration Models/data2019/Data Models/Data2020")

library(R2WinBUGS)
library(spdep)
library(maps)
library(rgdal)
library(reshape)

#Detection/No-detection Data
dat <- read.csv("RCountData.csv")
dat$EFFORT_MIN <- as.numeric(as.character(dat$EFFORT_MIN))
dat[is.na(dat$EFFORT_MIN),8] <- mean(dat$EFFORT_MIN, na.rm=T) #put the mean effort - avoid drop off more than 50% of the data
dat2 <- read.csv("ebirdData2008_2018.csv", sep = ",")
dat2 <- dat2[!is.na(dat2$EFFORT.DIS),]
dat2 <- dat2[!is.na(dat2$DURATION.M),]
dat2$DURATION.M <- as.numeric(as.character(dat2$DURATION.M))
dat2 <- dat2[dat2$ALL.SPECIE==1,]
dat3 <- read.csv("WikiData2008_2018.csv", sep=",")
dat4 <- read.csv("XCdata2008_2018.csv", sep = ",") #[,c("ID", "NSPECIES", "A_VINACEA")]

## Covariates Data:
COVS <- read.csv("COVSdata.csv")
VegCover <- COVS$AFCOVER_PROP
ArauCover <- COVS$ARAUCOVER_PROP
Altitude <- log(COVS$ALTITUDE_M)+1

ni = 750000
nb = 100000
nthin = 100
nc = 3


###############################################
### Researchers data + Wiki + Ebird + XC + COVS  ### CAR 

data3 <- list(muni1 = dat$IDENT, muni2 = dat2$IDENT, muni3 = dat3$IDENT, muni4 = dat4$IDENT, 
              Y1 = dat$A_VINACEA, Y2 = dat2$A_VINACEA, Y3 = dat3$AVINACEA, Y4 = dat4$A_VINACEA, 
              nMuni = length(muni), nObs1 = nrow(dat), nObs2 = nrow(dat2), 
              nObs3 = nrow(dat3), nObs4 = nrow(dat4), 
              TObs = dat$EFFORT_MIN/60, SSee = dat2$NSPECIES, TObs2 = dat2$DURATION.M/60, 
              RLen = dat2$EFFORT.DIS, NPho = dat3$NPIC, NAud = dat3$NSONG,
              NAud2 = dat4$NSONGS, VegCover = VegCover, ArauCover = ArauCover, 
              Alt = Altitude, nCell = nrow(hex.centroids), cell.id = cell.id, 
              nMuni = length(muni), adj = adj, num = num, sumNeigh = sumNeigh) 

inits = function() {list(z = rep(1, data3$nMuni))}#, alpha=rep(0, nrow(hex.centroids)))}#, beta = c(0.3), BETA = rep(0.01,7), alpha = rep(0,data$nMuni))} 
params <- c("beta","psi","BETA","z", "spacesigma","alpha", "muP1", "muP2", "muP3", "muP4")

mod3 = "G:/Meu Drive/Data Integration Models/data2019/Data Models/Data2020/FULLModel/BasicEffortALLDataCOVSCAR.txt"
out3 <- bugs(data = data3, inits = inits, parameters.to.save = params, model.file = mod3,
             n.chains = nc, n.iter = ni,n.burnin = nb, n.thin = nthin, debug = TRUE)

save(out3, file = "FULLModel.RData")




###############################################
### A. Researchers data + Wiki + Ebird + XC  ### 1-Sem COVS, sem espaço - todos os registros

#data <- list(muni1 = dat$IDENT, muni2 = dat2$IDENT, muni3 = dat3$ID, muni4 = dat4$ID, 
#            Y1 = dat$A_VINACEA, Y2 = dat2$A_VINACEA, Y3 = dat3$AVINACEA, Y4 = dat4$A_VINACEA, 
#            nMuni = length(muni), nObs1 = nrow(dat), nObs2 = nrow(dat2), 
#            nObs3 = nrow(dat3), nObs4 = nrow(dat4), 
#            EFFO = dat$EFFORT_MIN/60, eN = dat2$NSPECIES, eHours = dat2$DURATION.M/60, 
#            eKM = dat2$EFFORT.DIS, Pic = dat3$NPIC, Song = dat3$NSONG,
#            NSP = dat4$NSONGS)
#            #,VegCover = VegCover$absol, ArauCover = ArauCover$absol) #NS = dat2$NSPECIES, 

#inits = function() {list(z = rep(1, data$nMuni))}#, beta = c(0.3), BETA = rep(0.01,7), alpha = rep(0,data$nMuni))} 
#params <- c("beta","psi","BETA","z","muP1", "muP2", "muP3", "muP4")#, "spacesigma","alpha")

#mod = "G:/My Drive/Data Integration Models/data2019/Data Models/BasicEffortALLData.txt"
#out <- bugs(data = data, inits = inits, parameters.to.save = params, model.file = mod,
#            n.chains = nc, n.iter = ni,n.burnin = nb, n.thin = nthin, debug = TRUE)

#save(out, file = "BasicEffortALLData.RData")



###############################################
### B. Researchers data + Wiki + Ebird + XC + COVS  ### 1-sem espaço - todos os registros

#data2 <- list(muni1 = dat$IDENT, muni2 = dat2$IDENT, muni3 = dat3$IDENT, muni4 = dat4$IDENT, 
#              Y1 = dat$A_VINACEA, Y2 = dat2$A_VINACEA, Y3 = dat3$AVINACEA, Y4 = dat4$A_VINACEA, 
#              nMuni = length(muni), nObs1 = nrow(dat), nObs2 = nrow(dat2), 
#              nObs3 = nrow(dat3), nObs4 = nrow(dat4), 
#              TObs = dat$EFFORT_MIN/60, SSee = dat2$NSPECIES, TObs2 = dat2$DURATION.M/60, 
#              RLen = dat2$EFFORT.DIS, NPho = dat3$NPIC, NAud = dat3$NSONG,
#              NAud2 = dat4$NSONGS, VegCover = VegCover, ArauCover = ArauCover, 
#              Alt = Altitude) #NS = dat2$NSPECIES, 

#inits = function() {list(z = rep(1, data2$nMuni))}#, beta = c(0.3), BETA = rep(0.01,7), alpha = rep(0,data$nMuni))} 
#params <- c("beta","psi","BETA","z", "muP1", "muP2", "muP3", "muP4")#, "spacesigma","alpha")

#mod2 = "G:/Meu Drive/Data Integration Models/data2019/Data Models/BasicEffortALLDataCOVS.txt"
#out2 <- bugs(data = data2, inits = inits, parameters.to.save = params, model.file = mod2,
#            n.chains = nc, n.iter = ni,n.burnin = nb, n.thin = nthin, debug = TRUE)

#save(out2, file = "BasicEffortALLDataCOVS28Mar.RData")


###############################################
### C. Researchers data + Wiki + Ebird + XC + CAR - todos os registros

#data4 <- list(muni1 = dat$IDENT, muni2 = dat2$IDENT, muni3 = dat3$ID, muni4 = dat4$ID, 
#              Y1 = dat$A_VINACEA, Y2 = dat2$A_VINACEA, Y3 = dat3$AVINACEA, Y4 = dat4$A_VINACEA, 
#              nMuni = length(muni), nObs1 = nrow(dat), nObs2 = nrow(dat2), 
#              nObs3 = nrow(dat3), nObs4 = nrow(dat4), 
#              EFFO = dat$EFFORT_MIN/60, eN = dat2$NSPECIES, eHours = dat2$DURATION.M/60, 
#              eKM = dat2$EFFORT.DIS, Pic = dat3$NPIC, Song = dat3$NSONG,
#              NSP = dat4$NSONGS,
#              nCell = nrow(hex.centroids), cell.id = cell.id, nMuni = length(muni), adj = adj, num = num, sumNeigh = sumNeigh) #NS = dat2$NSPECIES,
#              #VegCover = VegCover$vegCover, ArauCover = ArauCover$arauCover, Alt = Altitude$altitude,

#inits = function() {list(z = rep(1, data4$nMuni))}#, beta = c(0.3), BETA = rep(0.01,7), alpha = rep(0,data$nMuni))} 
#params <- c("beta","psi","BETA","z", "spacesigma","alpha","muP1", "muP2", "muP3", "muP4")

#mod4 = "G:/Meu Drive/Data Integration Models/data2019/Data Models/BasicEffortALLDataCAR.txt"
#out4 <- bugs(data = data4, inits = inits, parameters.to.save = params, model.file = mod4,
#             n.chains = nc, n.iter = ni,n.burnin = nb, n.thin = nthin, debug = TRUE)

#save(out4, file = "BasicEffortALLDataCAR.RData")



###############################################
### All Data - Cross-Validation

dataCV <- list(muni1 = dat$IDENT, muni2 = dat2$IDENT, muni3 = dat3$IDENT, muni4 = dat4$IDENT, muni5 = dat5$IDENT, 
               Y1 = dat$A_VINACEA, Y2 = dat2$A_VINACEA, Y3 = dat3$AVINACEA, Y4 = dat4$A_VINACEA, 
               nMuni = length(muni), nObs1 = nrow(dat), nObs2 = nrow(dat2), 
               nObs3 = nrow(dat3), nObs4 = nrow(dat4), nObs5 = nrow(dat5), 
               TObs = dat$EFFORT_MIN/60, SSee = dat2$NSPECIES, TObs2 = dat2$DURATION.M/60, 
               RLen = dat2$EFFORT.DIS, NPho = dat3$NPIC, NAud = dat3$NSONG,
               NAud2 = dat4$NSONGS, SSee2 = dat5$NSPECIES, TObs3 = dat5$DURATION.M/60, 
               RLen2 = dat5$EFFORT.DIS, VegCover = VegCover, ArauCover = ArauCover, 
               Alt = Altitude, nCell = nrow(hex.centroids), cell.id = cell.id, 
               nMuni = length(muni), adj = adj, num = num, sumNeigh = sumNeigh, Ytruth = dat5$A_VINACEA)#, n = nrow(dat5))

inits = function() {list(z = rep(1, dataCV$nMuni))}#, beta = c(0.3), BETA = rep(0.01,7), alpha = rep(0,data$nMuni))} 
#params <- c("beta", "BETA", "spacesigma", "Y5", "BRSC", "DEV")
params <- c("Y5", "BRSC", "DEV")

mod3 = "G:/Meu Drive/Data Integration Models/data2019/Data Models/Data2020/ALLDataCOVSCAR_ValidationScores.txt"
outCVset <- bugs(data = dataCV, inits = inits, parameters.to.save = params, model.file = mod3,
              n.chains = nc, n.iter = ni,n.burnin = nb, n.thin = nthin, debug = TRUE)

save(outCVset, file = "ALLDataCOVSCAR_Validation2.RData")



#### Compute fit scores:

Ytruth <- dat5$A_VINACEA
Yhat <- outCVset$mean$Y5

## Brier Score: 
BrierScore <- sum((Ytruth-Yhat)^2)*(1/length(Ytruth))

## Deviance:
likhood <- (Yhat^Ytruth)*((1-Yhat)^(1-Ytruth))
DEV <- -(2*(sum(log(likhood))))




#dataCAR <- list(muni1 = dat$IDENT, muni2 = dat2$ID, muni3 = dat3$ID, muni4 = dat4$ID, muni5 = dat5$IDENT,
#                Y1 = dat$A_VINACEA, Y2 = dat2$AVINACEA, Y3 = dat3$A_VINACEA, 
#                Y4 = dat4$A_VINACEA, Y5 = dat5$A_VINACEA, nMuni = length(muni), adj = adj, num = num, sumNeigh = sumNeigh,
#                nObs1 = nrow(dat), nObs2 = nrow(dat2), 
#                nObs3 = nrow(dat3), nObs4 = nrow(dat4), nObs5 = nrow(dat5), eN = dat$NSPECIES, eHours = dat$DURATION.M/60, eKM = dat$EFFORT.DIS,
#                NS = dat2$NSPECIES, Pic = dat2$NPIC, Song = dat2$NSONG,
#                EFFO = dat3$EFFORT_MIN/60, EFFORT = dat4$DURATION_M/60, NSP = dat5$Nrecords,
#                VegCover = VegCover$vegCover, ArauCover = ArauCover$arauCover, Alt = Altitude$altitude,
#                nCell = nrow(hex.centroids), cell.id = cell.id)

#dataCAR <- list(muni1 = dat$IDENT, muni2 = dat2$ID, muni5 = dat5$IDENT, Y1 = dat$A_VINACEA,
#               Y2 = dat2$AVINACEA, Y5 = dat5$A_VINACEA, nMuni = length(muni), adj = adj, num = num, sumNeigh = sumNeigh, nObs1 = nrow(dat),
#                nObs2 = nrow(dat2), nObs5 = nrow(dat5), eN = dat$NSPECIES, eHours = dat$DURATION.M/60, eKM = dat$EFFORT.DIS,
#                NS = dat2$NSPECIES, Pic = dat2$NPIC, Song = dat2$NSONG,  NSP = dat5$Nrecords,
#                VegCover = VegCover$vegCover, ArauCover = ArauCover$arauCover, Alt = Altitude$altitude,
#                nCell = nrow(hex.centroids), cell.id = cell.id)

#inits = function() {list(z = rep(1, data.Cov$nMuni), beta = rep(0,3), BETA = rep(0.01,9), alpha = alpha+rnorm(length(alpha),0,0.3))} #rep(0, data$nMuni))}
#params <- c("beta","psi","BETA","z", "spacesigma","alpha")

#inits = function() {list(z = rep(1,dataCAR$nMuni))}#, beta = c(0.3), BETA = rep(0.01,7), alpha = rep(0,data$nMuni))} 

#paramsCAR <- c("beta","psi","BETA","z", "spacesigma","alpha")

#### model is run in jags. the function inputs are nearly identical to R2WinBUGS command bugs

# mod = "G:/My Drive/Data Integration Models/data2019/Data Models/BasicEffortResearWiki.txt"
# 
# mod = "G:/My Drive/Data Integration Models/data2019/Data Models/BasicEffortResearWikiCAR.txt"
# 
# mod = "G:/My Drive/Data Integration Models/data2019/Data Models/BasicEffortResearWikiEbird.txt"
# 
# mod = "G:/My Drive/Data Integration Models/data2019/Data Models/BasicEffortResearWikiEbirdCAR.txt"
# 
# mod = "G:/My Drive/Data Integration Models/data2019/Data Models/BasicEffortResearWikiEbirdXC.txt"
# 
# mod = "G:/My Drive/Data Integration Models/data2019/Data Models/BasicEffortResearWikiEbirdXCCAR.txt"
# 
# mod = "G:/My Drive/Data Integration Models/data2019/Data Models/BasicEffortResearWikiEbirdXCCOV.txt"

# mod = "G:/My Drive/Data Integration Models/data2019/Data Models/BasicEffortResearWikiEbirdXCCOV.txt"

# mod = "G:/Meu Drive/Data Integration Models/data2019/Data Models/BasicEffortResearWikiEbirdXCCOVCARDave3.txt"

# mod = "G:/Meu Drive/Data Integration Models/data2019/Data Models/BasicEffortResearWikiEbirdXCCARnoCovs.txt"

mod = "G:/Meu Drive/Data Integration Models/data2019/Data Models/BasicEffortWikiEbirdXCCOVCAR.txt"

#out <- bugs(data = data, inits = inits, parameters.to.save = params, model.file = mod,
#            n.chains = nc, n.iter = ni,n.burnin = nb, n.thin = nthin, debug = TRUE)

outCAR<- bugs(data = dataCAR, inits = inits, parameters.to.save = paramsCAR, model.file = mod,
            n.chains = nc, n.iter = ni,n.burnin = nb, n.thin = nthin, debug = TRUE)

#outCAR_small <- bugs(data = dataCAR, inits = inits, parameters.to.save = paramsCAR, model.file = mod,
#                    n.chains = nc, n.iter = ni,n.burnin = nb, n.thin = nthin, debug = TRUE)

save(outCAR, file = "outBasicEffortResearWikiEbirdCOVCAR.RData")
#save(outCARstandar, file = "BasicEffortResearWikiEbirdXCCARnoCovs.RData")




mod = "C:/Users/vivia/Dropbox/Viviane Parrots/Models/BasicEffortCAR_ALLData.txt"

out3 <- bugs(data = data.NoCov, inits = inits.NoCov, parameters.to.save = params, model.file = mod,
            n.chains = nc, n.iter = ni,n.burnin = nb, n.thin = nthin, debug = TRUE)

mod2 = "C:/Users/vivia/Dropbox/Viviane Parrots/Models/BasicEffortCAR_ALLDataCOVS.txt"

out4 <- bugs(data = data.Cov, inits = inits, parameters.to.save = params, model.file = mod2,
             n.chains = nc, n.iter = ni,n.burnin = nb, n.thin = nthin, debug = TRUE)


#save.image()


plot(VegCover$VCover, out2$mean$psi, 
     xlab="% cobertura Mata Atl?ntica por munic?pio", ylab="Probabilidade de ocupa??o", cex.lab=1.3, cex.axis=1.2)

plot(ArauCover$ArCover, out2$mean$psi, 
     xlab="% cobertura Mata de Arauc?ria por munic?pio", ylab="Probabilidade de ocupa??o", cex.lab=1.3, cex.axis=1.2)



### plot the map ####

library(ggplot2)
library(rgeos)
library(maptools)
library(ggsn)

all_states <- map_data("world")
#pal <- brewer.pal(11,"RdGy")[11:1]

BR_AR_PAR<- all_states [all_states$region == "Paraguay" | all_states$region == "Brazil" | 
                          all_states$region == "Argentina" | all_states$region == "Bolivia" | 
                          all_states$region == "Uruguay" , ]
#crs(muni)<-crs(BR_AR_PAR)

psi <- out3$mean$psi
psi <- data.frame(seq=seq(1:3701), out3$mean$psi, out3$sd$psi, out3$mean$z, out3$sd$z)
rownames(psi) <- muni@data$IDENT

f.mun<-fortify(muni, region = "IDENT")
#f.mun$id<-as.numeric(as.factor(f.mun$id))

intervalos <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
cortes <- cut(psi$out3.mean.psi, intervalos, include.lowest=TRUE)

levels(cortes)[levels(cortes)== "[0,0.2]"]  <- "[0 - 0.2["
levels(cortes)[levels(cortes)== "(0.2,0.4]"]  <- "[0.2 - 0.4["
levels(cortes)[levels(cortes)== "(0.4,0.6]"]  <- "[0.4 - 0.6["
levels(cortes)[levels(cortes)== "(0.6,0.8]"]  <- "[0.6 - 0.8["
levels(cortes)[levels(cortes)== "(0.8,1]"]  <- "[0.8 - 1.0]"

psi<-cbind(psi, cores=cortes)

f.mun<-cbind(f.mun, cores=psi[match(f.mun$id, psi$seq),]$cores)

p = ggplot() + geom_polygon(data=BR_AR_PAR, aes(x=long, y=lat, group = group),colour="gray70",fill="gray90" ,alpha = 0.5) +
  coord_fixed (xlim=c(-60,-35),  ylim = c(-35,-7),ratio = 1)

b <- p +  geom_polygon(data=f.mun, aes(x=long, y=lat, group=group, fill=cores), colour = "black", size=0.5) + 
  #coord_fixed (xlim=c(-65,-35),  ylim = c(-36,-10),ratio = 1) +
  scale_fill_brewer(palette="YlOrRd", name="Occupancy probability")
b

c <- b + annotate(geom="text", x=-56, y=-34, label="URUGUAY",color="black",size=3) +
  annotate(geom="text", x=-63, y=-17, label="BOLIVIA",color="black",size=3)+
  annotate(geom="text", x=-46, y=-31, label="ATLANTIC OCEAN",color="black",size=3)

d <- b + ggsn::scalebar(f.mun, location = "bottomleft", dist = 250, st.dist=0.02,st.size=3, height=0.01, dd2km = TRUE, model = 'WGS84')

e <- d + theme_bw() + xlab("Longitude (Decimal degrees)") + ylab("Latitude (Decimal degrees)") +
  theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey80"), panel.grid.minor = element_blank())


##############################

# c?lculo da ?rea baseado no psi estimado #
### como extrair das simula??es os valores de psi ####






#### plot points in the map ####


map.out <- data.frame(x = coordinates(hex)[,1], y = coordinates(hex)[,2], alfa = out3$mean$alpha)
library(ggplot2);library(RColorBrewer)

all_states <- map_data("world")
pal <- brewer.pal(11,"RdGy")[11:1]
# world version:
p = ggplot() + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="gray70",fill="gray90" ,alpha = 1)
p = p + geom_polygon( data=muni, aes(x=long, y=lat, group = group),colour="gray50",fill="gray80" ,alpha = 1)
p = p +  coord_cartesian(ylim=c(-36,-5), xlim=c(-65,-35)) 
p = p + geom_point(data=map.out, shape = 19,aes(x=x,y=y,color = alfa), size = 1.5) 
p = p + scale_color_gradientn("estimated alpha",limits=c(-0.7,1), colours =pal[c(3,7,7,8,8,9,9,10,10,11,11)])
p1 = p + theme(legend.title = element_blank(),axis.ticks.x = element_blank(),
               axis.text.x = element_blank(),axis.ticks.y = element_blank(),
               axis.text.y = element_blank(), axis.title.x = element_blank(),
               axis.title.y = element_blank(), legend.position = c(0.9,0.24),
               panel.background = element_rect(fill = "darkseagreen4"),
               legend.text = element_text(size= 10, face = "bold", color = "gray50"))
p1
#write.table(map.out, file = "psi07_23.csv", row.names = TRUE)


##SD
map.out <- data.frame(x = coordinates(muni)[,1], y = coordinates(muni)[,2], psi = out$sd$psi)
library(ggplot2);library(RColorBrewer)

all_states <- map_data("world")
pal <- brewer.pal(6,"RdGy")[6:1]
# world version:
p = ggplot() + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="gray70",fill="gray90" ,alpha = 1)
p = p + geom_polygon( data=muni, aes(x=long, y=lat, group = group),colour="gray50",fill="gray80" ,alpha = 1)
p = p +  coord_cartesian(ylim=c(-36,-10), xlim=c(-65,-35)) 
p = p + geom_point(data=map.out, shape = 19,aes(x=x,y=y,color = psi), size = 1.5) 
p = p + scale_color_gradientn("estimated psi",limits=c(0,1), colours =pal[c(2,2,3,4,5,6)])
p1 = p + theme(legend.title = element_blank(),axis.ticks.x = element_blank(),
               axis.text.x = element_blank(),axis.ticks.y = element_blank(),
               axis.text.y = element_blank(), axis.title.x = element_blank(),
               axis.title.y = element_blank(), legend.position = c(0.9,0.24),
               panel.background = element_rect(fill = "darkseagreen4"),
               legend.text = element_text(size= 10, face = "bold", color = "gray50"))
p1


write.table(map.out, file = "data2.csv", row.names = TRUE)
