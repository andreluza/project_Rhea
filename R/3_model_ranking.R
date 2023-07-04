
# -----------------------
# Comparing models
# -------------------------------------------------
# load packages
source ("R/packages.R")

load(here("output", "out_null_model1.RData"))
load(here("output", "out_model2.RData"))
load(here("output", "out_grass_model3.RData"))
load(here("output", "out_grass_agri_model4.RData"))
load(here("output", "out_model5.RData"))
load(here("output", "out_complete_model6.RData"))

# load validation data
load(here("output", "validation_data.RData"))
n_so_ebird<-11
#list modeels
model_list <- list(null=out_null_model1,
                   spatial10=out_model2[[1]],
                   spatial25=out_model2[[2]],
                   spatial50=out_model2[[3]],
                   grass_none=out_grass_model3,
                   complete_none=out_grass_agri_model4,
                   grass_aut10 = out_model5[[1]],
                   grass_aut25 = out_model5[[2]],
                   grass_aut50 = out_model5[[3]],
                   complete10 = out_complete_model6[[1]],
                   complete25 = out_complete_model6[[2]],
                   complete50 = out_complete_model6[[3]]
)

# roc curve
# deviance

model_sel <- lapply (model_list, function (i) {
  
  ## 1) Calculate Deviance
  # separate observed data (test) - GBIF
  Ytruth <- datGBOut[,"det"]
  # separate predicted/estimated by the model - GBIF
  Yhat <- i$mean$y.gbif6
  
  # separate observed data (test) - Wikiaves
  Ytruth2 <- datWAOut[, "RHAMERICANA"]
  # separate predicted/estimated by the model - Wikiaves
  Yhat2 <- i$mean$y.wikiaves9
  
  # separate observed data (test)  - eBird
  Ytruth3 <- datEBOut[,1:n_so_ebird]
  # separate predicted/estimated by the model - eBird
  Yhat3 <- i$mean$yEB10
  
  # separate observed data (test)  - INat
  Ytruth4 <- datINTOut [,'det']
  # separate predicted/estimated by the model - INat
  Yhat4 <- i$mean$y.inat7
  
  # separate observed data (test)  - VertNet
  Ytruth5 <-datVEROut [,"det"]
  # separate predicted/estimated by the model - VertNet
  Yhat5 <- i$mean$y.vertnet8
  
  ## Deviance of each dataset:
  # GBIF
  likhood <- (Yhat^Ytruth)*((1-Yhat)^(1-Ytruth))
  DEV <- -(2*(sum(log(likhood))))
  
  # Wikiaves
  likhood2 <- (Yhat2^Ytruth2)*((1-Yhat2)^(1-Ytruth2))
  lokLik2<- log(likhood2)
  lokLik2 <- lokLik2[is.infinite(lokLik2) == F]
  DEV2 <- -(2*(sum(lokLik2)))
  
  # eBird
  likhood3 <- (Yhat3^Ytruth3)*((1-Yhat3)^(1-Ytruth3))
  lokLik3<- log(likhood3)
  lokLik3 <- lokLik3[is.na(lokLik3) == F]
  DEV3 <- -(2*(sum(lokLik3)))
  
  # INat
  likhood4 <- (Yhat4^Ytruth4)*((1-Yhat4)^(1-Ytruth4))
  lokLik4<- log(likhood4)
  lokLik4 <- lokLik4[is.na(lokLik4) == F]
  DEV4 <- -(2*(sum(lokLik4)))
  
  # vertnet
  likhood5 <- (Yhat5^Ytruth5)*((1-Yhat5)^(1-Ytruth5))
  lokLik5<- log(likhood5)
  lokLik5 <- lokLik5[is.na(lokLik5) == F]
  DEV5 <- -(2*(sum(lokLik5)))
  
  # Deviance total:
  DEVtotal <- DEV + DEV2 + DEV3 + DEV4 + DEV5 # this value must be compared among models
  # the model with the lowest deviance = most fitted to the data
  
  # ROC
  # gbif
  roc_res <- data.frame (gbif = roc(Ytruth, Yhat)$auc,
                         wikiaves = roc(Ytruth2, Yhat2)$auc,
                         ebird = roc(as.numeric(Ytruth3), as.numeric(Yhat3))$auc,
                         inat = roc(Ytruth4, Yhat4)$auc
                         #vertnet =  roc(Ytruth5, Yhat5)$auc
  )
  
  
  # list of results
  res<-list (roc = roc_res,
             deviance= round(DEVtotal,2)
             )
  ;
  res
})

## 2) Plot AUC curves

deviance_sel <- sapply (model_sel,"[[","deviance")
# data.frame (deviance_sel[order(deviance_sel)])

# OBS: not possible to test deviance for VertNet data, as validation 
# dataset did not have detections (the data is too sparse)

roc_crit <- sapply (model_sel,"[[","roc")

save(deviance_sel,file=here ("output","deviance.RData"))

rm(list=ls())



