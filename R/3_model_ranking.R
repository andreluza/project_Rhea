
require (here) # para transitar entre pastas
require(vegan) # para padronizar os dados
library(R2WinBUGS) # para rodar o modelo 


# -----------------------
#### Comparando performance de diferentes modelos ####

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
require(pROC)
model_sel <- lapply (model_list, function (i) {
  
  ## 1) Calcular a Deviance
  # separar os dados observados (teste) - GBIF
  Ytruth <- datGBOut[,"det"]
  # separar os dados preditos/estimados pelo modelo - GBIF
  Yhat <- i$mean$y.gbif6
  
  # separar os dados observados (teste) - Wikiaves
  Ytruth2 <- datWAOut[, "RHAMERICANA"]
  # separar os dados preditos/estimados pelo modelo - Wikiaves
  Yhat2 <- i$mean$y.wikiaves9
  
  # separar os dados observados (teste) - eBird
  Ytruth3 <- datEBOut[,1:n_so_ebird]
  # separar os dados preditos/estimados pelo modelo - eBird
  Yhat3 <- i$mean$yEB10
  
  # separar os dados observados (teste) - INat
  Ytruth4 <- datINTOut [,'det']
  # separar os dados preditos/estimados pelo modelo - eBird
  Yhat4 <- i$mean$y.inat7
  
  # separar os dados observados (teste) - VertNet
  Ytruth5 <-datVEROut [,"det"]
  # separar os dados preditos/estimados pelo modelo - eBird
  Yhat5 <- i$mean$y.vertnet8
  
  ## Deviance em cada base de dados:
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
  DEVtotal <- DEV + DEV2 + DEV3 + DEV4 + DEV5 # valor que deve ser comparado entre modelos.
  # modelo com o menor valor de deviance tem 
  # o melhor ajuste aos dados.
  
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

## 2) Plotar curvas AUC

deviance_sel <- sapply (model_sel,"[[","deviance")
# data.frame (deviance_sel[order(deviance_sel)])

roc_crit <- sapply (model_sel,"[[","roc")

save(deviance_sel,file=here ("output","deviance.RData"))




