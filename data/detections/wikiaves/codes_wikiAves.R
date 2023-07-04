


# ------------------------------------------
# WikiAves data - already processed by V Zulian - filtered only data from Rio Grande do Sul
# ------------------------------------------

# load packages
source("R/packages.R")


## code to aggregate wikiaves data per year

dados <- read.csv (here("data","detections","wikiaves","DataWiki2008_2018.csv"),
                   h=T,sep=",")
# load RS map
shape_RS <- readOGR (dsn=here("data","shape_munRS"), layer = "43MUE250GC_SIR",
                     encoding="UTF-8", use_iconv=T)
# remove lakes
shape_RS <- shape_RS [-c (96,250),]

## COD_IBGE FOR MUNICIPALITIES MISSING DATA IN WIKIAVES
dados [is.na(dados$id),"id"] <- c(4301800, 4305603,4313334)

## municipality ids
MUN_nomes_corretos <- unlist(lapply (dados$id, function (i)
  shape_RS$NM_MUNICIP [which (shape_RS$CD_GEOCMU %in% i)]))

## correcting names 
dados$MUNI <- MUN_nomes_corretos

## bind data
dados <- rbind (dados,
                data.frame (X=NA,
                    id= shape_RS$CD_GEOCMU [which (shape_RS$NM_MUNICIP %in% dados$MUNI ==F)],
                    MUNI=shape_RS$NM_MUNICIP [which (shape_RS$NM_MUNICIP %in% dados$MUNI ==F)],
                    SITE="RS",
                    STATE = "RIO GRANDE DO SUL",
                    NSPECIES = 0,
                    NPIC = 0,
                    NSONG=0,
                    RHAMERICANA=0))

# matching names
dados_wikiaves <- dados [match(shape_RS$NM_MUNICIP,dados$MUNI),]

# plot
plot(shape_RS, col = rgb(dados_wikiaves$RHAMERICANA,0,1,alpha=0.5),
     main = "Wikiaves")


# save data
save (dados_wikiaves,file=here ("data","organized_data","input_wikiaves.RData"))

rm(list=ls())

                     


