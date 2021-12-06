#------------------------------------------------------------------------#
# organizing data to modeling (in batch)
# run this will save data into the folder "organized_data", within data
# each source will provide a map

# open a pallette for six maps
par(mfrow=c(3,2),mar=c(0,0,1,1))

# run code to organize and save expert knowledge data
source("data/expert_opinion/expert.R")

# run code to organize and save GBif data
source("data/deteccoes/Gbif/codigos.R")

# run code to organize and save Inaturalist data
source("data/deteccoes/outras_bases_INAT/codigos.R") 

# run code to organize and save vertnet data
source("data/deteccoes/vertnet/extract_vertnet_data.R")

# run code to organize and save wikiaves data
source("data/deteccoes/wikiaves/codigos_wiki.R")

# run code to organize and save EBird data
source("data/deteccoes/ebird/codigos_ebird.R")

# -------------------------------------------------
# site and spatial covariates

# this can take a little because it is needed to extract data from landcover maps
source("data/covariaveis/codigos_org_covariaveis.R")




