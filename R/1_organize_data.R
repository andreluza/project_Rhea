#------------------------------------------------------------------------#
# organizing data to modeling (in batch)
# run this will save data into the folder "organized_data", within data
# each source will provide a map

# load packages
source("R/packages.R")

# open a pallette for six maps
par(mfrow=c(3,2),mar=c(0,0,1,1))

# run code to organize and save expert knowledge data
source("data/expert_opinion/expert.R")

# run code to organize and save GBif data
source("data/detections/Gbif/codes_GBIF.R")

# run code to organize and save Inaturalist data
source("data/detections/outras_bases_INAT/codes_iNaturalist.R") 

# run code to organize and save vertnet data
source("data/detections/vertnet/codes_VertNet.R")

# run code to organize and save wikiaves data
source("data/detections/wikiaves/codes_wikiAves.R")

# run code to organize and save EBird data
source("data/detections/ebird/codes_eBird.R")

# -------------------------------------------------
# site and spatial covariates

# this can take a little because it is needed to extract data from landcover maps
source("data/covariates/codes_organization_covariates.R")




