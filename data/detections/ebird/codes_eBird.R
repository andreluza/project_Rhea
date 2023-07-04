

# ------------------------------------------
# eBird data - already processed by V Zulian - filtered only data from Rio Grande do Sul
# ------------------------------------------

# load packages
source("R/packages.R")

ebirdDataRS <- read.csv (here("data","detections","ebird","ebirdDataRS.csv"), h=T,
                         encoding = "UTF-8")

# number of species in complete or incomplete lists

ggplot (ebirdDataRS, aes (x=as.factor(ALL.SPECIES.REPORTED),y=NSPECIES)) + 
  geom_boxplot(fill= "gray") +
  xlab ("Type of list (0= incomplete, 1=complete)")+
  ylab ("Number of detected species")+
  theme_bw()


# checks of incomplete lists (results in the letter to reviewers)
# histogram
ebirdDataRS %>%
  filter (ALL.SPECIES.REPORTED == 0) %>%
  distinct (NSPECIES) %>%
  ggplot(aes (NSPECIES))+
  geom_histogram()

# summaries
table(ebirdDataRS %>%
             filter (ALL.SPECIES.REPORTED == 0) %>%
             distinct (NSPECIES) <=10)
ebirdDataRS %>%
  filter (ALL.SPECIES.REPORTED == 0) %>%
  distinct (NSPECIES) %>%
  mutate(NSPECIES, nspn=as.numeric(NSPECIES)) %>%
  summarise(mean(NSPECIES),
            sd(NSPECIES),
            range(NSPECIES))

## codes to obtain sampling effort and detection
# load study region map
shape_RS <- readOGR(dsn=here ("data","shape_munRS"), layer="43MUE250GC_SIR",
                    encoding = "UTF-8",use_iconv = T)

## remove lakes
shape_RS <- shape_RS [-c(96,250),]

## extract ebird coordinates
all_coord <-  ebirdDataRS[,c("LONGITUDE", "LATITUDE")]

# transform coord into  spatialpoints DF
coordinates (all_coord) <- ~ LONGITUDE + LATITUDE
crs(all_coord) <- crs(shape_RS)

## overlap coordinates over RS map to extract detection
over_coord <- over (all_coord,shape_RS)
ebirdDataRS$NM_MUNICIP <- over_coord$NM_MUNICIP ## add municipality id 
ebirdDataRS <- ebirdDataRS [which(is.na(ebirdDataRS$NM_MUNICIP)!=T),] # remove empty results

# number of municipalities if incomplete lists were removed
ebirdDataRS %>% 
  filter (ALL.SPECIES.REPORTED == 1) %>%
  filter (R_AMERIC == 1) %>%
  select (NM_MUNICIP) %>%
  distinct()

## check the years with many lists
#par(mar=c(5,5,5,1))
#barplot(table(as.numeric(substr (ebirdDataRS$OBSERVATION.DATE,1,4))),
#  xlab="Ano", ylab="Numero de listas",
#  las=2)
## range (ebirdDataRS$OBSERVATION.DATE)

## data frame with municipality (site) ID
ID_MUN <- data.frame (NM_MUNICIP = shape_RS$NM_MUNICIP,
                      ID_MUN = seq (1, length(shape_RS$NM_MUNICIP)))

## ID of each municipality
ID_MUN_ebird <- unlist (lapply (ebirdDataRS$NM_MUNICIP, function (i)
  ID_MUN$ID_MUN [which (ID_MUN$NM_MUNICIP == i )]))

## paste ID in the table with eBird data
ebirdDataRS$ID_MUN <- ID_MUN_ebird

### now we will extract data of each municipality using lapply
## 1st - create a list of municipalities, to
## 2nd - have the eBird data for each municipality
municipios <- unique (ebirdDataRS$NM_MUNICIP) # list of municipalities mentioned in the data

## interesting variables : list ID, sampling duration (minutes),
                                # effort distance (km)
                                # number of observers that produced each list
                                # detection of Rhea americana
                                # municipality 
				# number of species detected in each list
                               
ebirdDataRS_mun <- lapply (municipios, function (i)
  ebirdDataRS [which(ebirdDataRS$NM_MUNICIP == i),c("GLOBAL.UNIQUE.IDENTIFIER",
									"DURATION.MINUTES",
                  							"EFFORT.DISTANCE.KM",
									"NUMBER.OBSERVERS",
									"R_AMERIC",
									"NSPECIES",
									"NM_MUNICIP")]
  )
## create a continuous ID for each list in a municipality
ebirdDataRS_mun <- lapply (ebirdDataRS_mun, function (i)
 		cbind (i, cont_lista= seq (1,nrow(i))))


################################
####### DURATION.MINUTES #######
################################

## build a table (mun x list)
mun_list <- lapply (ebirdDataRS_mun, function (i) 
			cast (NM_MUNICIP ~ cont_lista,
				data= i,value="DURATION.MINUTES",na.rm=T, FUN=sum))

## bind missing lists
mun_list <- lapply (mun_list, function (i)
	cbind (i,
	matrix (rep(NA,length(seq (ncol(i), max(unlist(lapply (mun_list,ncol)))))), # add tantas colunas quanto as listas inexistentes
		byrow=F,nrow=1, # 1 municipio
		dimnames=list(NULL, 
			seq (ncol(i), max(unlist(lapply (mun_list,ncol)))))))
	)

## bind missing municipalities
para_add <- data.frame (matrix (rep (NA, length(shape_RS$NM_MUNICIP [which (shape_RS$NM_MUNICIP %in% unlist(lapply (mun_list, function (i) i$NM_MUNICIP ))==F)]) * ncol(mun_list[[1]])),
		nrow=length(shape_RS$NM_MUNICIP [which (shape_RS$NM_MUNICIP %in% unlist(lapply (mun_list, function (i) i$NM_MUNICIP ))==F)]),
		byrow=T))
para_add [,1] <- shape_RS$NM_MUNICIP [which (shape_RS$NM_MUNICIP %in% unlist(lapply (mun_list, function (i) i$NM_MUNICIP ))==F)]
colnames (para_add) <- c ("NM_MUNICIP",seq (1,ncol (para_add)-1))

## df with data
df_mun_duration <- do.call (rbind,mun_list)# melt the list to produce a dataframe
df_mun_duration  <- rbind (df_mun_duration, para_add) # bind missing municipalities (rows)
df_mun_duration <- df_mun_duration [match(shape_RS$NM_MUNICIP, df_mun_duration$NM_MUNICIP),] # order
rownames (df_mun_duration) <- df_mun_duration$NM_MUNICIP # set rownames
df_mun_duration <- df_mun_duration [,-1] # rm frst column 1 (municipality name)

##################################
####### EFFORT.DISTANCE.KM #######
##################################

## build the table
mun_list <- lapply (ebirdDataRS_mun, function (i) 
			cast (NM_MUNICIP ~ cont_lista,
				data= i,value= "EFFORT.DISTANCE.KM",na.rm=T, FUN=sum))

## bind missing data
mun_list <- lapply (mun_list, function (i)
	cbind (i,
	matrix (rep(NA,length(seq (ncol(i), max(unlist(lapply (mun_list,ncol)))))),
		byrow=F,nrow=1,
		dimnames=list(NULL, 
			seq (ncol(i), max(unlist(lapply (mun_list,ncol)))))))
	)

## missing municipalities
para_add <- data.frame (matrix (rep (NA, length(shape_RS$NM_MUNICIP [which (shape_RS$NM_MUNICIP %in% unlist(lapply (mun_list, function (i) i$NM_MUNICIP ))==F)]) * ncol(mun_list[[1]])),
		nrow=length(shape_RS$NM_MUNICIP [which (shape_RS$NM_MUNICIP %in% unlist(lapply (mun_list, function (i) i$NM_MUNICIP ))==F)]),
		byrow=T))
para_add [,1] <- shape_RS$NM_MUNICIP [which (shape_RS$NM_MUNICIP %in% unlist(lapply (mun_list, function (i) i$NM_MUNICIP ))==F)]
colnames (para_add) <- c ("NM_MUNICIP",seq (1,ncol (para_add)-1))

## dataframe with complete data
df_mun_distance <- do.call (rbind,mun_list)
df_mun_distance <- rbind (df_mun_distance, para_add)
df_mun_distance <- df_mun_distance [match(shape_RS$NM_MUNICIP, df_mun_distance$NM_MUNICIP),]
rownames (df_mun_distance) <- df_mun_distance$NM_MUNICIP
df_mun_distance <- df_mun_distance [,-1]

##################################
####### NUMBER.OBSERVERS   #######
##################################

## the table
mun_list <- lapply (ebirdDataRS_mun, function (i) 
			cast (NM_MUNICIP ~ cont_lista,
				data= i,value= "NUMBER.OBSERVERS",na.rm=T, FUN=sum))

## missing lists
mun_list <- lapply (mun_list, function (i)
	cbind (i,
	matrix (rep(NA,length(seq (ncol(i), max(unlist(lapply (mun_list,ncol)))))),
		byrow=F,nrow=1,
		dimnames=list(NULL, 
			seq (ncol(i), max(unlist(lapply (mun_list,ncol)))))))
	)

## missing municipalities
para_add <- data.frame (matrix (rep (NA, length(shape_RS$NM_MUNICIP [which (shape_RS$NM_MUNICIP %in% unlist(lapply (mun_list, function (i) i$NM_MUNICIP ))==F)]) * ncol(mun_list[[1]])),
		nrow=length(shape_RS$NM_MUNICIP [which (shape_RS$NM_MUNICIP %in% unlist(lapply (mun_list, function (i) i$NM_MUNICIP ))==F)]),
		byrow=T))

para_add [,1] <- shape_RS$NM_MUNICIP [which (shape_RS$NM_MUNICIP %in% unlist(lapply (mun_list, function (i) i$NM_MUNICIP ))==F)]
colnames (para_add) <- c ("NM_MUNICIP",seq (1,ncol (para_add)-1))

## df with data
df_mun_observers <- do.call (rbind,mun_list)
df_mun_observers <- rbind (df_mun_observers, para_add)
df_mun_observers <- df_mun_observers [match(shape_RS$NM_MUNICIP, df_mun_observers$NM_MUNICIP),]
rownames (df_mun_observers) <- df_mun_observers$NM_MUNICIP
df_mun_observers <- df_mun_observers [,-1]

################################
####### NSPECIES ###############
################################

## table
mun_list <- lapply (ebirdDataRS_mun, function (i) 
  cast (NM_MUNICIP ~ cont_lista,
        data= i,value="NSPECIES",na.rm=T, FUN=sum))

## missing lists
mun_list <- lapply (mun_list, function (i)
  cbind (i,
         matrix (rep(NA,length(seq (ncol(i), max(unlist(lapply (mun_list,ncol)))))), # add tantas colunas quanto as listas inexistentes
                 byrow=F,nrow=1,
                 dimnames=list(NULL, 
                               seq (ncol(i), max(unlist(lapply (mun_list,ncol)))))))
)

## missing municipalities
para_add <- data.frame (matrix (rep (NA, length(shape_RS$NM_MUNICIP [which (shape_RS$NM_MUNICIP %in% unlist(lapply (mun_list, function (i) i$NM_MUNICIP ))==F)]) * ncol(mun_list[[1]])),
                                nrow=length(shape_RS$NM_MUNICIP [which (shape_RS$NM_MUNICIP %in% unlist(lapply (mun_list, function (i) i$NM_MUNICIP ))==F)]),
                                byrow=T))

para_add [,1] <- shape_RS$NM_MUNICIP [which (shape_RS$NM_MUNICIP %in% unlist(lapply (mun_list, function (i) i$NM_MUNICIP ))==F)]
colnames (para_add) <- c ("NM_MUNICIP",seq (1,ncol (para_add)-1))

## data frame with data
df_mun_species <- do.call (rbind,mun_list)
df_mun_species  <- rbind (df_mun_species, para_add)
df_mun_species <- df_mun_species [match(shape_RS$NM_MUNICIP, df_mun_species$NM_MUNICIP),]
rownames (df_mun_species) <- df_mun_species$NM_MUNICIP
df_mun_species <- df_mun_species [,-1]


#####################################
###### RHEA AMERICANA    ############
#####################################

## rhea detection table
mun_list <- lapply (ebirdDataRS_mun, function (i) 
			cast (NM_MUNICIP ~ cont_lista,
				data= i,value= "R_AMERIC",na.rm=T, FUN=sum))

## missing lists
mun_list <- lapply (mun_list, function (i)
	cbind (i,
	matrix (rep(NA,length(seq (ncol(i), max(unlist(lapply (mun_list,ncol)))))),
		byrow=F,nrow=1,
		dimnames=list(NULL, 
			seq (ncol(i), max(unlist(lapply (mun_list,ncol)))))))
	)

## missing municipalities (no sampling)
para_add <- data.frame (matrix (rep (NA, length(shape_RS$NM_MUNICIP [which (shape_RS$NM_MUNICIP %in% unlist(lapply (mun_list, function (i) i$NM_MUNICIP ))==F)]) * ncol(mun_list[[1]])),
		nrow=length(shape_RS$NM_MUNICIP [which (shape_RS$NM_MUNICIP %in% unlist(lapply (mun_list, function (i) i$NM_MUNICIP ))==F)]),
		byrow=T))

para_add [,1] <- shape_RS$NM_MUNICIP [which (shape_RS$NM_MUNICIP %in% unlist(lapply (mun_list, function (i) i$NM_MUNICIP ))==F)]
colnames (para_add) <- c ("NM_MUNICIP",seq (1,ncol (para_add)-1))

## complete dataframe
df_mun_rhea <- do.call (rbind,mun_list)
df_mun_rhea <- rbind (df_mun_rhea, para_add)
df_mun_rhea <- df_mun_rhea [match(shape_RS$NM_MUNICIP, df_mun_rhea$NM_MUNICIP),]
rownames (df_mun_rhea) <- df_mun_rhea$NM_MUNICIP
df_mun_rhea <- df_mun_rhea [,-1]

## plot
plot (shape_RS, main = "eBird", 
      col = rgb (rowSums(df_mun_rhea,na.rm=T)>0,0,1,alpha=0.5),
      border = "black")


# format table
y.ebird <- unname (as.matrix (df_mun_rhea))


summary (

    unlist(
  
  
        lapply (seq (1,nrow(y.ebird)), function (i) 
  
      
            length(y.ebird [i,] [which (is.na (y.ebird [i,]) != T)])

    
            )
  
        )

    )

# y.ebird [is.na(y.ebird)] <- 0



## standardization and names
dist.ebird <- as.matrix(df_mun_distance)
dist.ebird [is.na(dist.ebird)] <-0
dist.ebird <- unname(dist.ebird)
dist.duration <- as.matrix(df_mun_duration)
dist.duration [is.na(dist.duration)] <-0
dist.duration <- unname(dist.duration)
dist.observers <- as.matrix(df_mun_observers)
dist.observers [is.na(dist.observers)] <-0  
dist.observers <- unname(dist.observers)

# save
save (y.ebird, dist.ebird, dist.duration, dist.observers, ID_MUN,
      file=here ("data","organized_data","INPUT_ebird.RData"))


rm(list=ls())


