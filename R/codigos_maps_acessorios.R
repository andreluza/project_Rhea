

##
# carregar shapefile RS
require(rgdal)
require(raster)

shape_RS <- readOGR(dsn=here("shape_munRS"), layer="43MUE250GC_SIR",
                    encoding = "UTF-8",use_iconv=T)## tem que ser use_iconv=T os nomes dos municipios terem acentos corretos

## remover as lagoas
#plot(shape_RS [-c(96,250),],col="red")
shape_RS <- shape_RS [-c(96,250),]

# plot de todos os pontos de amostragem, e dos pontos com Rhea 

plot(shape_RS,border="gray80",main="Dados GBIF")
points(dados_aves_filtrado$decimalLongitude,
       dados_aves_filtrado$decimalLatitude,
       pch=19,col="gray40")

## dados de Rhea americana
dados_rhea <- dados_aves_filtrado [which(dados_aves_filtrado$species == "Rhea americana"),]
points(dados_rhea$decimalLongitude,dados_rhea$decimalLatitude,
       col="red",pch=19,cex=1.3)

legend ("bottomleft", c ("Registros de R. americana", "Todos os registros"),
        pch=19,bty="n",cex=1,
        col=c("red","gray40"))

### gerando spatialpoints dataframe com os dados de EMA, para sobrepor com o shape do RS
coordenadas_ema <- dados_rhea [,c("decimalLongitude", "decimalLatitude")]
coordinates (coordenadas_ema) <- ~ decimalLongitude + decimalLatitude
crs(coordenadas_ema)<- crs(shape_RS)

# obter municipios com registro de ema
mun_ema <- over (coordenadas_ema,shape_RS)

# obter um mapa destes municipiso com ema
shape_mun_ema <- shape_RS [which (shape_RS@data$NM_MUNICIP %in% mun_ema$NM_MUNICIP),]

#plot(shape_RS,border="gray60",main="Dados GBIF")
plot(shape_mun_ema,add=T,border="red")

### obter um dataframe, com o cod do mun na linha, e a deteccao \ nao det de ema na coluna

det_ema <- data.frame (cod_mun = shape_RS@data$CD_GEOCMU,
                       det = ifelse (shape_RS@data$CD_GEOCMU %in% shape_mun_ema@data$CD_GEOCMU,1,0),
                       mun = shape_RS@data$NM_MUNICIP)

## abaixo, comando para conferir de seu certo
# det_ema$mun[which(det_ema$det == 1)] == shape_mun_ema@data$NM_MUNICIP

## agora, obter um dataframe com o numero de especies de aves registradas por municipio do RS
## para isto, primeiro criar um spatialpoint dataframe para as coordenadas de cada especie

## pegar a identidade de cada spp no banco de dados filtrado

especies_aves_RS <- unique (dados_aves_filtrado$species)

## entao fazer um subset dos dados completos, para pegar os pontos com o registro de cada especie
subconjunto_aves_RS <- lapply (especies_aves_RS, function (especie)
        dados_aves_filtrado [which (dados_aves_filtrado$species == especie),])

## subset do conjunto de coordenadas de cada sp. 

coordenadas_especies <- lapply (subconjunto_aves_RS, function (especie)
        especie [,c("decimalLongitude", "decimalLatitude")])

## transformar cada objeto da lista de coordenadas de cada especie,
# em spatialpoints dataframe
coordenadas_especies <- lapply (coordenadas_especies, function (especie) {
    coordinates (especie) <- ~ decimalLongitude + decimalLatitude
    crs(especie)<- crs(shape_RS); ## aproveitar para definir as projecao de referencia (do shapefile RS)
    especie # retornar o spdf de cada sp.

 }
)

# obter municipios com registro de cada especie
mun_det_especies <- lapply (coordenadas_especies,function (especie)
                            over (especie,shape_RS))

# obter um mapa destes municipiso com det de cada especie
shape_mun_especies <- lapply (mun_det_especies, function (especie)
        shape_RS [which (shape_RS@data$NM_MUNICIP %in% especie$NM_MUNICIP),])

#  finalmente, obter um dataframe, com o cod do mun na linha, 
# e a deteccao \ nao det de cada especie na coluna

det_especies <- lapply (shape_mun_especies, function (especie)
        data.frame (cod_mun = shape_RS@data$CD_GEOCMU,
                       det = ifelse (shape_RS@data$CD_GEOCMU %in% especie@data$CD_GEOCMU,1,0),
                       mun = shape_RS@data$NM_MUNICIP))

## colocar o nome de cada especie no respectivo dataframe com sua deteccao
names(det_especies) <- especies_aves_RS

## dissolver a lista com a deteccao de cada especie, e somar as linhas 
## para ter o numero de especies por municipio
df_todas_sp <- do.call (cbind, lapply (det_especies, function (especie) 
        especie$det))

## cotar o codigo do municipio como rownames desta tabela de todas as sp.
## como todas as tabelas dentro do objeto "det_especies" tem a mesma sequencia de codigos,
## uso os codigos da primeira tabela da lista = det_especies[[1]]$cod_mun
rownames(df_todas_sp) <- det_especies[[1]]$cod_mun

## obter a riqueza por municipio
riqueza_aves_mun_RS <- rowSums (df_todas_sp)

## A ordem dos municipios no vetor de riqueza e no shape RS esta igual (abaixo, comando para conferir)
## shape_RS$CD_GEOCMU == names(riqueza_aves_mun_RS)

## mapa para explorar as deteccoes de ema e a riqueza (esforco) nos municipios
cores <- data.frame (cores=riqueza_aves_mun_RS,
                     CD_GEOCMU=shape_RS@data$CD_GEOCMU)
require(ggplot2)
f.mun<-fortify(shape_RS, region="CD_GEOCMU")
f.mun<- cbind (f.mun, 
               Nespecies= cores [match (f.mun$id, cores$CD_GEOCMU),]$cores)

## carregar shapefiles da America do Sul, e dos lagos do RS

## shapefile lagos
lagos <- readOGR(dsn=here ("grandes_lagos_1_250000"),layer="Grandes_Lagos_1_250000",encoding = "latin1") ## "latin1"

## shape south america
southAme<- readOGR(dsn= here("South_America"),encoding="latin1", layer="South_America")
BR_AR_URU<- southAme [southAme@data$COUNTRY == "Paraguay" | southAme@data$COUNTRY == "Brazil" | southAme@data$COUNTRY == "Argentina" | southAme@data$COUNTRY == "Uruguay", ]
crs(munRS)<-crs(BR_AR_URU)

a <- ggplot() + geom_polygon (data=BR_AR_URU, aes(x=long, y=lat, group=group),size = 0.1, fill="gray90", colour="gray75",alpha=1) +
        coord_fixed (xlim = c(-57.5, -49),  ylim = c(-34, -27), ratio = 1) 

b <- a + geom_polygon(data=lagos, aes(x=long,y=lat, group=group),size=0.1, fill="lightcyan",colour="lightcyan",alpha=1)

c <-   b + geom_polygon(data=f.mun, aes(x=long, y=lat, group=group, 
                                   color=Nespecies, fill=Nespecies), colour = NA, size=1) + 
        labs (title= "GBIF - Riqueza de aves, e registros de Ema (em preto),\npor município")+
        #scale_fill_manual(values=c("white","gray70","black"),name="Existem amostras\ne detecções?")# values = c(),na.value="white",
        scale_fill_gradient2 (low='white', high='darkred', midpoint=40, 
                              limits=c(0,372), 
                              breaks=seq(0,372,by=70),
                              name="Número de\nespécies") ## para continuo

c1 <- c + geom_polygon(data=shape_mun_ema, 
                       aes(x=long,y=lat, group=group,fill=NULL),
                       colour="black",alpha=0, # alpha 0 deixa transparente
                       linetype = 1,size=1) 

d<-c1 + annotate(geom="text", x=-56, y=-32, label="URUGUAI",color="black",size=3) +
        annotate(geom="text", x=-56.5, y=-27.8, label="ARGENTINA",color="black",size=3)+
        annotate(geom="text", x=-56.5, y=-27, label="PARAGUAI",color="black",size=3)+
        annotate(geom="text", x=-51.8, y=-27, label="Santa Catarina",color="black",size=3)+
        annotate(geom="text", x=-50.5, y=-32.5, label="OCEANO ATLANTICO",color="black",size=3)

e <- d + ggsn::scalebar(f.mun, dist = 100, st.dist=0.02,st.size=1, 
                      height=0.01, transform = TRUE, dist_unit="km", 
                      model = 'WGS84', location = "bottomright")

f<-e + theme(panel.background = element_rect(fill = "lightcyan", colour = "lightcyan", size = 0.5, linetype = "solid")) + 
        xlab("Longitude (graus decimais)") + ylab("Latitude (graus decimais)") +
        theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        #theme(legend.position = "none")+
        theme(plot.title = element_text(size=10),
              axis.text=element_text(size=4),
              axis.text.x = element_text(size=5),
              axis.title.x = element_text(size = 7),
              axis.text.y = element_text(size=5),
              axis.title.y = element_text(size = 7),
              legend.text=element_text(size=7),
              legend.title=element_text(size=8),
              plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "lines"))

f_north <- f + ggsn::north(f.mun, symbol=1) 

f_north

########################
## os codigos dos municipios da riqueza fecham  com os cod das deteccoes de ema
#names (riqueza_aves_mun_RS) ==  det_ema$cod_mun

dados_det_ema_gbif <- cbind (det_ema, riqueza_aves= riqueza_aves_mun_RS)


############## BAIXAR DADOS DE OUTRAS BASES

# bases for download
db <- c(#"gbif",       # Global Biodiversity Information Facility (https://www.gbif.org/)
        #"ecoengine",  # Berkeley Initiative for Global Change Biology (https://ecoengine.berkeley.edu/)
        "inat",       # iNaturalist (https://www.inaturalist.org/)
        "vertnet",    # VertNet (http://vertnet.org/)
        "ebird",      # eBird (https://ebird.org/)
        "idigbio"    # Integrated Digitized Biocollections (https://www.idigbio.org/)
        #"obis",       # Ocean Biogeographic Information System (www.iobis.org)
        #"ala",        # Atlas of Living Australia (https://www.ala.org.au/)
        #"bison"       # Biodiversity Information Serving Our Nation (https://bison.usgs.gov)
)

db

# occ download
require (spocc)

bounds<- c(extent (shape_RS) [1],extent (shape_RS) [2],extent (shape_RS) [3],extent (shape_RS) [4])

occ <- lapply (as.character(especies_aves_RS), function (i)
                  occ(query = i, 
                  from = db, 
                  ebirdopts = list(key = "6dvjcvmgf2es"), # make key in https://ebird.org/api/keygen
                  has_coords = TRUE, 
                  limit = 1e5, geometry=bounds))
                  
## poligono definido aqui: https://arthur-e.github.io/Wicket/sandbox-gmaps3.html 

save(occ, file="avesRS_varias_DB.RData")

#'Rhea americana'
occ_to_df <- lapply (occ, function (i)
    occ2df(i,what="all")) #https://www.rdocumentation.org/packages/spocc/versions/1.0.2/topics/occ2df

unique (unlist(lapply (occ_to_df, function (i) unique (i$data$prov))))






