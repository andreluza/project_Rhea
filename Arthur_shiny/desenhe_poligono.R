library(shiny)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library (rgdal)
require(sp)
require(raster)
require(here)

## carregar o shapefile dos municipios do RS
mymun <- readOGR (dsn=here("data","shape_munRS"), layer= "43MUE250GC_SIR")

## funcao interna para desenhar os poligonos
pol.coords <- function(input.polig){
  
  pol.coords <- data.frame(x=numeric(),y=numeric())
  total <- length(input.polig$geometry$coordinates[[1]])
  long.pol <- numeric()
  lat.pol <- numeric()
  for (i in 1:total){
    long.pol <- input.polig$geometry$coordinates[[1]][[i]][[1]]
    lat.pol <- input.polig$geometry$coordinates[[1]][[i]][[2]]
    
    
    coords <- data.frame(x=long.pol,y=lat.pol)
    
    pol.coords <- rbind(pol.coords, coords)
    
  }
  
  pol.coords 
}

make.polygon <- function(df){
  
  # and then something like this
  sp <- SpatialPolygons(list(Polygons(list(Polygon(df)), 1)))
  sp
  
}

ui <- fluidPage(
  titlePanel("Desenhe um polígono"),
  
  sidebarLayout(
    sidebarPanel(
      helpText(h2("Instruções:"),
               br(),
               "Insira pontos no mapa para desenhar um polígono
               ligando os pontos onde você detectou ", em("Rhea americana"), 
               ".", 
               br(),
               br(),
               "Você pode desenhar quantos polígonos forem necessários. Para
               finalizar um polígono, você deve clicar no ponto onde iniciou o polígono.
               Tome cuidado para que seu polígono não sobreponha um município
               onde você não registrou a espécie.", 
               "Para se localizar no mapa, você verá o nome do município ao passar
               o mouse sobre o mapa do RS. Esta função fica desativada enquanto você está 
               desenho o polígono, para que não atrapalhe na visualização.",
               br(),
               "Na parte superior e direita do mapa você verá duas camadas de mapa disponíveis:
               StreetMap e Satelite. Explore estas opções para se localizar melhor.",
               br(),
               "Ao final do processo de desenho do polígono, clique em 'Salvar'.
               Três arquivos com o nome 'arquivo_poligono_Rhea' serão salvos automaticamente. 
               Os formatos são '.shp', 'dbf', e 'shx'. Por favor, envie os três arquivos
               para o email luza.andre@gmail.com"),
      actionButton("ok", "Salvar")
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)


server <- function(input, output, session){
  values <- reactiveValues()
  
  output$map <- renderLeaflet({
    
    leaflet(data=mymun) %>% 
      setView(lng = -52.8, lat = -30.5, zoom = 6) %>%
      # Add two tiles
      addTiles(options = providerTileOptions(noWrap = TRUE),group="StreetMap")%>%
      addProviderTiles("Esri.WorldImagery", group="Satelite")  %>%
      addDrawToolbar(
        targetGroup='Markers',
        polylineOptions = F,
        polygonOptions = T, 
        circleOptions = F,
        rectangleOptions = F,
        markerOptions = F,
        circleMarkerOptions = F,
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())) %>%
      addLayersControl( baseGroups = c("StreetMap","Satelite"),
                        options = layersControlOptions(collapsed = TRUE)) %>%
      
      addPolygons(fillColor="white"
                  , fillOpacity = 0.1
                  , color= "white"
                  , stroke= FALSE
                  , highlight=highlightOptions(weight=0.5
                                               , color="gray"
                                               , fillOpacity= 0.3
                                               , fillColor="white"
                                               , bringToFront = FALSE)
                  , label = ~ NM_MUNICIP)
    
  })
  
  ## observe passar o mouse sobre os polygonos
  observe ({
    click <- input$map_shape_click 
    if (is.null (click))
      return ()
    else 
      leafletProxy ("map") %>%
      setView (lng=click$lng, lat= click$lat, zoom = 8)
  })
  
  # observar o clique e usar a funcao definida fora para desenhar o poligono
  observeEvent(input$map_draw_all_features, {
    
    values$list.pol.df <- list()
    if(length(input$map_draw_all_features$features) == 0){return()}
    if(length(input$map_draw_all_features$features) >0){
      for(i in 1:length(input$map_draw_all_features$features)){
        values$list.pol.df[[i]] <- pol.coords(input$map_draw_all_features$features[[i]])
      }
    } 
    
  })
  
  observeEvent(input$ok, {
    if(length(values$list.pol.df) == 1){
      values$pol <- make.polygon(values$list.pol.df[[1]])
    }
    
    
    if(length(values$list.pol.df) > 1){
      
      values$pol <- bind(lapply(values$list.pol.df, make.polygon))
    }
    shape <- values$pol
    df <- data.frame(nome.poligono = paste("poligono", 1:length(values$list.pol.df)))
    
    shape <- SpatialPolygonsDataFrame(shape, df)
    
    writeOGR(shape, ".", "arquivo_poligono_Rhea", driver="ESRI Shapefile")
  })

  
}


shinyApp(ui,server)


