# Dashboard
library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library (rgdal)
require(sp)
require(raster)
require(here)

## carregar o shapefile dos municipios do RS
mymun <- readOGR (dsn=here("data","shape_munRS"), layer= "43MUE250GC_SIR",
                  use_iconv = T,encoding = "utf8")
mymun <- mymun [-c(96,250),]
## cobertura de campo
campo <- readOGR (dsn=here("Arthur_shiny" , "dashboardAPP"),
                  layer="arquivo_campo")
# inserir nomes dos muns
campo@data$NM_MUNICIP <- mymun$NM_MUNICIP

## colocar na mesma projecao
mymun_lambert <- spTransform (mymun, crs(campo))

## pallete of colors
bins <- c(0, 0.25, 0.5, 0.75, 1, Inf)
pal <- colorBin("YlOrRd", domain = campo$campo, bins = bins)

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

head <- dashboardHeader(title = em("Rhea Americana"))

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("General instructions", tabName ="Intro", icon = icon("info")),
    menuItem("Define the distribution", tabName ="Draw",icon = icon("map"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Intro", 
            includeMarkdown(here("Arthur_shiny",
                                  "dashboardAPP",
                                 "general_instructions.Rmd"))
            ),
  tabItem(tabName = "Draw",
          box(width = 12,
              uiOutput("text"),
              uiOutput("textout")),
          uiOutput("map1"),
          uiOutput('end.map1'),
          uiOutput("map2"),
          uiOutput('end.map2')))
  )


ui <- dashboardPage(head, sidebar , body,
                    skin = "green")
  
server <- function(input, output, session){
  values <- reactiveValues()
  values$name   <- NULL
  values$nameBT <- NULL
  values$map1.ok <- "ok"
  values$map2.ok <- "ok"

# Identificação do especialista -------------------------------------------

  output$text <- renderUI({
    req(is.null(values$nameBT))
    tagList(
      textInput("name", "Full name:"),
      actionButton("nameBT", "OK")
    )
    })
    
  observeEvent(input$nameBT,{
    values$name   <- input$name
    values$nameBT <- input$nameBT
    values$map1.ok <- NULL
  })

  output$textout <- renderUI({
    req(!is.null(values$nameBT))
    
    tagList(
      h3(strong("Name:"), values$name)
    )
  })

# Mapa 1 ------------------------------------------------------------------

  
  output$map1 <- renderUI({
    req(is.null(values$map1.ok))
    
    box(leafletOutput("leaf"),
        strong("Click here when you finished to draw poligons:"),
        actionButton("map1.ok", "OK"))
  })
  
  output$leaf <- renderLeaflet({
    
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
                                               , fillColor="blue"
                                               , bringToFront = FALSE)
                  , label = ~ NM_MUNICIP)
  })
  
  # observar o clique e usar a funcao definida fora para desenhar o poligono
  observeEvent(input$leaf_draw_all_features, {
    
    values$list.pol.df <- list()
    if(length(input$leaf_draw_all_features$features) == 0){return()}
    if(length(input$leaf_draw_all_features$features) >0){
      for(i in 1:length(input$leaf_draw_all_features$features)){
        values$list.pol.df[[i]] <- pol.coords(input$leaf_draw_all_features$features[[i]])
      }
    } 
    
  })
  
  observeEvent(input$map1.ok, {
    if(length(values$list.pol.df) == 1){
      values$pol <- make.polygon(values$list.pol.df[[1]])
    }
    
    
    if(length(values$list.pol.df) > 1){
      
      values$pol <- bind(lapply(values$list.pol.df, make.polygon))
    }
    shape <- values$pol
    df <- data.frame(nome.poligono = paste("poligono", 
                                           1:length(values$list.pol.df)))
    
    shape1 <- SpatialPolygonsDataFrame(shape, df)
    values$map1.ok <- "map 1 finished"
    values$text.end.map1 <- paste("You drew", length(shape1), "polygons")
    values$map2.ok <- NULL
  })
  
  output$end.map1 <- renderUI({
    req(values$map1.ok == "map 1 finished")
    
    box(width = 12,
        strong("You have finished the step 1"),
        h5(values$text.end.map1))
  })
  

# Mapa 2 ------------------------------------------------------------------

  output$map2 <- renderUI({
    req(is.null(values$map2.ok))
    
    box(leafletOutput("leaf2"),
        strong("Click here when you finished to draw poligons:"),
        actionButton("map2.ok", "OK"))
  })
  
  output$leaf2 <- renderLeaflet({
    
    leaflet(data=campo) %>% 
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
      ## grassland cover
      
      addPolygons(fillColor= ~ pal(campo)
                  , fillOpacity = 0.7
                  , color= "white"
                  , stroke= FALSE
                  , highlight=highlightOptions(weight=0.5
                                               , color="gray"
                                               , fillOpacity= 0.3
                                               , fillColor="blue"
                                               , bringToFront = T)
                  , label = ~ NM_MUNICIP)
  })
  
  # observar o clique e usar a funcao definida fora para desenhar o poligono
  observeEvent(input$leaf2_draw_all_features, {
    
    values$list.pol.df <- list()
    if(length(input$leaf2_draw_all_features$features) == 0){return()}
    if(length(input$leaf2_draw_all_features$features) >0){
      for(i in 1:length(input$leaf2_draw_all_features$features)){
        values$list.pol.df[[i]] <- pol.coords(input$leaf2_draw_all_features$features[[i]])
      }
    } 
    
  })
  
  observeEvent(input$map2.ok, {
    if(length(values$list.pol.df) == 1){
      values$pol <- make.polygon(values$list.pol.df[[1]])
    }
    
    
    if(length(values$list.pol.df) > 1){
      
      values$pol <- bind(lapply(values$list.pol.df, make.polygon))
    }
    shape <- values$pol
    df <- data.frame(nome.poligono = paste("poligono", 
                                           1:length(values$list.pol.df)))
    
    shape2 <- SpatialPolygonsDataFrame(shape, df)
    values$map2.ok <- "map 2 finished"
    values$text.end.map2 <- paste("You drew", length(shape2), "polygons")
    print(shape2)
  })
  
  output$end.map2 <- renderUI({
    req(values$map2.ok == "map 2 finished")
    
    box(width = 12,
        strong("Task Finished. Thank You!"),
        h5(values$text.end.map2))
  })  
  
}



shinyApp(ui, server)

