# Dashboard
library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(sp)
library(raster)
library(here)

## carregar o shapefile dos municipios do RS
mymun <- readOGR (dsn=here(), layer= "MyMun",
                  use_iconv = T,encoding = "utf8")

## transformar zero de cobertura de campo em NA, para ficar na cor adequada
mymun$campo[which(mymun$campo == 0)] <- NA

## pallete of colors
bins <- c(0,0.25,0.5,0.75,1)
pal <- colorBin("YlOrRd", domain = mymun$campo, bins = bins,
                na.color="white")

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

short.name <- function(input.name){
  name.split <- strsplit(input.name, " ")[[1]]
  
  last.name <- name.split[length(name.split)]
  abbrev <- paste0(sapply(name.split, substr, 1, 1)[-length(name.split)], collapse = "")
  
  f.name <- paste(last.name, abbrev, sep = "_")
  return(f.name)
}

head <- dashboardHeader(title = em("Rhea americana"))

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("General instructions", tabName ="Intro", icon = icon("info")),
    menuItem("Define the distribution", tabName ="Draw",icon = icon("map"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Intro", 
            includeMarkdown(here("general_instructions.Rmd"))
            ),
  tabItem(tabName = "Draw",
          box(width = 12,
              uiOutput("text"),
              uiOutput("textout")),
          uiOutput("map1"),
          uiOutput('end.map1'),
          uiOutput("map2"),
          uiOutput('end.map2'),
          uiOutput("send.result")))
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

  #configura a UI inicial 
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

  # Apresenta o nome fornecido
  output$textout <- renderUI({
    req(!is.null(values$nameBT))
    
    tagList(
      h3(strong("Name:"), values$name)
    )
  })

# Mapa 1 ------------------------------------------------------------------

  #configura a UI do mapa 1
  output$map1 <- renderUI({
    req(is.null(values$map1.ok))
    
    box(width = 12,
        column(width = 3,
               "Use the map features to draw as many polygons as you need 
                        to describe your knowlegde on", em("Rhea americana"),
                        "distribution in the State of Rio Grande do Sul. 
                        Use the function ",strong("Draw a polygon"), "to start drawing.",
               "Move the mouse over the RS map to see the name of the municipalities.
               One particular polygon must end at its initial point.",
               br(),
               br(),
               strong("Click here when you finished drawing poligons:"),
               actionButton("map1.ok", "OK")),
        
        column(width = 9,
               leafletOutput("leaf"))
        )
  })
  
  #mapa 1
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
                        options = layersControlOptions(collapsed = FALSE)) %>%
      
      addPolygons(fillColor="white", 
                  fillOpacity = 0.1,
                  color= "white", 
                  stroke= FALSE, 
                  highlight=highlightOptions(weight=0.5,
                                             color="gray",
                                             fillOpacity= 0.3,
                                             fillColor="blue",
                                             bringToFront = FALSE),
                  label = ~ NM_MUNICIP)
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
  
  # guardar poligonos desenhados
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
    
    values$shape1 <- SpatialPolygonsDataFrame(shape, df)
    values$map1.ok <- "map 1 finished"
    plural <- ifelse(length(values$shape1) > 1, "polygons", "polygon")
    values$text.end.map1 <- paste("You drew", length(values$shape1), plural)
    values$map2.ok <- NULL
  })
  
  # Configura UI de fim da primeira etapa
  output$end.map1 <- renderUI({
    req(values$map1.ok == "map 1 finished")
    
    box(width = 12,
        strong("You have finished the step 1"),
        h5(values$text.end.map1))
  })
  

# Mapa 2 ------------------------------------------------------------------
  
  
  #configura a UI do mapa 2
  output$map2 <- renderUI({
    req(is.null(values$map2.ok))
    
    box(width = 12,
        column(width = 3, 
               helpText(h5("Use the map features to draw as many polygons as you need 
                        to describe your knowlegde on", em("Rhea americana"),
                           "distribution in the State of Rio Grande do Sul.
                           Use the function ",strong("Draw a polygon"), "to start drawing.",
                           "Now, enjoy the help of the layer with grassland cover!
                           Remember that one particular polygon must end at its initial point.")),
               br(),
               br(),
               strong("Click here when you finished drawing poligons:"),
               actionButton("map2.ok", "OK")
               ),
        column(width = 9,
               radioButtons("opacity", 
                            "Change layer opacity:", 
                            choiceNames = list("low","high"),
                            choiceValues = list(0.25, 0.50),
                            selected = 0.50,
                            inline = T),
               leafletOutput("leaf2")
               )
        )
  })
  
  #mapa 2
  output$leaf2 <- renderLeaflet({
    
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
                        options = layersControlOptions(collapsed = FALSE)) %>%
      ## grassland cover
      addPolygons(fillColor= ~pal(campo), 
                  fillOpacity = input$opacity,
                  color= "white", 
                  stroke= FALSE, 
                  highlight=highlightOptions(weight=0.1,
                                             color="grey",
                                             fillOpacity= 0.3,
                                             fillColor="blue",
                                             bringToFront = FALSE),
                  label = ~ NM_MUNICIP
                  ) %>% 
      addLegend(position = "bottomright",
                labels = c("0",">0-25", ">25-50", ">50-75", ">75-100"),
                colors = c("white",pal(seq(0,0.75,0.25))),
                opacity = input$opacity,
                title = "Grassland cover (%)") 
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
  
  # guardar poligonos desenhados
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
    
    values$shape2 <- SpatialPolygonsDataFrame(shape, df)
    values$map2.ok <- "map 2 finished"
    plural <- ifelse(length(values$shape2) > 1, "polygons", "polygon")
    values$text.end.map2 <- paste("You drew", length(values$shape2), plural)
  })
  
  # Configura UI de fim da primeira etapa
  output$end.map2 <- renderUI({
    req(values$map2.ok == "map 2 finished")
    
    box(width = 12,
        strong("Task Finished!"),
        h5(values$text.end.map2))
  })  

# Download ----------------------------------------------------------------

  #Configura UI de Download
  output$send.result <- renderUI({
    req(values$map2.ok == "map 2 finished")
    
    box(width = 12,
        strong("Thank you for sharing your knowledge with us!"),
        br(),
        strong("Please, download the file with the results and mail it to "),
        code("luza.andre@gmail.com"),
        strong(" with the subject "), em("Expert knowledge about greater Rhea distribution"),
        br(),
        br(),
        downloadButton("download"))
    
    
    
  })
  
  #Prepara objetos e salva o arquivo
  observeEvent(values$map2.ok,{
    if(values$map2.ok == "map 2 finished"){
      data <- list(Name = input$name,
                   Shape_Mun = values$shape1,
                   Shape_campo = values$shape2)
      crs (data$Shape_Mun) <- "+proj=longlat +ellps=GRS80 +no_defs" #
      crs (data$Shape_campo) <- "+proj=longlat +ellps=GRS80 +no_defs"#
      
      exp.name <- short.name(data$Name)
    }
  
    
    output$download <- downloadHandler(
      
      filename = function() {
        paste(exp.name, "_",  Sys.Date(), '.rds', sep='')
      },
      content = function(con) {
        saveRDS(data, con)
      })
  })
  
  
  
}



shinyApp(ui, server)

