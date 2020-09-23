library(shiny)
library(shinyalert)
library(leaflet)
library(leaflet.extras)
library (rgdal)
require(sp)
require(raster)
require(here)
## carregar o shapefile dos municipios do RS
mymun <- readOGR (dsn=here(), layer= "43MUE250GC_SIR")


## criar a UI
ui <- fluidPage(
  titlePanel("Registros de especialistas"),
  sidebarLayout(
    sidebarPanel(
      helpText(h2("Instruções:"),
               br(),
               "Insira pontos no mapa para indicar onde 
               você detectou ", em("Rhea americana"), 
               ".", 
               br(),
               br(),
               "Como a análise será feita por município,
               você pode 1) indicar um único ponto dentro de um município que 
               você detectou a espécie, ou 2) indicar as várias localidades 
               onde você detectou a espécie dentro de um município.", 
               "Você verá o nome do município ao passar o mouse sobre o mapa do 
               RS.",
               "Use isto para se localizar no mapa, e então indicar os pontos de 
               detecção da espécie.",
               br(),
               "A cada novo ponto adicionado, uma janela se abrirá. 
               Insira um valor de 0 a 10 para a confiança no ponto de 
               detecção que você registrou", 
               br(),
               br(),
               "Clique em 'ok' quando um ponto for inserido, e em 'Baixe um arquivo .csv'
               quando terminar de inserir pontos."),
      actionButton("feito", "ok"),
      downloadButton(outputId = "download_table"
                     , label = "Baixe um arquivo .csv")
    ),
    mainPanel(
      leafletOutput("map"),
      verbatimTextOutput("teste"),
      useShinyalert()
    )
  )
)

server <- function(input, output, session){
  values <- reactiveValues()
  
  output$map <- renderLeaflet({
    
    leaflet(data=mymun) %>% #
      setView(lng = -52.8, lat = -30.5, zoom = 6) %>%
      # Add two tiles
      addTiles(options = providerTileOptions(noWrap = TRUE),group="StreetMap")%>%
      addProviderTiles("Esri.WorldImagery", group="Satelite")  %>%
      addDrawToolbar(
        targetGroup='Markers',
        polylineOptions = F,
        polygonOptions = F, 
        circleOptions = F,
        rectangleOptions = F,
        markerOptions = F,
        circleMarkerOptions = T,
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
      setView (lng=click$lng, lat= click$lat, zoom = 6)
  })
  
  ## observe o ato de indicar pontos
  observeEvent(input$map_draw_all_features, {
    if(length(input$map_draw_all_features$features) == 1){
      shinyalert(
        title = "Nível de confiança",
        text = "Atribua um valor de 0 a 10 para sua confiança no ponto de detecção",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "input",
        inputType = "number",
        inputValue = "5",
        inputPlaceholder = "num",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      
      values$length.feat <- length(input$map_draw_all_features$features)
      values$conf <- input$shinyalert
    }
    
    if(length(input$map_draw_all_features$features) > 1){
      
      if(length(input$map_draw_all_features$features) > values$length.feat){
        
        shinyalert(
          title = "Nível de confiança",
          text = "Atribua um valor de 0 a 10 para sua confiança no ponto de detecção",
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "input",
          inputType = "number",
          inputValue = "5",
          inputPlaceholder = "num",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#AEDEF4",
          timer = 0,
          imageUrl = "",
          animation = TRUE
        )
        
        values$length.feat <- length(input$map_draw_all_features$features)
        values$conf <- c(values$conf,input$shinyalert)
      }
    
    }
    
  })
  
  observeEvent(input$feito, {
    output$teste <- renderPrint({
      ll <- lapply(input$map_draw_all_features$features, function(x){
       lat <- x$geometry$coordinates[[2]]
       long <- x$geometry$coordinates[[1]]
       c(lat,long)
       })
      data <- as.data.frame(do.call(rbind, ll))
      
      values$coors_data <- data
      values$coors_data 
      
      conf <- c(values$conf,input$shinyalert)
      
      values$df.conf <- cbind(data, conf)
      
     list(data, values$df.conf)
    })
  })
  
  #output$my_table <- renderTable ({values$df.conf})
  
  
  # saving CSV
  output$download_table  <- downloadHandler(
    
    
    filename = function() {
      paste("file_EMA_", format(Sys.time(), "%d-%m-%Y"), ".csv", sep = "")
    },
    content = function(file) {
      save.table <- values$df.conf
      colnames(save.table) <- c("lat", "long", "confianca")
      write.csv(save.table, file, row.names = FALSE)
    }
    
  )
  
}

shinyApp(ui, server)

