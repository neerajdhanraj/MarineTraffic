###################################################
# Information
###################################################
# Date: 30-10-2021
# Author: Dr. Neeraj Dhanraj Bokde
# Email: neerajdhanraj@gmail.com
# Title: Marine Traffic - Dashboard to demonstrate journey of ships
# Description: This shiny app demonstrates the history of several
#      ships used for different purposes and shows the longest journey
#      between two consecutive observations. 
###################################################

###################################################
# Packages
###################################################
library(shiny)
library(shiny.semantic)
library(shiny.info)
library(shiny.worker)
library(data.table)
library(leaflet)
library(geosphere)
library(highcharter)
library(dplyr)
#library(wordcloud2)
#library(memoise)
#library(fst)
###################################################

###################################################
# Data, Preprocessing and CSS
###################################################
data <- fread('data/ships.csv')
#data <- read_fst('data/ships.csv')
worker <- initialize_worker()

loader_css <- "
#spinner {
display: inline-block;
border: 3px solid #f3f3f3;
border-top: 3px solid #3498db;
border-radius: 50%;
width: 40px;
height: 40px;
animation: spin 1s ease-in-out infinite;
}
@keyframes spin {
0% { transform: rotate(0deg); }
100% { transform: rotate(360deg); }
}"

#data <- data[, .(LAT, LON, ship_type, DATETIME)]

## Function for longest distance###################
find_min <- function(args) {
  # v is data.table( LON, LAT )
  v <- args$n
  max_ind <- 1
  max_dist <- -1
  for (i in 1:(nrow(v) - 1)) {
    # browser()
    dist <- as.numeric(distm(c(v$LON[i], v$LAT[i]), c(v$LON[(i + 1)], v$LAT[(i + 1)])))
    if (dist >= max_dist && v$FLAG[i]==v$FLAG[i+1]) {
      max_ind = i
      max_dist = dist
      #print(v[max_ind:(max_ind+1),])
    }
  }
  return(
    v[max_ind:(max_ind+1),]
  )
}

#find_min <- memoise(find_min)
###################################################

###################################################
# UI
###################################################
ui <- semanticPage(
  tags$style(loader_css),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  #useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css"),
  ),
  grid(
    grid_template = grid_template(
      default = list(
        areas = rbind(
          c("title", "titleh"),
          c("info", "map"),
          c("user", "map")
        ),
        cols_width = c("370px", "1fr"),
        rows_height = c("200px", "300px", "700px", "700px")
      ),
      mobile = list(
        areas = rbind(
          "title",
          "titleh",
          "map",
          "info",
          "user"
        ),
        rows_height = c("70px", "400px", "250px", "200px"),
        cols_width = c("100%")
      )
    ),
    area_styles = list(title = "margin: 20px;", info = "margin: 20px;", user = "margin: 20px;"),
    title = header(title = "Marine Traffic", description = "Demonstrating journey of ships", icon = "ship"),
    titleh = uiOutput("hbar"),
    info = uiOutput("sidebar"),
    user = shiny.info::display(
      "Designed by Dr. Neeraj Dhanraj Bokde (neerajdhanraj@gmail.com).
      Note: The database is huge! By default, this dashboard shows results for initinal 20000 observations for each ship types.
                                You can access the full database analysis by clicking on button in 'Database' tab.",
      position = "bottom right"
    ),
    map = leafletOutput('map1', width = "100%", height="100%")
  )
)

###################################################
# Server
###################################################
server <- function(input, output) {

  tog <- reactive({
    input$action_button
  })

  selected_data <- reactive({
      input$selectType
      input$action_button
      req(input$selectType)
      if(tog()) {
        #browser()
        list(n = data[ship_type == input$selectType])
      } else {
        list(n = data[ship_type == input$selectType][1:20000])
      }
  })
  
  # Job for shiny.worker
  max_dist_points1 <- worker$run_job("max_dist_points1",
                                    find_min,
                                    args_reactive = selected_data)
  barCache <- reactiveVal(c(0, 0))
  
  # To Render Map
  output$map1 <- renderLeaflet({
    if (!is.null(max_dist_points1()$result)) {
      barCache(max_dist_points1()$result) # Update Cache
    }
    cv <- max_dist_points1()$result
    req(cv)
    dt <- as.numeric(distm(c(cv$LON[1], cv$LAT[1]), c(cv$LON[(2)], cv$LAT[(2)])))
    max_dist_points <- max_dist_points1()$result
    leaflet() %>%
      addProviderTiles(
        provider = "Esri.WorldTopoMap",
        options = providerTileOptions(maxZoom = 19)
      ) %>%
      # addCircleMarkers(
      #   lng = ~LON, lat = ~LAT,
      #   fill = T, stroke = F,
      #   fillOpacity = 0.7, 
      #   data = selected_data()
      # ) %>%
      addMarkers(
        lng = ~LON, lat = ~LAT,
        popup = ~paste('<b>Ship type:</b>', ship_type, '<br>',
                       '<b>Timestamp:</b>', DATETIME, '<br>',
                       '<b>Longitude:</b>', LON, '<br>',
                       '<b>Latitude:</b>', LAT, '<br>',
                       '<b>Distance:</b>', paste(round((dt),2))),  #
        label = ~paste("Longitude:", LON, "Latitude:", LAT),
        data = max_dist_points1()$result,
      )
  })
  
  # To render ship images
  output$shipsImage <- renderUI({
    img(src= paste0(input$selectType, ".png"), align = "left")
  })
  
  # To render Notes
  observeEvent(input$selectType,{
    if (!is.null(max_dist_points1()$result)) {
      barCache(max_dist_points1()$result) # Update Cache
    }
    output$info <- renderUI({
      img(src= paste0(selected_data()$n$FLAG[1], ".png"), align = "center")
      })
    output$Ldistance <- renderUI({
      cv <- max_dist_points1()$result
      req(cv)
      dt <- round(as.numeric(distm(c(cv$LON[1], cv$LAT[1]), c(cv$LON[(2)], cv$LAT[(2)]))),2)
      str1 <- paste("Maximum traveled distance:")
      str2 <- paste(tags$h2(style="color:red", paste(dt, "Meters"))) #max_dist_points1()$result['max_dist']
      HTML(paste(str1, str2, sep = '<br/>'))
    })
    output$Ldate <- renderUI({
      str1 <- paste("Traveled on:")
      str2 <- paste(tags$h2(style="color:red", max_dist_points1()$result$date[1]))
      HTML(paste(str1, str2, sep = '<br/>'))
    })
    output$Lid <- renderUI({
      str1 <- paste("The ship's name:")
      str2 <- paste(tags$h2(style="color:red", paste0(max_dist_points1()$result$SHIPNAME[1])))
      HTML(paste(str1, str2, sep = '<br/>'))
    })
    # output$wordcld <- renderWordcloud2({
    #   text <- selected_data()$SHIPNAME
    #   df <- data.frame(table(text))
    #   df <- df[order(df$Freq, decreasing = TRUE),]
    #   # set.seed(1234) # for reproducibility 
    #   # wordcloud(words = df$text, freq = df$Freq, min.freq = 1,
    #   #           random.order=FALSE, rot.per=0.35,max.words=200,
    #   #           colors=brewer.pal(8, "Dark2"))
    #   #wordcloud2(data=df)#, size = 0.5, shape = 'pentagon')
    # })
  })
  
  # To render Pie chart
  output$pie1 <- renderUI({
    r0 <- 0
    r1 <- 1:10
    r2 <- 11:50
    r3 <- 51:100
    r4 <- 101:300
    r5 <- 301:1000
    r6 <- 1001:2000
    d5 <- data.frame(selected_data()$n$SPEED)
    names(d5) <- "S"
    d5 <- d5 %>%
      mutate(task = case_when(S %in% r1 ~ "1 to 10", S %in% r2 ~ "11 to 50", S %in% r3 ~ "51 to 100",
                              S %in% r4 ~ "101 to 300", S %in% r5 ~ "301 to 1000",
                              S %in% r6 ~ "1001 to 2000", S %in% r0 ~ "< 1"))
    Utasks <- NULL
    for (i in 1:length(unique(d5$task))) {
      Utasks[i] <- length(which(d5$task == unique(d5$task)[i]))
    }
    Utasks <- round(Utasks/sum(Utasks) *100, 2)
    
    df <- data.frame(
      x = 0:(length(unique(d5$task))-1),
      y = Utasks,
      name = as.factor(unique(d5$task))
    )
    hc <- df %>%
      hchart(
        "pie", hcaes(x = name, y = y),
        name = "Speed Category Share (%)"
      ) %>%
      hc_size(300, 300)
    hc
  })
  
  output$sidebar <- renderUI({
    grid(
      grid_template = grid_template(default = list(
        areas = rbind(
          c("status", "status"),
          c("gauge", "gauge"),
          c("plot", "plot"),
          c("settings", "settings")
        ),
        cols_width = c("50%", "50%"),
        rows_height = c("120px", "240px", "400px", "300px", "300px","auto")
      )),
      # Ship selector
      #area_styles = list(gauge1 = "padding-right: 5px", gauge2 = "padding-left: 5px"),
      status = div(div(class="ui center aligned header", " "),
                   segment(selectInput('selectType', div(class = "ui teal ribbon label",h3('Select Ship Type:')), unique(data$ship_type)),)),
      # Ship image
      #style = "border-radius: 0; width: 100%; height: 150px",
      gauge =  div(div(class="ui center aligned header", " "),
                   segment(uiOutput("shipsImage"),)),
      # Pie chart
      plot = card(
        style = "border-radius: 0; width: 100%; background: #efefef",
        div(class = "optionsSection",
            segment(div(class = "ui teal ribbon label",
            h3("Speed of ships (Knots):")),
            div(class="ui center aligned header",
                  uiOutput("pie1"),)
            )
        )
      ),
      # Loader
      settings = div(class = "optionsSection",
                    uiOutput("loader"),
      ),
    )
  })
  
  # To render Notes
  output$hbar <- renderUI({
    grid(
      grid_template = grid_template(default = list(
        areas = rbind(
          c("status1", "status2", "status3", "status4", "status5"),
          c("status1", "status2", "status3", "status4", "status5")
        ),
        cols_width = c("22%", "22%","22%", "22%"),
        rows_height = c("210px","210px","210px","210px")
      )),
      area_styles = list(status1 = "padding-right: 25px", status2 = "padding-right: 25px",
                         status3 = "padding-right: 25px", status4 = "padding-right: 25px",
                         status5 = "padding-right: 5px"),
      status1 = div(div(class = "optionsSection",
                        segment(div(class = "ui teal ribbon label", style="height:100%;",
                              h3("Origin:")),
                              h2(" "),
                              div(class="ui center aligned header",
                              uiOutput("info"))))),
      status2 = div(class = "optionsSection",
                        segment(div(class = "ui teal ribbon label",
                              h3("Ship Identifier:")),
                              h2(" "),
                              div(class="ui center aligned header",
                              htmlOutput("Lid")))),
      status3 = div(class = "optionsSection",
                        segment(div(class = "ui teal ribbon label",
                              h3("Date:")),
                              h2(" "),
                              div(class="ui center aligned header",
                              htmlOutput("Ldate")))),
      status4 = div(class = "optionsSection",
                        segment(div(class = "ui teal ribbon label", style="height:100%;",
                              h3("Distance:")),
                              h2(" "),
                              div(class="ui center aligned header",
                              htmlOutput("Ldistance")))),
      status5 = div(class = "optionsSection",
                        segment(div(class = "ui teal ribbon label",
                              h3("Database:"),),
                              h3("Use Full Database"),
                              toggle(
                                input_id = 'action_button',
                                label = NULL,
                                is_marked = F
                              ),
                        )),)
  })

  # To render loader/spinner
  output$loader <- renderUI({
    task <- max_dist_points1()
    if (!task$resolved) {
      div(
        div(class = "loader-text", "Please wait while data is loading..."),
        div(id = "spinner"),
      )
    }
  })
  
  # To render range selector
  # output$slider <- renderUI({
  #   div(class="ui center aligned header",
  #       h4("Database is Huge!"),
  #       h4("You may select a subset of Database:"),
  #   sliderInput("range", "Range:", min = 1, max = 1000, value = c(200,500)))
  # })
  

  
}

# Run the application 
shinyApp(ui = ui, server = server)
