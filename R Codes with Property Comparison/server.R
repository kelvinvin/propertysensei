library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(plotly)
library(leaflet)
library(shinyjs)
library(DT)

rent_listings <- read.csv("Data/rent_listings_data_cleaned.csv", stringsAsFactors = T)
sales_listings <- read.csv("Data/sales_listings_data_cleaned.csv", stringsAsFactors = T)
mall <- read.csv("Data/mall_coordinates_updated.csv", stringsAsFactors = T)
mrt <- read.csv("Data/mrtsg.csv", stringsAsFactors = T)
schools <- read.csv("Data/primaryschoolsg.csv", stringsAsFactors = T)


shinyServer(function(input, output, session) {
  output$rent_table <- DT::renderDataTable({
    DT::datatable(rent_listings[-1], escape = FALSE, filter="top", selection = "none")
  })
  
  output$sales_table <- DT::renderDataTable({
    DT::datatable(sales_listings[-1], escape = FALSE, filter="top", selection = "none")
  })
  
  datasetInput <- reactive({
    if (input$trends_tab == "Rent") {
      return(rent_listings)
    } else if (input$trends_tab == "Sales") {
      return(sales_listings)
    }
  })
  
  axis_vars <- reactive({
    vars_to_show = common_axis_vars
    if (input$trends_tab == "Rent") {
      vars_to_show = c(vars_to_show, rent_axis_vars)
    } else if (input$trends_tab == "Sales") {
      vars_to_show = c(vars_to_show, sales_axis_vars)
    }
    return(vars_to_show)
  })
  
  properties <- reactive({
    skipMin_PSF <- is.null(input$min_PSF) || input$min_PSF == 0
    skipMax_PSF <- is.null(input$max_PSF) || input$max_PSF == 0
    skipMin_Price <- is.null(input$min_price) || input$min_price == 0
    skipMax_Price <- is.null(input$max_price) || input$max_price == 0
    skipMin_Builtup <- is.null(input$min_area_builtup) || input$min_area_builtup == 0
    skipMax_Builtup <- is.null(input$max_area_builtup) || input$max_area_builtup == 0
    skipMin_Land <- is.null(input$min_area_land) || input$min_area_land == 0
    skipMax_Land <- is.null(input$max_area_land) || input$max_area_land == 0
    
    datasetInput() %>% filter(
      Built.Year. >= input$year[1],
      Built.Year. <= input$year[2],
      Bathrooms. >= input$bathrooms[1],
      Bathrooms. <= input$bathrooms[2],
      Bedrooms. >= input$bedrooms[1],
      Bedrooms. <= input$bedrooms[2],
      ## Allow NA values if there is no min or max filter set
      if (skipMin_PSF) T else PSF. >= input$min_PSF,
      if (skipMax_PSF) T else PSF. <= input$max_PSF,
      if (skipMin_Price) T else Asking. >= input$min_price,
      if (skipMax_Price) T else Asking. <= input$max_price,
      if (skipMin_Builtup) T else Area.Builtup >= input$min_area_builtup,
      if (skipMax_Builtup) T else Area.Builtup <= input$max_area_builtup,
      if (skipMin_Land) T else Area.Land >= input$min_area_land,
      if (skipMax_Land) T else Area.Land <= input$max_area_land,
      is.null(input$district) | District. %in% input$district,
      is.null(input$floors) | Floor.Description %in% input$floors,
      is.null(input$property_types) | Property.Type. %in% input$property_types,
      is.null(input$tenure) | Tenure. %in% input$tenure
    )
  })
  
  observeEvent(input$trends_tab, {
    initial_yvar = input$yvar
    initial_xvar = input$xvar
    initial_color = input$color
    initial_size = input$size
    
    if (initial_yvar %in% c(sales_axis_vars, rent_axis_vars)) {
      initial_yvar = 'PSF.'
    }
    if (initial_xvar %in% c(sales_axis_vars, rent_axis_vars)) {
      initial_xvar = 'Built.Year.'
    }
    if (initial_color %in% c(sales_axis_vars, rent_axis_vars)) {
      initial_color = 'District.'
    }
    if (initial_size %in% c(sales_axis_vars, rent_axis_vars)) {
      initial_size = 'Area.Builtup'
    }
    
    updateSelectizeInput(session, "xvar", choices = axis_vars(),
                         selected = initial_xvar, server = TRUE)
    updateSelectizeInput(session, "yvar", choices = axis_vars(),
                         selected = initial_yvar, server = TRUE)
    updateSelectizeInput(session, "color", choices = axis_vars(),
                         selected = initial_color, server = TRUE)
    updateSelectizeInput(session, "size", choices = axis_vars(),
                         selected = initial_size, server = TRUE)
  })
  
  observe({
    district <-
      datasetInput() %>%
      `$`('District.') %>% levels()
    stillSelected <- isolate(input$district[input$district %in% district])
    updateSelectizeInput(session, "district", choices = c(district),
                         selected = stillSelected, server = TRUE)
  })
  
  observe({
    property_types <- filter(datasetInput(), 
                             is.null(input$district) | District. %in% input$district) %>%
      `$`('Property.Type.') %>% 
      droplevels() %>% levels()
    stillSelected <- isolate(input$property_types[input$property_types %in% property_types])
    updateSelectizeInput(session, "property_types", choices = c(property_types),
                         selected = stillSelected, server = TRUE)
  })
  
  observe({
    floors <- filter(datasetInput(), 
                     is.null(input$district) | District. %in% input$district,
                     is.null(input$property_types) | Property.Type. %in% input$property_types) %>%
      `$`('Floor.Description') %>% 
      droplevels() %>% levels()   
    stillSelected <- isolate(input$floors[input$floors %in% floors])
    updateSelectizeInput(session, "floors", choices = c(floors),
                         selected = stillSelected, server = TRUE)
  })
  
  observe({
    tenures <- filter(datasetInput(), 
                      is.null(input$district) | District. %in% input$district,
                      is.null(input$property_types) | Property.Type. %in% input$property_types,
                      is.null(input$floors) | Floor.Description %in% input$floors) %>%
      `$`('Tenure.') %>% 
      droplevels() %>% levels()   
    stillSelected <- isolate(input$tenure[input$tenure %in% tenures])
    updateSelectizeInput(session, "tenure", choices = c(tenures),
                         selected = stillSelected, server = TRUE)
  })
  
  property_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x["X"])) return(NULL)
    
    all_properties <- isolate(properties())
    property <- all_properties[all_properties$X == x$X, ]
    paste0("<b>", property$Property.Name, "</b><br>",
           "Property Type: ", property$Property.Type., "<br>",
           "Built Year: ", property$Built.Year, "<br>",
           "Floor: ", property$Floor.Description, "<br>",
           "Asking: $", format(property$Asking., big.mark = ",", scientific = FALSE), "<br>"
    )
  }
  
  output$plot <- renderPlotly({
    # Labels for axes
    xvar_name <- names(axis_vars())[axis_vars() == input$xvar]
    yvar_name <- names(axis_vars())[axis_vars() == input$yvar]
    color_name <- names(axis_vars())[axis_vars() == input$color]
    size_name <- names(axis_vars())[axis_vars() == input$size]
    
    x_var <- if(input$xvar=='') NULL else input$xvar
    y_var <- if(input$yvar=='') NULL else input$yvar
    color_var <- if(input$color=='') NULL else input$color
    size_var <- if(input$size=='') NULL else input$size
    
    # Rotate x axis text if there are alot of x axis ticks
    # Identify by x axis being in only_x_var_charts from global.R
    if (input$chart_type %in% only_x_var_charts) {
      g <- ggplot(properties(), aes_string(x_var)) +
        labs(title = paste(input$chart_type, 'chart of', xvar_name),
             x = xvar_name)
    } else {
      g <- ggplot(properties(), aes_string(x_var, y_var)) +
        labs(title = paste(input$chart_type, 'chart of', yvar_name, "vs", xvar_name),
             y = yvar_name, x = xvar_name)
    }
    
    # Create charts based on chart type here
    # TODO: Add more chart types
    
    if (input$chart_type == "scatter") {
      plot <- g + geom_point(aes_string(color = color_var, size=size_var)) 
      plot <- plot + geom_smooth(method="lm")
    } else if (input$chart_type == "jitter") {
      plot <- g + geom_jitter(aes_string(color = color_var, size=size_var)) 
      plot <- plot + geom_smooth(method="lm")
    } else if (input$chart_type == "bar") {
      plot <- g + geom_bar(aes_string(fill = color_var), 
                           stat = 'count')
    } else if (input$chart_type == "density") {
      plot <- g + stat_density_2d(aes(fill = ..level..), geom="polygon")
    } else if (input$chart_type == "violin") {
      plot <- g + geom_violin(aes_string(color=color_var), trim = FALSE)
    } else if (input$chart_type == "boxplot") {
      plot <- g + geom_boxplot(aes_string(fill=color_var))
    }
    
    # The default breaks is insufficient for most variables, so shud increase
    if (!is.factor(properties()[[input$xvar]])) {
      plot <- plot + scale_x_continuous(n.breaks = 8)
    }
    if (!is.factor(properties()[[input$yvar]])) {
      plot <- plot + scale_y_continuous(n.breaks = 8)
    }
    
    # If rotate checkbox is checked, flip the axes
    if (input$rotate) {
      plot <- plot + coord_flip()
    }
    
    # Rotate x axis text slightly if there's alot of x axis ticks
    if ((!input$rotate && x_var %in% x_axis_must_rotate) |
        (input$rotate && y_var %in% x_axis_must_rotate)) {
      plot <- plot + theme(axis.text.x = element_text(angle=65, vjust=0.6))
    }
    
    return(plot)
  })
  
  output$n_properties <- renderText({ nrow(properties()) })
  output$text <- renderText({ ((!input$rotate && input$xvar %in% x_axis_must_rotate) |
                                 (input$rotate && input$yvar %in% x_axis_must_rotate)) })
  output$text2 <- renderText({ input$xvar %in% x_axis_must_rotate })
  
  ##  ............................................................................
  ##  Location comparison                                                     ####
  
  ##  ............................................................................
  ##  Reactive values                                                         ####
  
  observeEvent(input$compare, {
    shinyjs::show("reactiveOutput7a")
    shinyjs::show("reactiveOutput7b")
    shinyjs::show("reactiveOutput9")
  })
  
  observeEvent(input$searchAddr1, {
    shinyjs::hide("reactiveOutput7a")
  })
  
  observeEvent(input$searchAddr2, {
    shinyjs::hide("reactiveOutput7b")
  })
  
  observe({
    if ((is.null(input$searchAddr1) || input$searchAddr1 == "") ||
        (is.null(input$searchAddr2) || input$searchAddr2 == "")) {
      shinyjs::disable("compare")
    } else {
      shinyjs::enable("compare")
    }
  })
  
  map<- function(addr){
    leaflet() %>% setView(lat = sales_listings[sales_listings$Address == addr,"lat"][1], lng = sales_listings[sales_listings$Address == addr,"lon"][1], zoom = 15) %>% 
      addTiles() %>% addMarkers(data=sales_listings[sales_listings$Address == addr,], lng = ~lon, lat = ~lat, popup = sales_listings[sales_listings$Address == addr,"Property.Name."]) %>% 
      addCircleMarkers(data=schools, lng = ~Longitude, lat = ~Latitude, color = "red",popup = schools$Name) %>% 
      addCircleMarkers(data = mall, lng = ~longitude, lat = ~latitude, color = "blue",popup = mall$name) %>% 
      addCircleMarkers(data = mrt, lng = ~Longitude, lat = ~Latitude, color = "green",popup = mrt$STN_NAME) 
  }
  
  location1 <- reactive({
    input$searchAddr1
  })
  
  mapLocation1 <- eventReactive(input$compare, {
    addr <- location1()
    map(addr)
  })
  
  output$mapLocation1 <- renderLeaflet({
    mapLocation1()
  })
  
  location2 <- reactive({
    input$searchAddr2
  })
  
  mapLocation2 <- eventReactive(input$compare, {
    addr <- location2()
    map(addr)
  })
  
  output$mapLocation2 <- renderLeaflet({
    mapLocation2()
  })
  
  tableComparison <- function(add1,add2){
    table1 <- data.frame(cbind(t(sales_listings[sales_listings$Address. == add1,cols][1,]),cols,t(sales_listings[sales_listings$Address. == add2,cols][1,])))
    colnames(table1) <- c(as.character(sales_listings[sales_listings$Address. == add1,"Property.Name."][1]),"Metrics",as.character(sales_listings[sales_listings$Address. == add2,"Property.Name."][1]))
    datatable(table1,rownames = FALSE,
              filter = "none",
              options = list(
                paging = FALSE, searching = FALSE,
                sort = FALSE, info = FALSE,
                columnDefs = list(list(className = 'dt-center', targets = 0:2,
                                       width = '200px', targets = c(2))))) %>%
      formatStyle(
        "Metrics",
        backgroundColor = 'gray')
  }
  
  output$CTcomparisonTable <- renderDataTable({
    tableComparison(input$searchAddr1,input$searchAddr2) 
  })
  
})

