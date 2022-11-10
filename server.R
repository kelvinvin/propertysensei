shinyServer(function(input, output, session) {
    
    ################################################
    
    ## Preparation Section
    
    ################################################
    
    addPredictions <- function(sales_df) {
        sales_df %>% 
            rowwise() %>% 
            mutate(Predicted.Rent. = getRentPrediction(Built.Year., travel_time_minutes_to_central, travel_time_orchard, travel_time_raffles, Area.Builtup, Bathrooms., Bedrooms., is_HDB)) %>%
            mutate(Predicted.Rent. = Area.Builtup * Predicted.PSF.Rent.) %>%
            mutate(Months.Breakeven = Asking. / Predicted.Rent.) %>%
            relocate(Predicted.PSF.Rent., .after = PSF.) %>%
            relocate(Months.Breakeven, .after = Asking.) %>%
            relocate(Predicted.Rent., .after = Asking.)
    }
    
    getRentPrediction <- function(built_year, travel_time_minutes_to_central, travel_time_orchard, travel_time_raffles, area_builtup, bathroom, bedroom, is_HDB) {
        values <- c(1, built_year, travel_time_minutes_to_central, travel_time_orchard, travel_time_raffles, area_builtup, bathroom, bedroom, is_HDB)
        coefficients <- c(0.001399, -0.0012, -0.05109, -0.004986, -0.02864, 0.04372, 0.0023, 0.01997, -0.9789)
        return (sum(values * coefficients, na.rm = TRUE))
    }
    
    rent_listings$Property.Name. <- toupper(rent_listings$Property.Name.)
    rent_listings <- unique(rent_listings[c("Property.Name.","Property.Type.","Asking.","Area.","travel_time_changi","travel_time_orchard","travel_time_raffles","District.","Bedrooms.","Bathrooms.","Asking.","Area.Builtup","is_HDB","Built.Year.","PSF.")])
    cte <- left_join(data_rent, data_loc, by = c("Postal.District" = "id"))
    cte <- cte %>% mutate(lower = str_extract(cte$Floor.Area..sq.ft., "^\\d+(?=\\s)")) %>% mutate(upper = str_extract(cte$Floor.Area..sq.ft., "(?<=\\s)\\d+"))
    agg_data <- left_join(cte, rent_listings, by = c("Building.Project.Name" = "Property.Name.")) %>% filter(Area.Builtup >= lower & Area.Builtup <= upper)
    
    sales_listings <- left_join(sales_listings, agg_data, by = c("District." = "Postal.District"))
    
    sales_listings <- addPredictions(sales_listings)
    
    districts_polygons <- readOGR(districts_geojson)
    
    # returns <- left_join(hdb_avg_appreciation_district, private_avg_appreciation_district)
    # returns$Annual_Avg_Return<- rowMeans(returns[, c(2,3)], na.rm = T)
    districts_polygons@data$id <- paste0("D", districts_polygons@data$id)
    
    data <- data.frame(District = hdb_avg_appreciation_district$District.,
                       Values = hdb_avg_appreciation_district$Annual_Avg_Return,
                       Appreciation = paste0(format(hdb_avg_appreciation_district$Annual_Avg_Return*100, digits = 3),
                                             "%"))
    
    districts_polygons@data <- merge(districts_polygons@data, data, by.x="id", by.y="District", sort = F)

    ################################################
    
    ## Page: Data Explorer
    
    ################################################
    
    output$rent_table <- renderDataTable({
        columns = names(rent_listings)
        if (!is.null(input$select_rent_col)) {
            columns = input$select_rent_col
        }
        data <- rent_listings[-1, columns, drop = FALSE]
        datatable(data, escape = FALSE, options = list(scrollX = TRUE), filter="top", selection = "none")
    })
    
    output$sales_table <- renderDataTable({
        columns = names(sales_listings)
        if (!is.null(input$select_sales_col)) {
            columns = input$select_sales_col
        }
        data <- sales_listings[-1, columns, drop = FALSE]
        datatable(data, escape = FALSE, options = list(scrollX = TRUE), filter="top", selection = "none")
    })
    
    datasetInput <- reactive({
        if (input$trends_tab == "Rent Listings") {
            return(rent_listings)
        } else if (input$trends_tab == "Sales Listings") {
            return(sales_listings)
        }
    })
    
    ################################################
    
    ## Page: Interactive Map
    
    ################################################
    
    output$map <- renderLeaflet({
        
        pal <- colorNumeric(palette = "Greens", domain = districts_polygons$Values, na.color = NA)
        popup <- paste(paste0("<strong>District: </strong>",districts_polygons$name), 
                       "<br><strong>HDB Average Appreciation: </strong>", districts_polygons$Appreciation)
        
        leaflet(districts_polygons) %>% 
            # Base groups
            addTiles(group = "OSM (default)") %>%
            addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
            addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
            # Layer groups
            addPolygons(weight = 2, stroke = TRUE, smoothFactor = 0.1, 
                        fillOpacity = 0.8, color="black", 
                        fillColor = ~pal(Values), layerId = ~id, 
                        popup=~popup, highlight = highlightOptions(
                            weight = 5, fillOpacity = 0.7,
                            bringToFront = TRUE), group = "District") %>% 
            # Layers control
            addLayersControl(
                baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
                overlayGroups = c("District"),
                options = layersControlOptions(collapsed = T)
            ) %>%
            setView(lat = 1.3521, lng = 103.8198, zoom = 12) %>%
            addLegend(pal = pal, values = ~Values, opacity = 1,
                      title = "HDB Annual Appreciation (1990-2022)",
                      labFormat = labelFormat(suffix = "%",
                                              transform = function(x) {100 * x}))
    })
    
    is_HDB <- function(col) {
        grepl("HDB", col, fixed = T)
    }
    
    output$plot_district_map <- renderPlotly({
        selected_property_type <- input$select_map_property_type
        selected_district <- input$select_map_district
        hdb_selected <- is_HDB(input$select_map_property_type) || selected_property_type == all_public
        y_col <- "avg_psf"
        if (hdb_selected) {
            x_col <- "Year"
            dataset <- hdb_annual_avg_psf
        } else {
            x_col <- "Date."
            dataset <- private_annual_avg_psf
        }
        if (!is.null(selected_district)) {
            dataset <- dataset %>% filter(District. == selected_district)
        }
        if (!is.null(selected_property_type)) {
            grouped_selection <- grepl("All", input$select_map_property_type, fixed = T)
            # Filter for property type if "All" is not in selected
            if (!grouped_selection) {
                dataset <- dataset %>% filter(Property.Type. == selected_property_type)
            } else {
                if (hdb_selected) {
                    dataset <- dataset %>% filter(is_HDB(Property.Type.))
                } else {
                    dataset <- dataset %>% filter(!is_HDB(Property.Type.))
                }
            }
        }
        shiny::validate(
            shiny::need(nrow(dataset) > 0, "No data available")
        )
        plot <- ggplot(dataset, aes_string(x_col, y_col)) + 
                ylab("Average Selling PSF")
        if (grouped_selection) {
            plot <- plot + 
                geom_line(aes_string(group = "Property.Type.",
                               color = "Property.Type.")) + 
                labs(color = "Property Type")
        } else {
            plot <- plot + geom_line(group = 1)
        }
        return (plot)
    })
    
    observeEvent(input$map_shape_click$id, {
        updateSelectizeInput(session, inputId = "select_map_district", 
                             selected = input$map_shape_click$id)
    })
    
    ################################################
    
    ## Page: Find Property
    
    ################################################
    
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
    
    observe({
        selected_chart <- input$chart_type
        ## Modify size options
        if (selected_chart %in% c("Density", "Bar", "Violin", "Boxplot")) {
            updateSelectizeInput(session, "size",
                                 selected = "", server = TRUE)
        } else {
            updateSelectizeInput(session, "size", choices = common_axis_vars,
                                 selected = "Area.Builtup", server = TRUE)
        }
        
        ## Modify color options
        if (selected_chart %in% c("Density")) {
            updateSelectizeInput(session, "color",
                                 selected = "", server = TRUE)
        } else {
            updateSelectizeInput(session, "color", choices = common_axis_vars,
                                 selected = "District.", server = TRUE)
        }
    })
    
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
        
        if (input$chart_type == "Scatter") {
            plot <- g + geom_point(aes_string(color = color_var, size=size_var))
            plot <- plot + geom_smooth(method="lm")
        } else if (input$chart_type == "Jitter") {
            plot <- g + geom_jitter(aes_string(color = color_var, size=size_var)) 
            plot <- plot + geom_smooth(method="lm")
        } else if (input$chart_type == "Bar") { # Size
            plot <- g + geom_bar(aes_string(fill = color_var), 
                                 stat = 'Count')
        } else if (input$chart_type == "Density") {
            plot <- g + stat_density_2d(aes(fill = ..level..), geom="polygon") #Size, color
        } else if (input$chart_type == "Violin") {
            plot <- g + geom_violin(aes_string(color=color_var), trim = FALSE) #Size
        } else if (input$chart_type == "Boxplot") {
            plot <- g + geom_boxplot(aes_string(fill=color_var)) # Size
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
        
        # If axis is a currency, add dollar sign
        if (input$xvar %in% currency_format_axes) {
            if (input$log_x_axis) {
                plot <- plot + scale_x_continuous(trans="log10", labels=scales::dollar_format())
                plot <- plot + xlab(paste0("log10(", input$xvar, ")"))
            } else {
                plot <- plot + scale_x_continuous(labels=scales::dollar_format())
            }
        }
        if (input$yvar %in% currency_format_axes) {
            if (input$log_y_axis) {
                plot <- plot + scale_y_continuous(trans="log10", labels=scales::dollar_format())
                plot <- plot + ylab(paste0("log10(", input$yvar, ")"))
            } else {
                plot <- plot + scale_y_continuous(labels=scales::dollar_format())
            }        
        }
        
        # Rotate x axis text slightly if there's alot of x axis ticks
        if (!is.null(x_var) && (!input$rotate && x_var %in% x_axis_must_rotate) |
            (input$rotate && y_var %in% x_axis_must_rotate)) {
            plot <- plot + theme(axis.text.x = element_text(angle=65, vjust=0.6))
        }

        return(plot)
    })
    
    output$n_properties <- renderText({ nrow(properties()) })
    
    
    ################################################
    
    ## Page: Mortgage Calculator 
    # (modified from https://statsandr.com/blog/mortgage-calculator-r-shiny/)
    
    ################################################
    
    mortgage <- function(P = 500000, I = 6, L = 30, amort = TRUE, plotData = TRUE) {
        J <- I / (12 * 100)
        N <- 12 * L
        M <- P * J / (1 - (1 + J)^(-N))
        monthPay <<- M
        # Calculate Amortization for each Month
        if (amort == TRUE) {
            Pt <- P # current principal or amount of the loan
            currP <- NULL
            while (Pt >= 0) {
                H <- Pt * J # this is the current monthly interest
                C <- M - H # this is your monthly payment minus your monthly interest, so it is the amount of principal you pay for that month
                Q <- Pt - C # this is the new balance of your principal of your loan
                Pt <- Q # sets P equal to Q and goes back to step 1. The loop continues until the value Q (and hence P) goes to zero
                currP <- c(currP, Pt)
            }
            monthP <- c(P, currP[1:(length(currP) - 1)]) - currP
            aDFmonth <<- data.frame(
                Month = 1:length(currP),
                Year = sort(rep(1:ceiling(N / 12), 12))[1:length(monthP)],
                Balance = c(currP[1:(length(currP))]),
                Payment = monthP + c((monthPay - monthP)[1:(length(monthP))]),
                Principal = monthP,
                Interest = c((monthPay - monthP)[1:(length(monthP))])
            )
            aDFmonth <<- subset(aDFmonth, Year <= L * 12)
            aDFyear <- data.frame(
                Amortization = tapply(aDFmonth$Balance, aDFmonth$Year, max),
                Annual_Payment = tapply(aDFmonth$Payment, aDFmonth$Year, sum),
                Annual_Principal = tapply(aDFmonth$Principal, aDFmonth$Year, sum),
                Annual_Interest = tapply(aDFmonth$Interest, aDFmonth$Year, sum),
                Year = as.factor(na.omit(unique(aDFmonth$Year)))
            )
            aDFyear <<- aDFyear
        }
        if (plotData == TRUE) {
            aDFyear2 <- aDFyear %>%
                rename(
                    Interest = Annual_Interest,
                    Payment = Annual_Payment,
                    Principal = Annual_Principal
                )
            aDFyear2$Year <- as.factor(aDFyear2$Year)
            aDFyear2 <- melt(aDFyear2[, c("Interest", "Principal", "Year")], id.vars = "Year")
            
            ggplot(aDFyear2, aes(x = Year, y = value, fill = variable)) +
                geom_bar(position = "fill", stat = "identity") +
                labs(y = "Payment") +
                scale_y_continuous(labels = percent) +
                theme_minimal() +
                theme(legend.title = element_blank(), legend.position = "top")
        }
    }
    
    output$text <- renderUI({
        mortgage(P = input$principal, I = input$interest, L = input$length, plotData = FALSE)
        HTML(paste0(
            "<h3>", "Summary", "</h3>",
            "Principal (loan amount): ", format(round(input$principal, 2), big.mark = ","),
            "<br>",
            "Annual interest rate: ", input$interest, "%",
            "<br>",
            "Effective annual rate: ", format(round(((1 + (input$interest / 1200)) ^ 12 -1) * 100, digits = 2)), "%",
            "<br>",
            "Term: ", input$length, " years (", input$length * 12, " months)",
            "<br>",
            "<b>", "Monthly payment: ", format(round(monthPay, digits = 2), big.mark = ","), "</b>",
            "<br>",
            "<b>", "Total cost: ", "</b>", format(round(input$principal, 2), big.mark = ","), " (principal) + ", format(round(monthPay * 12 * input$length - input$principal, 2), big.mark = ","), " (interest) = ", "<b>", format(round(monthPay * 12 * input$length, digits = 2), big.mark = ","), "</b>"
        ))
    })
    
    output$distPlot <- renderPlot({
        mortgage(P = input$principal, I = input$interest, L = input$length)
    })
    
    output$tbl <- DT::renderDataTable({
        mortgage(P = input$principal, I = input$interest, L = input$length, plotData = FALSE)
        df_month <- DT::datatable(data.frame(round(aDFmonth, 2)),
                                  extensions = "Buttons",
                                  options = list(
                                      lengthChange = TRUE,
                                      dom = "Blrtip",
                                      buttons = c("copy", "csv", "excel", "pdf", "print"),
                                      
                                      lengthMenu = list(c(10, 12, 15, 25, 50, 100, -1), c("10", "12", "15", "25", "50", "100", "All"))
                                  ),
                                  rownames = FALSE
        ) %>%
            formatCurrency(c("Balance", "Payment", "Principal", "Interest"), currency = "", interval = 3, mark = ",")
    })
    
    ##  ............................................................................
    ##  Location comparison                                                     ####
    
    ##  ............................................................................
    ##  Reactive values                                                         ####
    
    observeEvent(input$compare, {
      shinyjs::show("reactiveOutput7a")
      shinyjs::show("reactiveOutput7b")
      shinyjs::show("reactiveOutput9")
      shinyjs::show("reactiveOutput10a")
      shinyjs::show("reactiveOutput10b")
    })
    
    observeEvent(input$searchAddr1, {
      shinyjs::hide("reactiveOutput7a")
      shinyjs::hide("reactiveOutput10a")
    })
    
    observeEvent(input$searchAddr2, {
      shinyjs::hide("reactiveOutput7b")
      shinyjs::hide("reactiveOutput10b")
    })
    
    observeEvent(input$searchAddr3, {
      shinyjs::hide("reactiveOutput10a")
    })
    
    observeEvent(input$searchAddr4, {
      shinyjs::hide("reactiveOutput10b")
    })
    
    observe({
      if ((is.null(input$searchAddr1) || input$searchAddr1 == "") ||
          (is.null(input$searchAddr2) || input$searchAddr2 == "")) {
        shinyjs::disable("compare")
        shinyjs::disable("compare2")
      } else {
        shinyjs::enable("compare")
        shinyjs::enable("compare2")
      }
    })
    
    observe({
      if ((is.null(input$searchAddr3) || input$searchAddr4 == "") ||
          (is.null(input$searchAddr4) || input$searchAddr4 == "")) {
        shinyjs::disable("compare2")
      } else {
        shinyjs::enable("compare2")
      }
    })
    
    map<- function(addr){
      
      address_data <- sales_listings[sales_listings$Address. == addr,][1,]
      address_lon <- as.numeric(address_data$lon)
      address_lat <- as.numeric(address_data$lat)
      
      s_threshold <- 0.01
      while (nrow(schools[abs(schools$Longitude - address_lon) <= s_threshold & abs(schools$Latitude - address_lat) <= s_threshold, ]) < 1){
        s_threshold <- s_threshold + 0.005
      }
      
      schools1 <- schools[abs(schools$Longitude - address_lon) <= s_threshold & abs(schools$Latitude - address_lat) <= s_threshold, ]
      S_duration <- NULL
      S_distance <- NULL
      for (i in 1:nrow(schools1)){
        S_route_summary <- osrmRoute(src=c(address_lon, address_lat),dst=c(schools1[i,"Longitude"], schools1[i,"Latitude"]),overview = FALSE)
        S_duration <- c(S_duration, S_route_summary[1])
        S_distance <- c(S_distance, S_route_summary[2])
      }
      schools2 <- cbind(schools1,S_distance,S_duration)
      
      m_threshold <- 0.01
      while (nrow(mall[abs(mall$Longitude - address_lon) <= m_threshold & abs(mall$Latitude - address_lat) <= m_threshold, ]) < 1){
        m_threshold <- m_threshold + 0.005
      }
      
      mall1 <- mall[abs(mall$Longitude - address_lon) <= m_threshold & abs(mall$Latitude - address_lat) <= m_threshold, ]
      m_duration <- NULL
      m_distance <- NULL
      for (i in 1:nrow(mall1)){
        m_route_summary <- osrmRoute(src=c(address_lon, address_lat), dst=c(mall1[i,"Longitude"], mall1[i,"Latitude"]),overview = FALSE)
        m_duration <- c(m_duration, m_route_summary[1])
        m_distance <- c(m_distance, m_route_summary[2])
      }
      mall2 <- cbind(mall1,m_distance,m_duration)
      
      mrt_threshold <- 0.01
      while (nrow(mrt[abs(mrt$Longitude - address_lon) <= mrt_threshold & abs(mrt$Latitude - address_lat) <= mrt_threshold, ]) < 1){
        mrt_threshold <- mrt_threshold + 0.005
      }
      
      mrt1 <- mrt[abs(mrt$Longitude - address_lon) <= mrt_threshold & abs(mrt$Latitude - address_lat) <= mrt_threshold, ]
      mrt_duration <- NULL
      mrt_distance <- NULL
      for (i in 1:nrow(mrt1)){
        mrt_route_summary <- osrmRoute(src=c(address_lon, address_lat), dst=c(mrt1[i,"Longitude"], mrt1[i,"Latitude"]),overview = FALSE)
        mrt_duration <- c(mrt_duration, mrt_route_summary[1])
        mrt_distance <- c(mrt_distance, mrt_route_summary[2])
      }
      mrt2 <- cbind(mrt1,mrt_distance,mrt_duration)
      
      leaflet() %>% 
        setView(lat = address_lat, lng = address_lon, zoom = 15) %>% 
        addTiles() %>% 
        addMarkers(data=address_data, lng = ~lon, lat = ~lat, popup = address_data$Property.Name.) %>% 
        addCircleMarkers(data=schools2, lng = ~Longitude, lat = ~Latitude, color = "red",popup = paste(schools2$Name,"-",schools2$S_distance,"KM","-",schools2$S_duration,"Mins"),group = "Schools") %>% 
        addCircleMarkers(data = mall2, lng = ~Longitude, lat = ~Latitude, color = "blue",popup =  paste(mall2$Name,"-",mall2$m_distance,"KM","-",mall2$m_duration,"Mins"), group = "Malls") %>% 
        addCircleMarkers(data = mrt2, lng = ~Longitude, lat = ~Latitude, color = "green",popup = paste(mrt2$Name,"-",mrt2$mrt_distance,"KM","-",mrt2$mrt_duration,"Mins"), group = "MRT") %>%
        addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
        addDrawToolbar(editOptions=editToolbarOptions(selectedPathOptions=selectedPathOptions())) %>% 
        addMeasure(primaryLengthUnit="kilometers", secondaryLengthUnit="kilometers") %>%
        addTiles(group="Default") %>% addProviderTiles("Esri.WorldImagery", group = "Esri") %>% 
        addLayersControl(baseGroups = c("Default","Esri"), overlayGroups = c("Schools","Malls","MRT"))
    }
    
    location1 <- reactive({
      str_to_title(input$searchAddr1)
    })
    
    mapLocation1 <- eventReactive(input$compare, {
      addr <- location1()
      map(addr)
    })
    
    output$mapLocation1 <- renderLeaflet({
      mapLocation1()
    })
    
    location2 <- reactive({
      str_to_title(input$searchAddr2)
    })
    
    mapLocation2 <- eventReactive(input$compare, {
      addr <- location2()
      map(addr)
    })
    
    output$mapLocation2 <- renderLeaflet({
      mapLocation2()
    })
    
    tableComparison <- function(add1,add2){
  
      add1_data = t(sales_listings[sales_listings$Address. == add1,cols][1,])
      add2_data = t(sales_listings[sales_listings$Address. == add2,cols][1,])
      
      table1 <- data.frame(add1_data, cols, add2_data )
      
      datatable(table1,rownames = FALSE,
                colnames = c("","Metrics",""),
                filter = "none",
                options = list(
                  paging = FALSE, searching = FALSE,
                  sort = FALSE, info = FALSE,
                  columnDefs = list(list(className = 'dt-center', targets = 0:2,
                                         width = '200px', targets = c(2))))) %>%
        formatStyle(
          "cols",
          backgroundColor = 'gray')
    }
    
    tableOut <- eventReactive(input$compare,{
      add1 <- location1()
      add2 <- location2()
      tableComparison(add1,add2)
    })
    
    output$CTcomparisonTable <- renderDataTable({
      tableOut()
    })
    
    distancemap<- function(addr1,addr2,t_option){
      if ( grepl("SCHOOL", addr2,fixed = T)){
        schools$Name = str_to_upper(schools$Name)
        data1 = schools
      } else if (grepl("MRT",addr2,fixed=T)){
        data1 = mrt
      } else { data1 = mall}
      
      address1_data <- sales_listings[sales_listings$Address. == addr1,][1,]
      address2_data <- data1[data1$Name == addr2,][1,]
      address1_lon <- as.numeric(address1_data$lon)
      address1_lat <- as.numeric(address1_data$lat)
      address2_lon <- as.numeric(address2_data$Longitude)
      address2_lat <- as.numeric(address2_data$Latitude)
      
      df = data.frame(
        Name = c(addr1,addr2),
        lon = c(address1_lon, address2_lon),
        lat = c(address1_lat, address2_lat)
      )
      
      api_key = "AIzaSyBeXEQcDFrjCHrEISl2cncPr3opKt59paM"
      
      res <- google_directions(
        key = api_key,
        origin = c(address1_lat,address1_lon),
        destination = c(address2_lat,address2_lon),
        mode = t_option
      )
      
      df_polyline <- decode_pl(res$routes$overview_polyline$points)
      
      leaflet() %>%
        addTiles() %>%
        addPolylines(data=df_polyline, lng = ~lon, lat = ~lat, 
                     label = paste(res$routes$legs[[1]]$duration[1], ' - ', res$routes$legs[[1]]$distance[1]), 
                     labelOptions = labelOptions(noHide = TRUE, direction = "top",
                                                 style = list(
                                                   "color"="black",
                                                   "font-family" ="sans-serif",
                                                   "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                   "font-size" = "12px",
                                                   "border-color" = "rgba(0,0,0,0.5)"))) %>% 
        addCircleMarkers(
          data = df,
          popup = df$Name,
          stroke = FALSE, 
          label = seq_len(nrow(df)),
          fillOpacity = 0.8, 
          labelOptions = labelOptions(
            direction = "center",
            style = list('color' = "white"),
            noHide = TRUE, 
            offset=c(0,0), 
            fill = TRUE, 
            opacity = 1, 
            weight = 10, 
            textOnly = TRUE
          ))
    }
    
    location3 <- reactive({
      str_to_upper(input$searchAddr3)
    })
    
    t_option1 <- reactive({
      input$traveling_option1
    })
    
    mapLocation3 <- eventReactive(input$compare2, {
      addr1 <- location1()
      addr2 <- location3()
      t_option <- t_option1()
      distancemap(addr1,addr2,t_option)
    })
    
    output$mapLocation3 <- renderLeaflet({
      mapLocation3()
    })
    
    location4 <- reactive({
      str_to_upper(input$searchAddr4)
    })
    
    t_option2 <- reactive({
      input$traveling_option2
    })
    
    mapLocation4 <- eventReactive(input$compare2, {
      addr1 <- location2()
      addr2 <- location4()
      t_option <- t_option2()
      distancemap(addr1,addr2,t_option)
    })
    
    output$mapLocation4 <- renderLeaflet({
      mapLocation4()
    })
    
    
    ################################################
    
    # Page: About
    
    ################################################
    
    getPageAbo <- function() {
        return(includeHTML("About.html"))
    }
    output$abo <- renderUI({
        getPageAbo()
    })
})