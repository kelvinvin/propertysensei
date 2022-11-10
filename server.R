shinyServer(function(input, output, session) {
    
    ################################################
    
    ## Preparation Section
    
    ################################################
    
    addPredictions <- function(sales_df) {
        sales_df %>% 
            rowwise() %>% 
            mutate (Predicted.Rent. = getRentPrediction(Built.Year., Area.Builtup, Bathrooms., Bedrooms., is_HDB)) %>%
            relocate(Predicted.Rent., .after = PSF.)
    }
    
    getRentPrediction <- function(built_year, area_builtup, bathroom, bedroom, is_HDB) {
        values <- c(1, built_year, area_builtup, bathroom, bedroom, is_HDB)
        coefficients <- c(-0.01373, 0.07329, -0.000464, -0.4887, 0.1542, -1.077)
        return (sum(values * coefficients, na.rm = TRUE))
    }
    
    sales_listings <- addPredictions(sales_listings)
    
    districts_polygons <- readOGR(districts_geojson)
    
    #returns <- left_join(hdb_avg_appreciation_district, private_avg_appreciation_district)
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
            plot <- plot + scale_x_continuous(labels=scales::dollar_format())
        }
        if (input$yvar %in% currency_format_axes) {
            plot <- plot + scale_y_continuous(labels=scales::dollar_format())
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
