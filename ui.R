header <- dashboardHeader(
    title = "PropertySenpai"
)

data_explorer <- fluidPage(
  titlePanel("Find Property"),
  tabsetPanel(
      hr(),
      tabPanel("Sales Listings", 
               box(title ="Select columns",
                   selectInput("select_sales_col", "",
                               choice = names(sales_listings), 
                               selected = names(sales_listings),
                               multiple = TRUE),
                   collapsible = T, status = "info"
               ),
               DT::dataTableOutput("sales_table")),
      tabPanel("Rent Listings", 
               box(title ="Select columns",
                 selectInput("select_rent_col", "",
                             choice = names(rent_listings), 
                             selected = names(rent_listings),
                             multiple = TRUE),
                 collapsible = T, status = "info"
               ),
               DT::dataTableOutput("rent_table"))
  )
)

interactive_map <- fluidPage(
  titlePanel("Interactive Map"),
  fluidRow(
    column(12, leafletOutput("map", width = "100%", height = "800") %>% 
           withSpinner(color="grey", type=4))
  ),
  br(),
  fluidRow(
    column(9, 
     wellPanel(
       plotlyOutput("plot_district_map"),
     )
    ),
    column(3, box(width = NULL, status = "warning", 
     selectInput(inputId = 'select_map_district', 
                 label = 'District', 
                 choices =  paste0("D", seq(1, 28))),
     selectInput(inputId = "select_map_property_type", 
                 label = "Property Types", 
                 choices = c(all_public, all_private, levels(all_property_types)))
    ))
  )
)

sqft_input <- function(id, label) {
  return (autonumericInput(id, label,
                           currencySymbol =' sqft',
                           emptyInputBehavior = 'always',
                           decimalPlaces = 0, 
                           currencySymbolPlacement = "s",
                           min=0, value=NULL))
}

currency_input <- function(id, label) {
  return (autonumericInput(id, label,
                           currencySymbol ='$ ',
                           emptyInputBehavior = 'always',
                           decimalPlaces = 0, 
                           min=0, value=NULL))
}

mortgage_calculator <- fluidPage(
  # App title ----
  titlePanel("Mortgage Calculator"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      h4("Calculate amortization for a annuity loan with monthly compounding:"),
      hr(),
      # Input: Simple integer interval ----
      numericInput("principal", "Principal (loan amount)", 200000, min = 0, step = 1000),
      hr(),
      numericInput("interest", "Annual interest rate (in %)", 2, min = 0, max = 100, step = 0.01),
      hr(),
      sliderInput("length", "Duration of the loan (in years)",
                  min = 0,
                  max = 30,
                  value = 25,
                  step = 1
      ),
      hr(),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Table summarizing the values entered ----
      uiOutput("text"),
      br(),
      plotOutput("distPlot"),
      br(),
      DT::dataTableOutput("tbl"),
      br(),      
      br(),
    )
  )
)

trends <- fluidPage(
  titlePanel("Statistical Analysis"),
  fluidRow(
    column(3,
         wellPanel(
             selectInput('chart_type', 'Chart Type', chart_choices, selected = chart_choices[1]),
             selectInput("yvar", "Y-axis variable", common_axis_vars, selected = "PSF."),
             selectInput("xvar", "X-axis variable", common_axis_vars, selected = "Built.Year."),
             selectInput("color", "Color variable", common_axis_vars, selected = "District."),
             selectInput("size", "Size variable", common_axis_vars, selected = "Area.Builtup"),
             checkboxInput("rotate", "Rotate axes"),
             checkboxInput("log_x_axis", "Log x axis (only for prices)"),
             checkboxInput("log_y_axis", "Log y axis (only for prices)")
         ),
         wellPanel(
             h4("Filter"),
             selectInput(inputId = 'district', 
                         label = 'District', 
                         choices = c("All districts"="", all_districts_names), 
                         multiple = T),
             selectInput(inputId = "property_types", 
                         label = "Property Types", 
                         choices = c("All types"=""),
                         multiple = T),
             selectInput(inputId = "floors", 
                         label = "Floors", 
                         choices = c("All floors"=""),
                         multiple = T),
             selectInput(inputId = "tenure", 
                         label = "Tenure", 
                         choices = c("All tenures"=""),
                         multiple = T),
             sliderInput("year", "Year built", 1940, 2023, value = c(1940, 2023),
                         sep = ""),
             sliderInput("bedrooms", "Bedrooms", 1, 10, value = c(1, 10),
                         sep = ""),
             sliderInput("bathrooms", "Bathrooms", 1, 10, value = c(1, 10),
                         sep = ""),
             fluidRow(
               column(6, sqft_input("min_area_builtup", "Min Area (Built-up)")),
               column(6, sqft_input("max_area_builtup", "Max Area (Built-up)"))),
             fluidRow(
               column(6, sqft_input("min_area_land", "Min Area (Land)")),
               column(6, sqft_input("max_area_land", "Max Area (Land)"))),
             fluidRow(
                 column(6, currency_input("min_PSF", "Min PSF")),
                 column(6, currency_input("max_PSF", "Max PSF"))),
             fluidRow(
                 column(6, currency_input("min_price", "Min Price")),
                 column(6, currency_input("max_price", "Max Price"))),
             
         )
    ),
    column(9,
         tabsetPanel(id = "trends_tab", tabPanel("Sales Listings"), tabPanel("Rent Listings")),
         br(),
         withSpinner(plotlyOutput("plot", height = "800px"), type=4, color="blue"),
         wellPanel(
           span("Number of properties selected:",
                textOutput("n_properties"))
         )
    )
  )
)

#Location Comparison                                                     
propertyComparison <- function() {
  
  sampleLocations <- c("66 Lorong L Telok Kurau (425509)",
                       "5 Shenton Way (068808)",
                       "21 Marina Way (018978)",
                       "1 Dundee Road (149456)",
                       "1 Lorong 5 Toa Payoh (319458)")
  
  sampleLocations <- sample(sampleLocations,2)
  
  samplePlaces <- c("River Valley Primary School",
                    "Cantonment Primary School",
                    "PENINSULA PLAZA",
                    "MARINA BAY FINANCIAL CENTRE TOWER 3",
                    "EUNOS MRT STATION")
  
  samplePlace <- sample(samplePlaces,2)
  
  tagList(
    div(class = "container",
        h1("Property Location Comparison", class = "title fit-h1"),
        #tags$script(src = "plugins/fittext.js"),
        p("You have already identified two properties of interest, but cant decide which one to invest in? Let us help make your final decision."),
        p("Enter the addresses of two properties below and click the button to compare the locations against Primary Schools, MRT and Shopping Malls Nearby!"),
        p("You may copy and paste sales listing addresses from the Find Property tab."),
        p("Click on the markers to view the names of the location, and the driving distance and duration!"),
        p("Feel free to use the toggles to filter the map style and features that you wish to see on the map! You can also draw lines to measure distance and areas!"),
        p("Please be patient as our maps will take up to 10 seconds to plot to calculate the distance and duration accurately!"),
        fluidRow(
          column(4,
                 div(class = "addrSearch",
                     textInput("searchAddr1", 
                               value = sampleLocations[1],
                               placeholder = "Enter address...",
                               label = "Enter Address in the format E.g. 1 Lorong 5 Toa Payoh (319458)"),
                     class = "search")
          ),
          column(4, class="text-center", 
                 disabled(actionButton("compare", width = "75%",
                                       class = "btn-primary", style = "margin: 20px 0 20px 0;",
                                       HTML("&laquo; Compare locations &raquo;")))
          ),
          column(4,
                 div(class = "addrSearch",
                     textInput("searchAddr2", 
                               value = sampleLocations[2],
                               placeholder = "Enter address...",
                               label = "Enter Address in the format E.g. 1 Lorong 5 Toa Payoh (319458)"),
                     class = "search"
                 )
          )
        ),
        fluidRow(
          column(6,
                 hidden(div(id = "reactiveOutput7a",
                            leafletOutput("mapLocation1"),
                            h4(tags$script(src="https://kit.fontawesome.com/811000c4cc.js"),
                               tags$div(
                                 tags$i(class = "fa-solid fa-location-dot",style = "color:blue"),
                                 tags$span("Property"),
                              icon("circle", style = "color: red"), "Primary Schools",
                               icon("circle",style="color:blue"),"Shopping Malls",
                               icon("circle",style="color:green"),"MRT Stations"))
                 )
                 ))
          ,
          column(6,
                 hidden(div(id = "reactiveOutput7b",
                            leafletOutput("mapLocation2"),
                            h4(tags$script(src="https://kit.fontawesome.com/811000c4cc.js"),
                               tags$div(
                                 tags$i(class = "fa-solid fa-location-dot",style = "color:blue"),
                                 tags$span("Property"),
                                 icon("circle", style = "color: red"), "Primary Schools",
                                 icon("circle",style="color:blue"),"Shopping Malls",
                                 icon("circle",style="color:green"),"MRT Stations"))
                 )
                 )
          )),
        fluidRow(
          column(12,
                 hidden(div(id = "reactiveOutput9", 
                            style = "margin-top: 30px; font-size: 1.5em;",
                            hr(),
                            h3("Property Information Comparison"),
                            DT::dataTableOutput("CTcomparisonTable"))
                 ))
        ),
        fluidRow(h3("Check Routes, Exact Distance and Traveling Time to Primary Schools, Malls or MRT!")
        ),
        fluidRow(
          column(4,
                 div(class = "addrSearch",
                     textInput("searchAddr3", 
                               value = samplePlaces[1],
                               placeholder = "Enter Primary School/MRT/Malls",
                               label = "Enter Primary School/MRT/Malls in the format (E.g.River Valley Primary School/EUNOS MRT STATION/MARINA SQUARE) to find out routes, distance and traveling time!"),
                     class = "search")
          ),
          column(4, class="text-center", 
                 disabled(actionButton("compare2", width = "75%",
                                       class = "btn-primary", style = "margin: 20px 0 20px 0;",
                                       HTML("&laquo; Compare locations &raquo;")))
          ),
          column(4,
                 div(class = "addrSearch",
                     textInput("searchAddr4", 
                               value = samplePlaces[2],
                               placeholder = "Enter Primary School/MRT/Malls",
                               label = "Enter Primary School/MRT/Malls in the format (E.g.River Valley Primary School/EUNOS MRT STATION/MARINA SQUARE) to find out routes, distance and traveling time!"),
                     class = "search"
                 )
          )
        ),
        fluidRow(
          column(4,
                 selectInput('traveling_option1', 'Traveling Option', traveling_option, selected = traveling_option[1])
                 )
          ,
          column(4,
                 ""
                 )
          ,
          column(4,
                 selectInput('traveling_option2', 'Traveling Option', traveling_option, selected = traveling_option[1])
          )
        ),
        fluidRow(
          column(6,
                 hidden(div(id = "reactiveOutput10a",
                            leafletOutput("mapLocation3")
                 )
                 ))
          ,
          column(6,
                 hidden(div(id = "reactiveOutput10b",
                            leafletOutput("mapLocation4")
                 )
                 )
          )),
        
    )
  )}

about <- fluidPage(htmlOutput("abo"))
    
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(tabName = "InteractiveMap","Interactive Map",icon = icon("map")),
    menuItem(tabName = "Trends", "Statistical Analysis",icon = icon("chart-simple")),
    menuItem(tabName = "DataExplorer", "Find Property",icon = icon("database")),
    menuItem(tabName = "MortgageCalculator", "Mortgage Calculator",icon = icon("calculator")),
    menuItem(tabName = "PropertyComparison", "Property Location Comparison",icon = icon("scale-balanced")),
    menuItem(tabName = "About","About",icon = icon("question"))
  )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName="About", about),
        tabItem(tabName="InteractiveMap", interactive_map),
        tabItem(tabName="Trends", trends),
        tabItem(tabName="DataExplorer", data_explorer),
        tabItem(tabName="MortgageCalculator", mortgage_calculator),
        tabItem(tabName="PropertyComparison", propertyComparison())
    )
)

ui <- dashboardPage(
    header = header,
    sidebar = sidebar,
    body = body
)