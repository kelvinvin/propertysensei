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
             checkboxInput("rotate", "Rotate axes")
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

about <- fluidPage(htmlOutput("abo"))
    
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(tabName = "InteractiveMap","Interactive Map"),
    menuItem(tabName = "Trends", "Statistical Analysis"),
    menuItem(tabName = "DataExplorer", "Find Property"),
    menuItem(tabName = "MortgageCalculator", "Mortgage Calculator"),
    menuItem(tabName = "About","About")
  )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName="About", about),
        tabItem(tabName="InteractiveMap", interactive_map),
        tabItem(tabName="Trends", trends),
        tabItem(tabName="DataExplorer", data_explorer),
        tabItem(tabName="MortgageCalculator", mortgage_calculator)
    )
)

ui <- dashboardPage(
    header = header,
    sidebar = sidebar,
    body = body
)