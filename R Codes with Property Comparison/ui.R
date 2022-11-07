library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(shinyjs)
library(leaflet)

header <- dashboardHeader(
    title = "PropertySenpai"
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(tabName = "InteractiveMap","Interactive Map"),
        menuItem(tabName = "Trends", "Trends"),
        menuItem(tabName = "DataExplorer", "Data Explorer"),
        menuItem(tabName = "FAQ","FAQ"),
        menuItem(tabName = "PropertyComparison", "Property Comparison")
    )
)

data_explorer <- tabsetPanel(
      hr(),
      tabPanel("Rent", DT::dataTableOutput("rent_table")), 
      tabPanel("Sales", DT::dataTableOutput("sales_table"))
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

trends <- 
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
                         choices = c("All districts"=""), 
                         selected = "Rent Listings", multiple = T),
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
             
         ),
         wellPanel(
           span("Number of properties selected:",
                textOutput("n_properties"), textOutput("text"), textOutput("text2"))
         )
    ),
    column(9,
         tabsetPanel(id = "trends_tab", tabPanel("Rent"), tabPanel("Sales")),
         plotlyOutput("plot", height = "800px"),
    )
)

#   Location Comparison                                                     ####
propertyComparison <- function() {
  
  sampleLocations <- c("66 Lorong L Telok Kurau (425509)",
                       "5 Shenton Way (068808)",
                       "21 Marina Way (018978)",
                       "1 Dundee Road (149456)",
                       "1 Lorong 5 Toa Payoh (319458)")
  
  sampleLocations <- sample(sampleLocations,2)
  
  tagList(
    div(class = "container",
        h1("Property Comparison", class = "title fit-h1"),
        #tags$script(src = "plugins/fittext.js"),
        p("You have already identified two properties of interest, but cant decide which one to invest in? Let us help make your final decision."),
        p("Enter the addresses of two properties below and click the button to compare the locations."),
        p("Click the button to compare two sample locations or enter search addresses of your own!"),
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
                            h4(icon("circle", style = "color: red"), "Primary Schools",
                               icon("circle",style="color:blue"),"Shopping Malls",
                               icon("circle",style="color:green"),"MRT Stations")
                 )
                 ))
          ,
          column(6,
                 hidden(div(id = "reactiveOutput7b",
                            leafletOutput("mapLocation2"),
                            h4(icon("circle", style = "color: red"), "Primary Schools",
                               icon("circle",style="color:blue"),"Shopping Malls",
                               icon("circle",style="color:green"),"MRT Stations")
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
        )
        
    )
  )
}

body <- dashboardBody(
    tabItems(
        tabItem(tabName="FAQ", "FAQ"),
        tabItem(tabName="InteractiveMap", "This is the first page of the program"),
        tabItem(tabName="Trends", trends),
        tabItem(tabName="DataExplorer", data_explorer),
        tabItem(tabName="PropertyComparison", propertyComparison())
    )
)

ui <- dashboardPage(
    header = header,
    sidebar = sidebar,
    body = body
)