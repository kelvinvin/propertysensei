library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)

library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(plotly)
library(rgdal)
library(reshape2)
library(scales)
library(DT)
library(leaflet)
library(RColorBrewer)
options(scipen = 999)

rent_listings <- read.csv("Data/rent_listings_data_cleaned.csv", stringsAsFactors = T)
sales_listings <- read.csv("Data/sales_listings_data_cleaned.csv", stringsAsFactors = T)
sales_hdb_history <- read.csv("Data/resale_1990_2022_cleaned_v2.csv", stringsAsFactors = T)
sales_private_history <- read.csv("Data/sales_transactions_private_properties cleaned.csv")
districts_geojson <- "Data/postal_districts.geojson"
hdb_annual_avg_psf <- read.csv("Data/HDB_avg_psf_annual.csv", stringsAsFactors = T)
private_annual_avg_psf <- read.csv("Data/private_avg_psf_annual.csv", stringsAsFactors = T)
hdb_avg_appreciation_district <- read.csv("Data/hdb_avg_appreciation_district.csv")
districts_metadata <- read.csv("Data/districts_metadata.csv")

# private_avg_appreciation_district <- read.csv("Data/private_avg_appreciation_district.csv")
common_axis_vars <- c(
  "None" = "",
  "Bathrooms" = "Bathrooms.",
  "Bedrooms" = "Bedrooms.",
  "Built Year" = "Built.Year.",
  "PSF" = "PSF.",
  "Asking Price" = "Asking.",
  "Area (Built-up, sqft)" = "Area.Builtup",
  "Area (Landed, sqft)" = "Area.Land",
  "District" = "District.",
  "Property Type" = "Property.Type.",
  "Floor Description" = "Floor.Description",
  "Tenure" = "Tenure.",
  "Furnish" = "Furnish.",
  "Is HDB" = "is_HDB"
)

sales_axis_vars <- c(
  "HDB Town" = "HDB.Town."
)

rent_axis_vars <- c(
  "HDB Town" = "neighbourhood",
  "Travel Time to Changi" = "travel_time_changi",
  "Travel Time to Orchard" = "travel_time_orchard",
  "Travel Time to Raffles" = "travel_time_raffles",
  "Nearest MRT" = "nearest_mrt_name"
)

chart_choices <- c(
  'Scatter', 'Jitter', 'Bar', 'Density', 'Violin', 'Boxplot'
)

only_x_var_charts <- c(
  'Bar'
)

x_axis_must_rotate <- c(
  'District.', 'HDB.Town.', "nearest_mrt_name"
)

currency_format_axes <- c(
  "PSF.", "Asking."
)

all_public <- "All Public"
all_private <- "All Private"

all_property_types <- sales_listings$Property.Type. %>% unique()
all_districts_names <- districts_metadata$name                      
all_districts_code <- districts_metadata$district                      
