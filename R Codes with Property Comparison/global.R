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
  'scatter', 'jitter', 'bar', 'density', 'violin', 'boxplot'
)

only_x_var_charts <- c(
  'bar'
)

x_axis_must_rotate <- c(
  'District.', 'HDB.Town.', "nearest_mrt_name"
)

cols <- c("Property.Type.","Built.Year.","Model.","District.",
          "Developer.","Tenure.","Facilities")