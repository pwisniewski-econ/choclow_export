all_labels <- list(
  "Sector, 2-digit" = 
  data.frame(
    value = 1:10,
    name = c(
      "Agriculture",
      "Manuf. & Extr.",
      "Construction",
      "Retail",
      "Info & Com",
      "Finance",
      "Real Estate",
      "Services to Firms",
      "Public",
      "Other Services"
    ),
    stringsAsFactors = FALSE
  ), 
  "Occupation" = 
  data.frame(
    value = 1:7,
    name = c(
      "Farmers",
      "CEO, business owners",
      "Professional (high wage)",
      "Intermediate",
      "Employees",
      "Blue-collars",
      "Pensioner"
    ),
    stringsAsFactors = FALSE
  ),
  "Educational Attainment" = 
  data.frame(
    value = 1:8,
    name = c(
      "No diploma",
      "Elementary school",
      "Junior High School",
      "Vocational basic",
      "Vocational advanced",
      "High School Grad.",
      "Undergraduate Univ.",
      "Univ. Graduate"
    ),
    stringsAsFactors = FALSE
  )
)
