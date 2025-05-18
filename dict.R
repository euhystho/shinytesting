#Dictionary-esque thing here:

#These refer to the Countries in the VDEM dataset that are not the same in the other two datasets
demo_countries <- c("Bolivia", "Burma/Myanmar", "Cape Verde", "Czechia", "Democratic Republic of the Congo", "Georgia", "German Democratic Republic", "Hong Kong", "Iran", "Laos", "North Korea", "Palestine/British Mandate", "Palestine/Gaza", "Palestine/West Bank", "Republic of the Congo", "Republic of Vietnam", "Russia", "South Korea", "Syria", "Türkiye", "United States of America", "Venezuela", "Vietnam")

#These refer to the Countries in the Hugo and Maddison dataset that are not the same in the VDEM dataset
gdp_countries <- c("Bolivia (Plurinational State of)", "Myanmar", "Cabo Verde","Czech Republic","D.R. of the Congo","Gambia","Georgia","China, Hong Kong SAR","Iran (Islamic Republic of)","Lao People's DR","D.P.R. of Korea","State of Palestine","West Bank and Gaza","West Bank and Gaza","Congo","Republic of Moldova","Russian Federation","Republic of Korea","Syrian Arab Republic","Turkey","United States","Venezuela (Bolivarian Republic of)","Viet Nam")

#These refer to the countries which may or may not have equivalents from VDEM to other datasets
incomplete_countries <- c(
  "Bhutan",
  "Cuba",
  "Eswatini",
  "Fiji",
  "Guyana",
  "Kosovo",
  "Maldives",
  "North Korea",
  "North Macedonia",
  "Palestine/British Mandate",
  "Palestine/Gaza",
  "Palestine/West Bank",
  "Republic of the Congo",
  "South Sudan",
  "Suriname",
  "Syria"
)

#These refer to the countries which may or may not have equivalents from Hugo Data to VDEM
h_countries <- c(
  "Bhutan",
  NA,
  "Eswatini",
  NA,
  "Fiji",
  "Guyana",
  "Kosovo",
  "Maldives",
  NA,
  "North Macedonia",
  NA,
  "West Bank and Gaza",
  "West Bank and Gaza",
  "Congo",
  "South Sudan",
  "Suriname",
  NA
)

#These refer to the countries which may or may not have equivalents from Maddison Data to VDEM
m_countries <- c(
  NA,
  "Cuba",
  NA,
  NA,
  NA,
  NA,
  NA,
  "D.P.R. of Korea",
  NA,
  "State of Palestine",
  NA,
  NA,
  "Congo",
  NA,
  NA,
  "Syrian Arab Republic"
)