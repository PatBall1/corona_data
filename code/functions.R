# Function to clean raw CSVs and make tidy
# library(countrycode)

tidy.data <- function(data, dtype){
  data <-
    data %>%
    rename(Prov.State=`Province/State`, Country=`Country/Region`) %>%
    pivot_longer(-(1:4), names_to="date", values_to="cum.cases") %>%
    mutate(date=lubridate::mdy(date)) %>%
    mutate(type=dtype) %>%
    mutate(iso3n= countrycode::countrycode(Country, "country.name", "iso3n")) %>%
    mutate(cum.cases=as.integer(cum.cases)) %>%
    select(iso3n, everything()) %>%
    select(-cum.cases, everything())
  return(data)
}



  
  