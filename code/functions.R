# Function to clean raw CSVs and make tidy
# library(countrycode)

tidy.data <- function(data, dtype){
  data <-
    data %>%
    rename(Prov.State=`Province/State`, Country=`Country/Region`) %>%
    pivot_longer(-(1:4), names_to="date", values_to="cases") %>%
    mutate(date=lubridate::mdy(date)) %>%
    mutate(type=dtype) %>%
    mutate(iso3n= countrycode::countrycode(Country, "country.name", "iso3n")) %>%
    mutate(cases=as.integer(cases)) %>%
    select(iso3n, everything()) %>%
    select(-cases, everything())
  return(data)
}



  
  