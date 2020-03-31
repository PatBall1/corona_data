library(wpp2019)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)
source("./code/functions.R")


confirmed.url <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
deaths.url <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

confirmed <- read_csv(confirmed.url)
deaths <- read_csv(deaths.url)

# Tidy raw data
confirmed <- tidy.data(confirmed, "confirmed")
deaths <- tidy.data(deaths, "death")  
paste("Lastest date", max(confirmed$date))

corona.data <- bind_rows(confirmed, deaths)
corona.data
rm(confirmed, deaths)

data(pop)
rm(popF, popFT, popM, popMT)
pop <- as_tibble(pop) %>%
  select(country_code, "2020") %>%
  rename(iso3n=country_code, population.2020="2020") %>%
  mutate(population.2020=population.2020*1000)

corona.data <- left_join(corona.data, pop, by="iso3n")

corona <- corona.data %>%
  select(-4, -5) %>%
  group_by(Prov.State, Country, type) %>%
  mutate(cum.cases=cumsum(cases)) %>%
  mutate(cases.rel=cases/population.2020) %>%
  mutate(cum.cases.rel=cum.cases/population.2020) %>%
  select(1:3, 7, everything())

#countries <- sample(unique(corona$Country.Region), 15)
countries <- c("United Kingdom", "France",  "US", "Sweden", "Spain", "Italy", "Greece", "Korea, South")
#countries <- "Italy"

case.type <- "death"

##### PLOTS #####

corona %>%
  filter(type==case.type, Country %in% countries, date>"2020-02-20") %>%
  ggplot(aes(x=date, y=log(cum.cases.rel))) +
  ggtitle(case.type) +
  geom_smooth(aes(col=Country, linetype=Country), size=1) 

corona %>%
  filter(type==case.type, Country %in% countries, date>"2020-02-20") %>%
  ggplot(aes(x=log(cum.cases), y=log(cases))) +
  ggtitle(case.type) +
  geom_smooth(aes(col=Country, linetype=Country), size=1) 
