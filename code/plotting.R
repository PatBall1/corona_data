devtools::install_github("RamiKrispin/coronavirus", force = TRUE)
library(coronavirus)
library(wpp2019)
library(dplyr)
library(tidyr)
library(ggplot2)

data("coronavirus")
corona.data <- as_tibble(coronavirus)
corona.sp <- coronavirus_spatial()
corona.sp$iso_n3 <- as.integer(corona.sp$iso_n3)
corona.sp <- as_tibble(corona.sp) %>%
  select(iso_n3)
corona.data <- bind_cols(corona.sp, corona.data)

data(pop)
pop <- as_tibble(pop) %>%
  select(country_code, "2020") %>%
  rename(iso_n3=country_code, population.2020="2020") %>%
  mutate(population.2020=population.2020*1000)


corona.data <- left_join(corona.data, pop, by="iso_n3")


#setdiff(pop$name, coronavirus$Country.Region)
#setdiff(coronavirus$Country.Region, pop$name)




paste("Lastest date", max(coronavirus$date))

#corona <- corona.data %>%
#  filter(Province.State=="") %>%
#  select(-4, -5) %>%
#  mutate(cum.cases=ave(cases, Country.Region, type, FUN=cumsum))

corona <- as_tibble(corona.data) %>%
  filter(Province.State=="") %>%
  select(-4, -5) %>%
  group_by(Country.Region, type) %>%
  mutate(cum.cases=cumsum(cases)) %>%
  mutate(cases.rel=cases/population.2020) %>%
  mutate(cum.cases.rel=cum.cases/population.2020)

#countries <- sample(unique(corona$Country.Region), 15)
countries <- c("United Kingdom", "Korea, South", "Spain", "France", "Italy", "Germany","US", "Sweden")
#countries <- "Italy"

case.type <- "death"

##### PLOTS #####

corona %>%
  filter(type==case.type, Country.Region %in% countries, date>"2020-02-20") %>%
  ggplot(aes(x=date, y=log(cum.cases.rel))) +
  ggtitle(case.type) +
  geom_smooth(aes(col=Country.Region, linetype=Country.Region), size=1) 

corona %>%
  filter(Country.Region %in% countries, date>"2020-02-20") %>%
  ggplot(aes(x=date, y=cum.cases)) +
  ggtitle(case.type) +
  geom_smooth(aes(col=Country.Region, linetype=type), size=1)

corona %>%
  filter(type==case.type, Country.Region %in% countries, date>"2020-02-20") %>%
  ggplot(aes(x=date, y=log(cum.cases))) +
  ggtitle(case.type) +
  geom_smooth(aes(col=Country.Region), size=1)

##### SUMMARY #####

summary_df <- coronavirus %>%
  filter(Province.State=="") %>%
  group_by(Country.Region, type) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases)

