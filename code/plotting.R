devtools::install_github("RamiKrispin/coronavirus", force = TRUE)
library(coronavirus)
library(wpp2019)
library(dplyr)
library(tidyr)
library(ggplot2)

data("coronavirus")
coronavirus <- as_tibble(coronavirus)
corona.sp <- coronavirus_spatial()
corona.sp$iso_n3 <- as.integer(corona.sp$iso_n3)
corona.sp <- as_tibble(corona.sp) %>%
  select(Country.Region, iso_n3, -geometry)
corona <- full_join(corona.sp, coronavirus, by="Country.Region")

data(pop)
pop <- as_tibble(pop) %>%
  select(country_code, "2020") %>%
  rename(iso_n3=country_code, population.2020="2020")


corona <- left_join(corona, pop, by="iso_n3")


#setdiff(pop$name, coronavirus$Country.Region)
#setdiff(coronavirus$Country.Region, pop$name)




paste("Lastest date", max(coronavirus$date))

#corona <- as_tibble(coronavirus) %>%
#  filter(Province.State=="") %>%
#  select(-3, -4) %>%
#  mutate(cum.cases=ave(cases, Country.Region, type, FUN=cumsum))

corona <- as_tibble(corona) %>%
  filter(Province.State=="") %>%
  select(-4, -5) %>%
  group_by(Country.Region, type) %>%
  mutate(cum.cases=cumsum(cases))


countries <- sample(unique(corona$Country.Region), 15)
countries <- c("United Kingdom", "Korea, South", "Spain", "France", "Italy", "Germany")

case.type <- "confirmed"

corona %>%
  filter(type==case.type, Country.Region %in% countries, date>"2020-02-20") %>%
  ggplot(aes(x=date, y=cum.cases/population.2020)) +
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

summary_df <- coronavirus %>%
  filter(Province.State=="") %>%
  group_by(Country.Region, type) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases)

populations <- pop %>%
  select(name, "2020") %>%
  rename(Country.Region=name)

left_join(summary_df, populations)

UK_cases <- as_tibble(coronavirus) %>%
  filter(Country.Region=="United Kingdom", Province.State=="", type=="confirmed") %>%
  mutate(cum.cases=cumsum(cases))

Korea <- coronavirus %>%
  filter(Country.Region=="Korea, South")

cases <- 
  filter(Korea_cases, type=="confirmed")
deaths <- filter(Korea_cases, type=="death")



plot(cases$date, cumsum(cases$cases), type='l')
lm(cases ~ date , cases)
