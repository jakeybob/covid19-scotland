library(tidyverse)
library(lubridate)
library(roll)
library(readxl)
library(ckanr)


#### PUBLICATION URLs ####
# update these as and when they change...
NRS_covid_deaths <- "https://www.nrscotland.gov.uk/files//statistics/covid19/covid-deaths-data-week-15.xlsx"
NRS_weekly_deaths <- "https://www.nrscotland.gov.uk/files//statistics/weekly-monthly-births-deaths-data/2020/mar/weekly-march-20.xlsx"


#### GET COVID DEATHS DATA ####

# NRS publication, contains Scottish daily number of deaths using both NRS and HPS methodology
download.file(url = NRS_covid_deaths, destfile = "data/NRS_covid_deaths.xlsx")

# HPS daily Scottish deaths as announced by Scottish Gov, and compiled/scraped by @watty62
download.file(url = "https://github.com/watty62/Scot_covid19/raw/master/data/processed/scot_test_positive_deceased.csv",
              destfile = "data/watt.csv")

# Will combine @watty62 data with HPS numbers from the NRS publication. The numbers announced 
# by the Scot Gov are not readily available in any official form other than twitter and transient
# webpages -- this is the only method I know of to maximise date range and timeliness for this data

df_covid_deaths <- read_csv("data/watt.csv") %>%
  mutate(date = dmy(Date), deaths_cumulative = deceased, source = "HPS") %>%
  select(date, deaths_cumulative, source) %>%
  full_join(
    read_xlsx("data/NRS_covid_deaths.xlsx", sheet = "Figure 2 data", 
              skip = 2, col_types = c("date", "numeric", "text")) %>%
      rename(date = Date, deaths_cumulative = `Cumulative Count`, source = Source) %>%
      filter(is.na(date) == FALSE) %>%
      mutate(date = ymd(date))) %>%
  mutate(date = if_else(source == "HPS", date - days(1), date)) %>%
  arrange(source, date) %>%
  group_by(source) %>%
  mutate(deaths_new = if_else(row_number() == 1, 0, deaths_cumulative - lag(deaths_cumulative)),
         deaths_new_per_day = if_else(row_number() == 1, 0, deaths_new / ((lag(date) %--% date)/days(1))),
         deaths_new_per_day_roll_week = roll_mean(deaths_new_per_day, width = 7)) %>%
  ungroup()

# test is TRUE if no missing intermediate days in each data source
df_covid_deaths %>% 
  group_by(source) %>% 
  mutate(diff = lag(date) %--% date / days(1)) %>% 
  filter(row_number() != 1) %>%
  summarise(test = all(diff == 1))


#### GET POPULATION / WEEKLY DEATHS DATA ####
# get national population numbers
ckanr_setup(url = "https://www.opendata.nhs.scot/")
res_est_1981_2018 <- resource_show(id = "27a72cc8-d6d8-430c-8b4f-3109a9ceadb1") # pop estimates
res_proj_2018_2043 <- resource_show(id = "7a9e74c9-8746-488b-8fba-0fad7c7866ea") # pop projections

data_est_1981_2018 <- ckan_fetch(x=res_est_1981_2018$url) %>%
  filter(HB == "S92000003") %>%
  select(-HB) %>% as_tibble()

data_proj_2018_2043 <- ckan_fetch(x=res_proj_2018_2043$url) %>%
  select(-Country) %>% as_tibble()

df_pop <- data_est_1981_2018 %>% filter(Year >= 2015, Year < 2018) %>%
  bind_rows(data_proj_2018_2043 %>% filter(Year >= 2018, Year <= 2020) )%>%
  distinct() %>%
  rename("year" = "Year") %>%
  group_by(year) %>%
  summarise(population = sum(AllAges)) %>%
  arrange(year)

# get national weekly deaths numbers
download.file(url = NRS_weekly_deaths, destfile = "data/NRS_weekly_deaths.xlsx")

df_weekly_deaths <- read_xlsx("data/NRS_weekly_deaths.xlsx", sheet = "2014", skip = 3)


#### PLOTS  ####
df_covid_deaths %>% 
  ggplot(aes(x = deaths_cumulative, y = deaths_new_per_day, group = source, color = source, label=date)) +
  geom_point() + geom_line() + theme_bw()

df_covid_deaths %>% 
  ggplot(aes(x = deaths_cumulative, y = deaths_new_per_day_roll_week, group = source, color = source, label=date)) +
  geom_point() + geom_line() + theme_bw()
plotly::ggplotly()

df_covid_deaths %>% 
  ggplot(aes(x = date, y = deaths_new_per_day, group = source, color = source, label=date)) +
  geom_point() + geom_line() + theme_bw() + scale_y_log10()

df_covid_deaths %>% 
  ggplot(aes(x = date, y = deaths_new_per_day_roll_week, group = source, color = source, label=date)) +
  geom_point() + geom_line() + theme_bw()

df_covid_deaths %>% 
  ggplot(aes(x = date, y = deaths_new_per_day_roll_week, group = source, color = source, label=date)) +
  geom_point() + geom_line() + theme_bw() +
  scale_y_log10()

df_covid_deaths %>%
  ggplot(aes(x = date, y = deaths_cumulative, group = source, color = source, label=date)) +
  geom_point() + geom_line() + theme_bw()

df_covid_deaths %>%
  ggplot(aes(x = date, y = deaths_cumulative, group = source, color = source, label=date)) +
  geom_point() + geom_line() + theme_bw() +
  scale_y_log10()

df_covid_deaths %>% 
  pivot_wider(date, names_from = source, values_from = deaths_cumulative) %>%
  mutate(ratio = NRS/HPS) %>% 
  ggplot(aes(x = date, y = ratio)) +
  geom_point() + geom_line() + theme_bw()

df_covid_deaths %>% 
  pivot_wider(date, names_from = source, values_from = deaths_new_per_day_roll_week) %>%
  mutate(ratio = NRS/HPS) %>% 
  ggplot(aes(x = date, y = ratio)) +
  geom_point() + geom_line() + theme_bw()
