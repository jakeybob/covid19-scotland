library(tidyverse)
library(lubridate)
library(roll)
library(readxl)
library(ckanr)
library(scales)
library(patchwork)


#### PUBLICATION URLs ####
# update these as and when they change...
if(wday(today(), week_start = 1) >= 3 & hour(now()) >= 12){
  NRS_covid_deaths <- paste0("https://www.nrscotland.gov.uk/files//statistics/covid19/covid-deaths-data-week-", isoweek(today()) - 1, ".xlsx")
} else {
  NRS_covid_deaths <- paste0("https://www.nrscotland.gov.uk/files//statistics/covid19/covid-deaths-data-week-", isoweek(today()), ".xlsx")
}
NRS_weekly_deaths <- "https://www.nrscotland.gov.uk/files//statistics/weekly-monthly-births-deaths-data/2020/mar/weekly-march-20.xlsx"
# SG_covid_trends <- "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/04/trends-in-number-of-people-in-hospital-with-confirmed-or-suspected-covid-19/documents/trends-in-number-of-people-in-hospital-with-confirmed-or-suspected-covid-19/trends-in-number-of-people-in-hospital-with-confirmed-or-suspected-covid-19/govscot%3Adocument/HSCA%2B-%2BSG%2BWebsite%2B-%2BIndicator%2BTrends%2Bfor%2Bdaily%2Bdata%2Bpublication.xlsx"
NRS_covid_deaths_csv <- "https://statistics.gov.scot/downloads/cube-table?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fdeaths-involving-coronavirus-covid-19"


#### GET COVID DEATHS DATA ####
# NRS publication, contains Scottish daily number of deaths using both NRS and HPS methodology
download.file(url = NRS_covid_deaths, destfile = "data/NRS_covid_deaths.xlsx")

# NRS publication as csv, weekly numbers only
download.file(url = NRS_covid_deaths_csv, destfile = "data/NRS_covid_deaths.csv")

# HPS daily Scottish deaths as announced by Scottish Gov, and compiled/scraped by @watty62
# download.file(url = "https://github.com/watty62/Scot_covid19/raw/master/data/processed/scot_test_positive_deceased.csv",
#               destfile = "data/watt.csv")

# HPS daily Scottish deaths as announced by Scottish Gov
download.file(url = "https://raw.githubusercontent.com/DataScienceScotland/COVID-19-Management-Information/master/COVID19%20-%20Daily%20Management%20Information%20-%20Scotland%20-%20Deaths.csv",
              destfile = "data/sg_hps.csv")

# Will combine SG announced data with HPS numbers from the NRS publication. 
df_covid_deaths <- read_csv("data/sg_hps.csv") %>%
  mutate(date = ymd(Date), deaths_cumulative = `Number of COVID-19 confirmed deaths registered to date`, source = "HPS") %>%
  # read_csv("data/watt.csv") %>%
  # mutate(date = dmy(Date), deaths_cumulative = deceased, source = "HPS") %>%
  mutate(date = date - days(1)) %>%
  select(date, deaths_cumulative, source) %>%
  full_join(
    read_xlsx("data/NRS_covid_deaths.xlsx", sheet = "Figure 2 data",
              skip = 2, col_types = c("date", "numeric", "text")) %>%
      rename(date = Date1, deaths_cumulative = `Cumulative Count`, source = Source) %>%
      filter(is.na(date) == FALSE) %>%
      mutate(date = ymd(date))) %>%
  distinct() %>%
  # mutate(date = if_else(source == "HPS", date - days(1), date)) %>%
  arrange(source, date) %>%
  group_by(source) %>%
  mutate(week_number = isoweek(date),
         deaths_new = if_else(row_number() == 1, 0, deaths_cumulative - lag(deaths_cumulative)),
         deaths_new_per_day = if_else(row_number() == 1, 0, deaths_new / ((lag(date) %--% date)/days(1))),
         deaths_new_per_day_roll_week = roll_mean(deaths_new_per_day, width = 7)) %>%
  ungroup()

# df_covid_deaths <- read_xlsx("data/NRS_covid_deaths.xlsx", sheet = "Figure 2 data",
#                              skip = 2, col_types = c("date", "numeric", "text")) %>%
#   rename(date = Date1, deaths_cumulative = `Cumulative Count`, source = Source) %>%
#   filter(is.na(date) == FALSE) %>%
#   mutate(date = ymd(date)) %>%
#   mutate(date = if_else(source == "HPS", date - days(1), date)) %>%
#   arrange(source, date) %>%
#   group_by(source) %>%
#   mutate(week_number = isoweek(date),
#          deaths_new = if_else(row_number() == 1, 0, deaths_cumulative - lag(deaths_cumulative)),
#          deaths_new_per_day = if_else(row_number() == 1, 0, deaths_new / ((lag(date) %--% date)/days(1))),
#          deaths_new_per_day_roll_week = roll_mean(deaths_new_per_day, width = 7)) %>%
#   ungroup()

# test is TRUE if no missing intermediate days in each data source
df_covid_deaths %>% 
  group_by(source) %>% 
  mutate(diff = lag(date) %--% date / days(1)) %>% 
  filter(row_number() != 1) %>%
  summarise(test = all(diff == 1))


#### GET POPULATION / WEEKLY DEATHS DATA ####
# get national population numbers
# ckanr_setup(url = "https://www.opendata.nhs.scot/")
# res_est_1981_2018 <- resource_show(id = "27a72cc8-d6d8-430c-8b4f-3109a9ceadb1") # pop estimates
# res_proj_2018_2043 <- resource_show(id = "7a9e74c9-8746-488b-8fba-0fad7c7866ea") # pop projections
# 
# data_est_1981_2018 <- ckan_fetch(x=res_est_1981_2018$url) %>%
#   filter(HB == "S92000003") %>%
#   select(-HB) %>% as_tibble()
# 
# data_proj_2018_2043 <- ckan_fetch(x=res_proj_2018_2043$url) %>%
#   select(-Country) %>% as_tibble()
# 
# df_pop <- data_est_1981_2018 %>% filter(Year >= 2015, Year < 2018) %>%
#   bind_rows(data_proj_2018_2043 %>% filter(Year >= 2018, Year <= 2020) )%>%
#   distinct() %>%
#   rename("year" = "Year") %>%
#   group_by(year) %>%
#   summarise(population = sum(AllAges)) %>%
#   arrange(year) %>%
#   write_rds("data/pop.rds")

df_pop <- read_rds("data/pop.rds")

# get national weekly deaths numbers
download.file(url = NRS_weekly_deaths, destfile = "data/NRS_weekly_deaths.xlsx")

sheets <- c("2015 ", "2016", "2017", "2018", "2019", "2020") # note space after "2015 "

# extract weekly death stats from this extremely consistent and totally accessible xlsx file
for(i in seq_along(sheets)){
  if(i == 1){df_weekly_deaths <- tibble()}
  
  df_to_append <- read_xlsx("data/NRS_weekly_deaths.xlsx", 
                            sheet = sheets[i], skip = 3) %>%
    select(Deaths, starts_with("w/c"), starts_with("Week No")) %>%
    select(1, 2, last_col())
  
  names(df_to_append) <- c("deaths", "week_start_date", "week_number")
  
  df_to_append <- df_to_append %>%
    mutate(year = as.integer(sheets[i]), 
           week_number = as.integer(week_number),
           week_start_date = date(week_start_date))
  
  df_weekly_deaths <- df_weekly_deaths %>%
    bind_rows(df_to_append %>% filter(is.na(week_start_date) == FALSE)) %>%
    arrange(week_start_date)
  
  rm(df_to_append)
}

# weekly deaths, 5 year average, max, min etc
df_weekly_deaths_comp <- df_weekly_deaths %>%
  left_join(df_pop) %>%
  filter(year != 2020) %>%
  mutate(deaths_per_100k = 10^5*deaths/population) %>%
  group_by(week_number) %>%
  summarise(deaths_max = max(deaths),
            deaths_min = min(deaths),
            deaths_avg = mean(deaths),
            deaths_per_100k_max = max(deaths_per_100k),
            deaths_per_100k_min = min(deaths_per_100k),
            deaths_per_100k_avg = mean(deaths_per_100k))

deaths_2020 <- 
  read_csv("data/NRS_covid_deaths.csv") %>% 
  filter(FeatureCode == "S92000003",
         DateCode != "2020",
         Sex == "All",
         Age == "All",
         `Location Of Death` == "All",
         `Cause Of Death` == "All") %>%
  rename(deaths = Value,
         week_start_date = DateCode) %>%
  select(deaths, week_start_date) %>%
  mutate(week_start_date = ymd(week_start_date),
         week_number = isoweek(week_start_date),
         year = 2020) %>%
  arrange(week_number)

# add on NA rows for the rest of 2020
deaths_2020 <- deaths_2020 %>%
  bind_rows(
    tibble(week_number = 1:53,
           week_start_date = ymd("2019-12-30")+days((week_number-1) *7),
           deaths = NA_integer_,
           year = 2020) %>%
      filter(week_number > max(deaths_2020$week_number))
  ) %>% 
  # add on daily HPS and NRS numbers (summed to weekly here)
  left_join(
    df_covid_deaths %>% 
      group_by(week_number, source) %>%
      summarise(deaths = sum(deaths_new)) %>%
      pivot_wider(id_cols = week_number, names_from = source, names_prefix = "deaths_covid_", values_from = deaths)
  ) %>%
  # join on 2020 population to work out rates for HPS/NRS covid deaths
  left_join(df_pop) %>%
  mutate(deaths_total_per_100k = 10^5*deaths/population,
         deaths_covid_NRS_per_100k = 10^5*deaths_covid_NRS/population,
         deaths_covid_HPS_per_100k = 10^5*deaths_covid_HPS/population) %>%
  # join on the averaged etc rates by week, for comparison
  left_join(df_weekly_deaths_comp, by = c("week_number" = "week_number"))


#### PLOTS  ####

# common plot elements
theme_custom <- theme_bw() +
  theme(plot.title = element_text(face="bold"),
        text=element_text(size=16, family = "Source Sans Pro"),
        panel.border = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"))
  
# weekly death rates
deaths_2020 %>%
  ggplot(aes(x = week_start_date)) +
  geom_line(aes(y = deaths_total_per_100k, label = deaths, colour="all deaths")) +
  geom_line(aes(y = deaths_per_100k_avg, colour="5yr avg deaths"), alpha=.1) +
  geom_ribbon(aes(ymin = deaths_per_100k_min, ymax = deaths_per_100k_max, fill="5yr max/min deaths"), alpha=.1) +
  geom_line(aes(y = deaths_covid_HPS_per_100k + deaths_per_100k_avg, colour="HPS")) +
  geom_line(aes(y = deaths_covid_NRS_per_100k + deaths_per_100k_avg, colour="NRS")) +
  scale_colour_manual(name = "weekly deaths", 
                      values = c("all deaths" = "black",
                                "5yr avg deaths" = rgb(0,0,0,.1),
                                "HPS" = "blue",
                                "NRS" = "red"), 
                      labels = c("5yr avg", "all", "HPS COVID", "NRS COVID")) +
  scale_fill_manual(name = "5yr max/min deaths",
                    values = c("5yr max/min deaths" = "black"),
                    labels = c("5yr max/min deaths" = "5yr max/min")) +
  theme_custom +
  theme(legend.position = "right",
        legend.title = element_text(face="bold", size=10)) +
  labs(x = "", y = "deaths per 100,000 population", title = "Weekly Death Rates in Scotland 2020, compared to 5 year average")

ggsave("pics/deaths_comp_rate_year.png", device = "png", dpi="retina", width=300, height=200, units="mm")

# weekly deaths, zoomed in
all_deaths_recent <- deaths_2020 %>% 
  select(deaths, week_start_date) %>%
  filter(is.na(deaths) == FALSE) %>%
  summarise(deaths = last(deaths),
            date = last(week_start_date))

avg_deaths_recent <- deaths_2020 %>% 
  select(deaths_avg, week_start_date) %>%
  filter(week_start_date == all_deaths_recent$date) %>%
  summarise(deaths = last(deaths_avg))

NRS_covid_deaths_recent <- deaths_2020 %>% 
  select(deaths_covid_NRS, week_start_date) %>%
  filter(is.na(deaths_covid_NRS) == FALSE) %>%
  summarise(deaths = last(deaths_covid_NRS))

deaths_2020 %>%
  filter(week_number >= 11,
         week_number <= 22) %>%
  ggplot(aes(x = week_start_date)) +
  geom_line(aes(y = deaths, label = deaths, colour="all deaths")) +
  geom_line(aes(y = deaths_avg, colour="5yr avg deaths"), alpha=.1) + 
  geom_ribbon(aes(ymin = deaths_min, ymax = deaths_max, fill="5yr max/min deaths"), alpha=.1) +
  geom_line(aes(y = deaths_covid_HPS + deaths_avg, colour="HPS")) +
  geom_line(aes(y = deaths_covid_NRS + deaths_avg, colour="NRS")) +
  geom_hline(yintercept = filter(deaths_2020, deaths_covid_HPS == max(deaths_covid_HPS, na.rm=T))$deaths_covid_HPS + filter(deaths_2020, deaths_covid_HPS == max(deaths_covid_HPS, na.rm=T))$deaths_avg,
             colour = "blue", alpha=.3) +
  geom_hline(yintercept = filter(deaths_2020, deaths_covid_NRS == max(deaths_covid_NRS, na.rm=T))$deaths_covid_NRS + filter(deaths_2020, deaths_covid_NRS == max(deaths_covid_NRS, na.rm=T))$deaths_avg,
             colour = "red", alpha=.3) +
  geom_hline(yintercept = filter(deaths_2020, deaths == max(deaths, na.rm=T))$deaths,
             colour = "black", alpha=.3) +
  scale_colour_manual(name = "weekly deaths", 
                      values = c("all deaths" = "black",
                                 "5yr avg deaths" = rgb(0,0,0,.1),
                                 "HPS" = "blue",
                                 "NRS" = "red"), 
                      labels = c("5yr avg", "all", "HPS COVID", "NRS COVID")) +
  scale_fill_manual(name = "5yr max/min deaths",
                    values = c("5yr max/min deaths" = "black"),
                    labels = c("5yr max/min deaths" = "5yr max/min")) +
  scale_y_continuous(labels=comma) +
  theme_custom +
  theme(legend.position = "right",
        legend.title = element_text(face="bold", size=10)) +
  labs(x = "", y = "deaths", title = "Weekly Deaths in Scotland 2020, compared to 5 year average") +
  annotate("label", x = dmy("22/05/2020"), y = filter(deaths_2020, deaths_covid_HPS == max(deaths_covid_HPS, na.rm=T))$deaths_covid_HPS + filter(deaths_2020, deaths_covid_HPS == max(deaths_covid_HPS, na.rm=T))$deaths_avg, 
           label=paste0("max: ", (filter(deaths_2020, deaths_covid_HPS == max(deaths_covid_HPS, na.rm=T))$deaths_covid_HPS + filter(deaths_2020, deaths_covid_HPS == max(deaths_covid_HPS, na.rm=T))$deaths_avg) %>% formatC(big.mark = ",")), 
           colour="blue") +
  annotate("label", x = dmy("22/05/2020"), y = filter(deaths_2020, deaths_covid_NRS == max(deaths_covid_NRS, na.rm=T))$deaths_covid_NRS + filter(deaths_2020, deaths_covid_NRS == max(deaths_covid_NRS, na.rm=T))$deaths_avg, 
           label=paste0("max: ", (filter(deaths_2020, deaths_covid_NRS == max(deaths_covid_NRS, na.rm=T))$deaths_covid_NRS + filter(deaths_2020, deaths_covid_NRS == max(deaths_covid_NRS, na.rm=T))$deaths_avg) %>% formatC(big.mark = ",")), 
           colour="red") +
  annotate("label", x = dmy("22/05/2020"), y = filter(deaths_2020, deaths == max(deaths, na.rm=T))$deaths, 
           label=paste0("max: ", (filter(deaths_2020, deaths == max(deaths, na.rm=T))$deaths) %>% formatC(big.mark = ",")), 
           colour="black")
  # geom_label(aes(x = all_deaths_recent$date,
  #                y = NRS_covid_deaths_recent$deaths + avg_deaths_recent$deaths,
  #                label = paste0("NRS COVID deaths = ", NRS_covid_deaths_recent$deaths, 
  #                               "\n5yr avg = ", avg_deaths_recent$deaths,
  #                               "\ntotal = ", NRS_covid_deaths_recent$deaths + avg_deaths_recent$deaths)),
  #            nudge_y = -20, nudge_x = 8)

ggsave("pics/deaths_comp_recent.png", device = "png", dpi="retina", width=300, height=200, units="mm")


# HPS / NRS rolling average  daily death comparisons
max_y_axis <- df_covid_deaths$deaths_new_per_day %>% max()

p_daily_sources_raw <- df_covid_deaths %>% 
  ggplot(aes(x = date, y = deaths_new_per_day, group = source, color = source, label=date)) +
  geom_line() + 
  scale_y_log10(limits=c(1, max_y_axis)) +
  scale_color_manual(values = c("blue", "red")) +
  theme_custom +
  theme(legend.position = "right",
        legend.title = element_blank(),
        text=element_text(size=12, family = "Source Sans Pro")) +
  labs(x = "", y = "deaths", title = "Raw Daily Data") 
  
p_daily_sources_rolling <- df_covid_deaths %>% 
  ggplot(aes(x = date, y = deaths_new_per_day_roll_week, group = source, color = source, label=date)) +
  geom_line() + 
  scale_y_log10(limits=c(1, max_y_axis)) +
  scale_color_manual(values = c("blue", "red")) +
  theme_custom +
  theme(legend.position = "right",
        legend.title = element_blank(),
        text=element_text(size=12, family = "Source Sans Pro")) +
  labs(x = "", y = "deaths", title = "Rolling 7 Day Average") 

p_daily_sources_combined <- p_daily_sources_raw + p_daily_sources_rolling +
  plot_annotation(
    title = "Daily COVID19 Deaths in Scotland 2020 (using HPS/NRS methodologies)",
    subtitle = " ",
    theme = theme_custom)
p_daily_sources_combined
ggsave("pics/deaths_daily_sources_combined.png", device = "png", dpi="retina", width=300, height=200, units="mm")


# cumulative deaths
p_linear_cum <- df_covid_deaths %>%
  ggplot(aes(x = date, y = deaths_cumulative, group = source, color = source, label=date)) +
  geom_line()  +
  scale_color_manual(values = c("blue", "red")) +
  theme_custom +
  theme(legend.position = "right",
        legend.title = element_blank(),
        text=element_text(size=12, family = "Source Sans Pro")) +
  labs(x = "", y = "deaths", title = "(linear scale)") 

p_log_cum <- df_covid_deaths %>%
  ggplot(aes(x = date, y = deaths_cumulative, group = source, color = source, label=date)) +
  geom_line()  +
  scale_color_manual(values = c("blue", "red")) +
  scale_y_log10() +
  theme_custom +
  theme(legend.position = "right",
        legend.title = element_blank(),
        text=element_text(size=12, family = "Source Sans Pro")) +
  labs(x = "", y = "deaths", title = "(log scale)") 

p_cum_combined <- p_linear_cum + p_log_cum +
  plot_annotation(
    title = "Cumulative COVID19 Deaths in Scotland 2020 (using HPS/NRS methodologies)",
    subtitle = " ",
    theme = theme_custom)
p_cum_combined
ggsave("pics/cum_combined.png", device = "png", dpi="retina", width=300, height=200, units="mm")


# ratio of NRS to HPS
max_y_axis <- max(df_covid_deaths %>% 
      pivot_wider(date, names_from = source, values_from = deaths_cumulative) %>%
      mutate(ratio = NRS/HPS) %>%
      select(ratio) %>% max(na.rm = TRUE),
    df_covid_deaths %>% 
      pivot_wider(date, names_from = source, values_from = deaths_new_per_day_roll_week) %>%
      mutate(ratio = NRS/HPS) %>%
      select(ratio) %>% max(na.rm = TRUE),
    na.rm = TRUE)

p_ratio_cum <- df_covid_deaths %>% 
  pivot_wider(date, names_from = source, values_from = deaths_cumulative) %>%
  mutate(ratio = NRS/HPS) %>% 
  ggplot(aes(x = date, y = ratio)) +
  geom_line() + 
  ylim(0, max_y_axis) +
  theme_custom +
  theme(legend.position = "right",
        legend.title = element_blank(),
        text=element_text(size=12, family = "Source Sans Pro")) +
  labs(x = "", y = "ratio", title = "Cumulative Deaths Comparison") 

p_ratio_daily_roll <- df_covid_deaths %>% 
  pivot_wider(date, names_from = source, values_from = deaths_new_per_day_roll_week) %>%
  mutate(ratio = NRS/HPS) %>% 
  ggplot(aes(x = date, y = ratio)) +
  geom_line() + 
  ylim(0, max_y_axis) +
  theme_custom +
  theme(legend.position = "right",
        legend.title = element_blank(),
        text=element_text(size=12, family = "Source Sans Pro")) +
  labs(x = "", y = "ratio", title = "Daily Deaths (7 day rolling avg) Comparison") 

p_ratio_combined <- p_ratio_cum + p_ratio_daily_roll +
  plot_annotation(
    title = "Ratio of NRS to HPS COVID19 Daily Deaths",
    subtitle = " ",
    theme = theme_custom)
p_ratio_combined
ggsave("pics/ratio_combined.png", device = "png", dpi="retina", width=300, height=200, units="mm")


## extra

a <- df_covid_deaths %>% 
  ggplot(aes(x = date, y = deaths_new_per_day_roll_week, group = source, color = source, label=date)) +
  geom_line(size=1.2) + 
  scale_y_log10() +
  scale_color_manual(values = c("blue", "red")) +
  theme_custom +
  theme(legend.position = "none",
        legend.title = element_blank(),
        text=element_text(size=12, family = "Source Sans Pro")) +
  labs(x = "", y = "deaths", title = "Daily Deaths (7 day rolling avg)") 

b <- df_covid_deaths %>%
  ggplot(aes(x = date, y = deaths_cumulative, group = source, color = source, label=date)) +
  geom_line(size=1.2) +
  scale_color_manual(values = c("blue", "red")) +
  scale_y_log10() +
  theme_custom +
  theme(legend.position = "right",
        legend.title = element_blank(),
        text=element_text(size=12, family = "Source Sans Pro")) +
  labs(x = "", y = " ", title = "Cumulative Deaths") 

p <- a + b
p
ggsave("pics/comp.png", device = "png", dpi="retina", width=300, height=200, units="mm")

