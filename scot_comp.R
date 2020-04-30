library(tidyverse)
library(lubridate)
library(roll)
library(scales)
library(RColorBrewer)
library(patchwork)


#### SETUP ####
daily_deaths_baseline <- 3
comparison_countries <- c("Sweden", "Norway", "Finland", "Denmark", "Ireland", "Netherlands")


#### GET DATA ####
# UK four nations deaths data
download.file(url = "https://coronavirus.data.gov.uk/downloads/csv/coronavirus-deaths_latest.csv",
              destfile = "data/UK_nations_deaths.csv")
# worldwide deaths data
download.file(url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
              destfile = "data/covid19_deaths_world.csv")

# country level daily deaths, worldwide
df_world <- read_csv("data/covid19_deaths_world.csv") %>%
  pivot_longer(cols = contains("/20"), names_to = "date", values_to = "deaths") %>%
  filter(is.na(deaths) == FALSE) %>%
  mutate(date = mdy(date), 
         deaths = as.integer(deaths)) %>%
  rename(sub_area = "Province/State", area = "Country/Region", lat = "Lat", long = "Long") %>%
  arrange(area, date) %>%
  group_by(area, date) %>%
  summarise(deaths = sum(deaths)) %>%
  group_by(area) %>%
  mutate(deaths_daily = if_else(row_number() == 1, 0,  (deaths - lag(deaths)) / ((lag(date) %--% date) / days(1))),
         deaths_daily_roll_week = roll_mean(deaths_daily, width = 7)) %>%
  ungroup() %>%
  mutate(week_number = isoweek(date))

# country level daily deaths, UK four nations
df_uk <- read_csv("data/UK_nations_deaths.csv") %>%
  rename(area = `Area name`, date = `Reporting date`, 
         deaths_daily = `Daily change in deaths`, deaths = `Cumulative deaths`) %>%
  select(area, date, deaths, deaths_daily) %>%
  mutate(area = recode(area, "United Kingdom" = "UK",
                       "Northern Ireland" = "N. Ireland")) %>%
  arrange(area, date) %>%
  group_by(area) %>%
  # replacing the daily deaths data as supplied as has NAs for early UK dates and for first data points
  mutate(deaths_daily = if_else(row_number() == 1, 0,  (deaths - lag(deaths)) / ((lag(date) %--% date) / days(1))),
         deaths_daily_roll_week = roll_mean(deaths_daily, width = 7)) %>%
  ungroup() %>%
  mutate(week_number = isoweek(date))


#### PLOTS ####
# common plot elements
theme_custom <- theme_bw() +
  theme(plot.title = element_text(face="bold"),
        text=element_text(size=16, family = "Source Sans Pro"),
        panel.border = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"))

highlight_line_size <- 2
regular_line_size <- 1
highlight_alpha <- 1
regular_alpha <- .65


# UK nations comparisons
colours <- c("#000000", brewer.pal(4, "Set1")) # black for UK total, plus four others

df_uk %>%
  mutate(area = factor(area, levels = c("UK", "England", "N. Ireland", "Scotland", "Wales"))) %>%
  filter(date >= dmy("16/03/2020")) %>%
  ggplot(aes(x = date, y = deaths_daily_roll_week, group = area, color = area)) +
  geom_line(size = regular_line_size) +
  scale_colour_manual(values = colours) +
  geom_label(data = df_uk %>% group_by(area) %>% filter(date == max(date) ),
             aes(label = area), show.legend = FALSE, nudge_x = -1.5) +
  scale_y_log10(labels=comma) +
  theme_custom +
  theme(legend.position = "none",
        legend.title = element_blank(),
        text=element_text(size=12, family = "Source Sans Pro")) +
  labs(x = "", y = "deaths", title = "UK nations: average daily deaths") 
ggsave("pics/uk_comp.png", device = "png", dpi="retina", width=300, height=200, units="mm")

df_uk %>%
  arrange(date) %>%
  mutate(area = factor(area, levels = c("UK", "England", "N. Ireland", "Scotland", "Wales"))) %>%
  group_by(area) %>%
  filter(date >= dmy("01/04/2020")) %>%
  mutate(deaths_daily_log = log10(deaths_daily_roll_week)) %>%
  mutate(grad = (deaths_daily_log - lag(deaths_daily_log))) %>%
  ggplot(aes(x = date, y = grad, color = area, fill = area)) +
  geom_hline(yintercept = 0, size = 1.2) +
  geom_point() + 
  scale_colour_manual(values = colours) +
  scale_fill_manual(values = colours) +
  # geom_smooth(method = "loess", n = 7, size = highlight_line_size) +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), size = highlight_line_size) + 
  ylim(c(-.1, .1)) +
  facet_wrap(~area, nrow = 1) +
  theme_custom +
  theme(legend.position = "none",
        axis.text.x = element_text(angle=90)) +
  labs(x = "", y = "", title = "UK nations: gradient of log average daily deaths") 
ggsave("pics/uk_grad_comp.png", device = "png", dpi="retina", width=300, height=200, units="mm")


# Scotland compared to selected international countries
df_comp <- df_world %>%
  filter(area %in% comparison_countries) %>%
  bind_rows(df_uk %>% filter(area == "Scotland")) 

baseline_dates <- df_comp %>% 
  mutate(more_than_baseline = deaths_daily_roll_week >= daily_deaths_baseline) %>% 
  filter(more_than_baseline == TRUE) %>%
  group_by(area) %>%
  summarise(baseline_date = first(date))

df_comp <- df_comp %>%
  left_join(baseline_dates) %>%
  mutate(days_since_baseline = baseline_date %--% date / days(1)) %>%
  mutate(area = factor(area, levels = c("Scotland", sort(comparison_countries))))

p_comp_1 <- df_comp %>%
  filter(date >= dmy("01/03/2020")) %>%
  ggplot(aes(x = date, y = deaths_daily_roll_week, group = area, color = area)) +
  geom_line(aes(size = area, alpha = area)) + 
  scale_size_manual(values=c(highlight_line_size, rep(regular_line_size, length(comparison_countries)))) +
  scale_alpha_manual(values = c(highlight_alpha, rep(regular_alpha, length(comparison_countries)))) +
  geom_label(data = df_comp %>% group_by(area) %>% filter(date == max(date) ),
             aes(label = area), show.legend = FALSE, nudge_x = -1.5) +
  scale_y_log10() +
  scale_color_brewer(palette = "Dark2", type = "qual") +
  theme_custom +
  theme(legend.position = "none",
        legend.title = element_blank(),
        text=element_text(size=12, family = "Source Sans Pro")) +
  labs(x = "date", y = "deaths", title = "average daily deaths since March") 

p_comp_2 <- df_comp %>%
  filter(days_since_baseline >= 0) %>%
  ggplot(aes(x = days_since_baseline, y = deaths_daily_roll_week, group = area, color = area)) +
  geom_line(aes(size = area, alpha = area)) + 
  scale_size_manual(values=c(highlight_line_size, rep(regular_line_size, length(comparison_countries)))) +
  scale_alpha_manual(values = c(highlight_alpha, rep(regular_alpha, length(comparison_countries)))) +
  geom_label(data = df_comp %>% group_by(area) %>% filter(days_since_baseline == max(days_since_baseline)),
             aes(label = area), show.legend = FALSE, nudge_x = -1.5) +
  scale_y_log10() +
  scale_color_brewer(palette = "Dark2", type = "qual") +
  theme_custom +
  theme(legend.position = "right",
        legend.title = element_blank(),
        text=element_text(size=12, family = "Source Sans Pro")) +
  labs(x = "days since avg daily deaths > 3", y = "", title = "average daily deaths (rebased date)") 

p_comp_combined <- p_comp_1 + p_comp_2 +
  plot_annotation(
    title = "Daily COVID-19 Hospital Deaths in Scotland",
    subtitle = "(compared to selected countries)",
    theme = theme_custom)
p_comp_combined
ggsave("pics/scot_comp.png", device = "png", dpi="retina", width=350, height=210, units="mm")
