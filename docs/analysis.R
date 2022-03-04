# libraries
library(dplyr)
library(tidyverse)
library(openintro)
library(ggplot2)

# county-level data
incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# jurisdiction-level data
jail_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends_jail_jurisdiction.csv")

# Washington most recent jailed population (black vs white)
wa_black_diff<- incarceration_data %>%
  filter(state == "WA", year == "2018", county_name == "King County") %>%
  group_by(state) %>%
  summarize(diff_jail = white_jail_pop - black_jail_pop) %>%
  pull(diff_jail)

# State with the highest proportion of black jail population
total_jail_ratio <- incarceration_data %>%
  mutate(jail_ratio = black_jail_pop / total_jail_pop) %>%
  filter(jail_ratio == max(jail_ratio, na.rm=TRUE)) %>%
  pull(state)

# State with the highest sum of proportion of black jail population in 2018
total_recent_jail_ratio <- incarceration_data %>%
  filter(!is.na(black_jail_pop), year == "2018") %>%
  mutate(jail_ratio_recent = black_jail_pop / total_jail_pop) %>%
  select(jail_ratio_recent, state) %>%
  group_by(state) %>%
  summarize(total_jail_ratio = sum(jail_ratio_recent, na.rm=TRUE)) %>%
  filter(total_jail_ratio == max(total_jail_ratio)) %>%
  pull(state)

# County with the highest proportion of people jailed from ICE
ice_ratio <- incarceration_data %>%
  mutate(ice_prop = total_jail_from_ice / total_jail_pop) %>%
  filter(ice_prop == max(ice_prop, na.rm=TRUE)) %>%
  pull(county_name)

# County with highest jailed Latino population
latinx_wa_highest <- incarceration_data %>%
  filter(latinx_jail_pop == max(latinx_jail_pop, na.rm=TRUE)) %>%
  pull(county_name)
