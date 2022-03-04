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
  pull(state)

# County with highest jailed Latino population
latinx_wa_highest <- incarceration_data %>%
  filter(latinx_jail_pop == max(latinx_jail_pop, na.rm=TRUE)) %>%
  pull(county_name)

# Trends Over Time Chart
black_prop <- incarceration_data %>%
  filter(state == "WA", year >= "1985", !is.na(black_jail_pop))

black_jail_over_time <- ggplot(data=black_prop,
       mapping = aes(x = year, y = black_jail_pop, fill = urbanicity)) +
  geom_bar(stat="identity") +
  scale_fill_manual(breaks = c("rural", "small/mid", "suburban", "urban"),
                          values=c("#0B2B26", "#235347", "#47735D", "#8EB69B")) +
  labs(title="Black Jail Population in Washington State from 1985 to 2018",
       x="Year",
       y="Population", 
       fill="Urbanicity")

# Variable Comparison Chart 
urban <- c("rural", "urban", "small/mid", "suburban")

var_df <- incarceration_data %>%
  filter(urbanicity %in% urban, year >= "1985") %>%
  group_by(urbanicity) %>%
  summarize(white = sum(black_jail_pop, na.rm=TRUE),
            black = sum(white_jail_pop, na.rm=TRUE)) %>%
  pivot_longer(cols = 2:3, names_to = "race", values_to = "population")

comp_bvw <- ggplot(data=var_df,
       mapping = aes(x=urbanicity, y=population, fill=race)) +
         geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(breaks = c("black", "white"),
                    values=c("#47735D", "#8EB69B")) +
  labs(title="Black vs White Incarcerated Population",
       subtitle="Comparison Separated by Urbanicity",
       x="Urbanicity",
       y="Incarcerated Population",
       fill="Race")

# Map
# libraries
library(maps)
library(mapproj)
library(patchwork)

# new data frame
wa_high <- incarceration_data %>%
  filter(!is.na(latinx_jail_pop), year == "2018") %>%
  mutate(jail_prop = latinx_jail_pop / total_jail_pop) %>%
  select(yfips, year, fips, county_name, state, latinx_jail_pop, jail_prop)

# creating county shape data frame 
county_shape <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by="polyname")

# Joining county_shape with wa_high
map_data <- county_shape %>%
  left_join(wa_high, by="fips") %>%
  filter(year=="2018", state=="WA")

# Map Theme
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

# Creating the Map
incarc_map <- ggplot(map_data) +
  geom_polygon(mapping = aes(x=long, y=lat, group=group, fill=jail_prop),
               color="grey", size=0.1) +
  coord_map() +
  scale_fill_continuous(limits = c(0, max(map_data$jail_prop)),
                        na.value= "white",low="grey", high="dark green", 
                        name="proportion") +
  ggtitle("Proportion of Incarcerated Latinos Across Washington Counties in 2018") +
  blank_theme








