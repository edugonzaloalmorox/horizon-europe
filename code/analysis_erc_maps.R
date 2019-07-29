##################################
# Maps ERC country participation
# Participation measured as relative share of projects done by any country in the year
# July 2019
# @EdudinGonzalo
#############################################

# Create maps with ERC Grants




rm(list = ls()) # clean environment

library(tmap)
library(tidyverse)
library(sf)
library(ggalt)
library(readxl)
library(ggpubr)

# ------------
# Figure 12
# -------------

# Load data on grants

advanced =  read_excel("data/erc/erc_advanced_country.xlsx", sheet =  1) %>% gather(year, advanced_grant, -Country) %>% arrange(year)


all_grants = bind_rows(advanced, consolidator, starting)

# ADVANCED GRANTS ------

# data prep
advanced_long = advanced %>% 
  group_by(year) %>%
  mutate(total_grants = sum(advanced_grant, na.rm = TRUE),
         share_country = (advanced_grant/total_grants)*100, 
         quintile = ntile(share_country, 4)) %>%
  mutate(Country = fct_recode(Country, "UK" = "United Kingdom"))



labs =  advanced_long %>%
  group_by(year, quintile) %>%
  filter(!is.na(share_country)) %>%
   summarise_at(vars(share_country), list(min, max)) %>%
  mutate(fn1 = round(fn1, 2), 
         fn2  = round(fn2, 2), 
    lab = paste("(", fn1,"-",fn2,"]")) %>%
  select(-fn1, -fn2) 

advanced_long = advanced_long  %>%  left_join(., labs, by = c("year",  "quintile"))



# Load geographic data

data(World)

european_countries = World %>% filter(continent == "Europe") %>%
  select(name, -geometry) %>%
  as.data.frame()

european_countries  = european_countries %>%
  mutate(name =  fct_recode(name, "UK" = "United Kingdom",
                               "Czech Republic" = "Czech Rep.")) 

european_countries$name = droplevels(european_countries$name)


europe = ggplot2::map_data("world") %>%
  filter(region %in% unique(european_countries$name))


# add missing countries to advanced long

miss_countries = setdiff(europe$region, advanced_long$Country)

miss_countries_df = data.frame(Country = rep(miss_countries, each = 10),
year = as.character(rep(2008:2017, length(miss_countries))), 
           advanced_grant = NA, 
           total_grants = NA, 
           share_country = NA, 
quintile = NA, 
lab = NA) %>%
  filter(! Country %in% c("Russia"))


advanced_long = bind_rows(advanced_long, miss_countries_df) 




europe_ext = europe %>%
  full_join(., advanced_long, by = c("region" = "Country")) %>%
  filter(! region %in% c("Russia", "Iceland"))


# Map


map_advanced = europe_ext %>%
  filter(!is.na(year), year %in% c("2008", "2013", "2017")) %>% # change number of maps displayed here - select years
  ggplot() + 
  geom_cartogram(
    map = europe,
    aes(x = long, y = lat, map_id = region, fill = share_country),
    color = "grey", size = 0.2, alpha = 0.9) +
  theme_void() +
  scale_fill_gradientn(name="Share country (%)",
                       colours=c("#f0f9e8",
                         "#bae4bc",
                         "#7bccc4",
                         "#43a2ca",
                         "#0868ac"),
                       
                       limits=c(0,30),
                       breaks = c(1, 5, 10, 15, 24), 
                       na.value="#f6eff7") +
  facet_wrap(.~ year) + 
  labs(#title = "Country participation in Advanced Grants (2008 - 2017)",
       #subtitle = "By share of projects funded per year",
       caption = "Note: White countries show data not available in the year") +
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "snow2")) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_minimal() +
  theme(text= element_text(family="System Font"),
    axis.text = element_blank()
        ,axis.title = element_blank()
        ,axis.ticks = element_blank()
        ,axis.line=element_blank()
        ,panel.grid = element_blank()
        ,legend.title = element_text(size = 22)
        ,legend.text = element_text(size = 22)
        ,legend.key.width = unit(0.25,"cm")
        ,legend.key.height = unit(0.25,"cm")
        ,plot.title = element_text(size= 6)
        ,legend.position = "bottom"
        ,plot.caption = element_text()
        ,legend.background = element_blank()
        ,panel.background = element_blank()
        ,strip.text = element_text(size = 18)) + 
  font("title", size = 30) +
  font("subtitle", size = 22) + 
  font("caption", size = 16)
  
ggsave( "map_advanced_plot_test.png", width = 45, height = 30, units = "cm", map_advanced)


rm(list = ls()) # clean environment


# ------------
# Figure 10
# ------------




######################################################

# CONSOLIDATOR GRANTS ------



consolidator =  read_excel("data/erc/erc_consolidator_country.xlsx", sheet =  1) %>% gather(year, consolidator_grant, -Country) %>% arrange(year)


consolidator_long = consolidator %>% 
  group_by(year) %>%
  mutate(total_grants = sum(consolidator_grant, na.rm = TRUE),
         share_country = (consolidator_grant/total_grants)*100, 
         quintile = ntile(share_country, 4)) %>%
  mutate(Country = fct_recode(Country, "UK" = "United Kingdom"))



labs =  consolidator_long %>%
  group_by(year, quintile) %>%
  filter(!is.na(share_country)) %>%
  summarise_at(vars(share_country), list(min, max)) %>%
  mutate(fn1 = round(fn1, 2), 
         fn2  = round(fn2, 2), 
         lab = paste("(", fn1,"-",fn2,"]")) %>%
  select(-fn1, -fn2) 

consolidator_long = consolidator_long  %>%  left_join(., labs, by = c("year",  "quintile"))

# Load geographic data

data(World)

european_countries = World %>% filter(continent == "Europe") %>%
  select(name, -geometry) %>%
  as.data.frame()

european_countries  = european_countries %>%
  mutate(name =  fct_recode(name, "UK" = "United Kingdom",
                            "Czech Republic" = "Czech Rep.")) 

european_countries$name = droplevels(european_countries$name)


europe = ggplot2::map_data("world") %>%
  filter(region %in% unique(european_countries$name))


# add missing countries to advanced long

miss_countries = setdiff(europe$region, consolidator_long$Country)

miss_countries_df = data.frame(Country = rep(miss_countries, each = 10),
                               year = as.character(rep(2008:2017, length(miss_countries))), 
                               advanced_grant = NA, 
                               total_grants = NA, 
                               share_country = NA, 
                               quintile = NA, 
                               lab = NA) %>%
  filter(! Country %in% c("Russia"))


consolidator_long = bind_rows(consolidator_long, miss_countries_df) 



europe_ext = europe %>%
  full_join(., consolidator_long, by = c("region" = "Country")) %>%
  filter(! region %in% c("Russia", "Iceland"))

# Map 

map_consolidator = europe_ext %>%
  filter(!is.na(year), year %in% c("2013", "2015", "2017")) %>%
  ggplot() + 
  geom_cartogram(
    map = europe,
    aes(x = long, y = lat, map_id = region, fill = share_country),
    color = "grey", size = 0.2, alpha = 0.9) +
  theme_void() +
  scale_fill_gradientn(name="Share country (%)",
                       colours=c("#f0f9e8",
                                 "#bae4bc",
                                 "#7bccc4",
                                 "#43a2ca",
                                 "#0868ac"), 
                       limits=c(0,30),
                       breaks = c(1, 2.5, 5, 10, 20), 
                       na.value="#f6eff7") +
  facet_wrap(.~ year) + 
  labs(#title = "Country participation in Consolidator Grants (2013 - 2017)",
       #subtitle = "By share of projects funded per year",
       caption = "Note: White countries show data not available in the year") +
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "snow2")) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_minimal() +
  theme(text= element_text(family="System Font"),
        axis.text = element_blank()
        ,axis.title = element_blank()
        ,axis.ticks = element_blank()
        ,axis.line=element_blank()
        ,panel.grid = element_blank()
        ,legend.title = element_text(size = 22)
        ,legend.text = element_text(size = 22)
        ,legend.key.width = unit(0.25,"cm")
        ,legend.key.height = unit(0.25,"cm")
        ,plot.title = element_text(size= 6)
        ,legend.position = "bottom"
        ,plot.caption = element_text()
        ,legend.background = element_blank()
        ,panel.background = element_blank()
        ,strip.text = element_text(size = 18)) + 
  font("title", size = 30) +
  font("subtitle", size = 22) + 
  font("caption", size = 16)

ggsave( "map_consolidator_plot.png", width = 45, height = 30, units = "cm", map_consolidator)

rm(list = ls()) # clean environment


# ------------
# Figure 8
# ------------


##############################################



starting =  read_excel("data/erc/erc_starting_country.xlsx", sheet =  1) %>% gather(year, starting_grant, -Country) %>% arrange(year)


# STARTING GRANTS --------



starting_long = starting %>% 
  group_by(year) %>%
  mutate(total_grants = sum(starting_grant, na.rm = TRUE),
         share_country = (starting_grant/total_grants)*100, 
         quintile = ntile(share_country, 4)) %>%
  mutate(Country = fct_recode(Country, "UK" = "United Kingdom"))



labs =  starting_long %>%
  group_by(year, quintile) %>%
  filter(!is.na(share_country)) %>%
  summarise_at(vars(share_country), list(min, max)) %>%
  mutate(fn1 = round(fn1, 2), 
         fn2  = round(fn2, 2), 
         lab = paste("(", fn1,"-",fn2,"]")) %>%
  select(-fn1, -fn2) 

starting_long = starting_long  %>%  left_join(., labs, by = c("year",  "quintile"))

# Load geographic data

data(World)

european_countries = World %>% filter(continent == "Europe") %>%
  select(name, -geometry) %>%
  as.data.frame()

european_countries  = european_countries %>%
  mutate(name =  fct_recode(name, "UK" = "United Kingdom",
                            "Czech Republic" = "Czech Rep.")) 

european_countries$name = droplevels(european_countries$name)


europe = ggplot2::map_data("world") %>%
  filter(region %in% unique(european_countries$name))


# add missing countries to advanced long

miss_countries = setdiff(europe$region, starting_long$Country)

miss_countries_df = data.frame(Country = rep(miss_countries, each = 12),
                               year = as.character(rep(2007:2018, length(miss_countries))), 
                               starting_grant = NA, 
                               total_grants = NA, 
                               share_country = NA, 
                               quintile = NA, 
                               lab = NA) %>%
  filter(! Country %in% c("Russia"))


starting_long = bind_rows(starting_long, miss_countries_df) 



europe_ext = europe %>%
  full_join(., starting_long, by = c("region" = "Country")) %>%
  filter(! region %in% c("Russia", "Iceland"))



# Map 

map_starting = europe_ext %>%
  filter(!is.na(year), year %in% c("2007", "2013", "2018")) %>%
  ggplot() + 
  geom_cartogram(
    map = europe,
    aes(x = long, y = lat, map_id = region, fill = share_country),
    color = "grey", size = 0.2, alpha = 0.8) +
  theme_void() +
  scale_fill_gradientn(name="Share country (%)",
                       colours=c("#f0f9e8",
                                 "#bae4bc",
                                 "#7bccc4",
                                 "#43a2ca",
                                 "#0868ac"),
                       #values=c(0.7042,1.5149,3.1746,7.8125,26.7606),
                       limits=c(0,30),
                       breaks = c(0.5, 1, 5, 10, 23), 
                       na.value="#f6eff7") +
  facet_wrap(.~ year) + 
  labs(#title = "Country participation in Starting Grants (2007 - 2018)",
       #subtitle = "By share of projects funded per year",
       caption = "Note: White countries show data not available in the year") +
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "snow2")) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_minimal() +
  theme(text= element_text(family="System Font"),
        axis.text = element_blank()
        ,axis.title = element_blank()
        ,axis.ticks = element_blank()
        ,axis.line=element_blank()
        ,panel.grid = element_blank()
        ,legend.title = element_text(size = 22)
        ,legend.text = element_text(size = 22)
        ,legend.key.width = unit(0.25,"cm")
        ,legend.key.height = unit(0.25,"cm")
        ,plot.title = element_text(size= 6)
        ,legend.position = "bottom"
        ,plot.caption = element_text()
        ,legend.background = element_blank()
        ,panel.background = element_blank()
        ,strip.text = element_text(size = 18)) + 
  font("title", size = 30) +
  font("subtitle", size = 22) + 
  font("caption", size = 16)

ggsave( "map_starting_plot.png", width = 45, height = 30, units = "cm", map_starting)













