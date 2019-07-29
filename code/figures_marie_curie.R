########################
# Figures Marie Curie grants
# July 2019
# @EdudinGonzalo
########################

# Figures using Marie-Curie grants data




library(tidyverse)
library(readr)
library(readxl)
library(ggpubr)
library(extrafont)





# ------------------------
# Figure 7 
# ------------------------

# load data 

marie = read_excel("data/data_horizon_europe.xlsx", sheet = 3)

# data prep

marie$propotion_physics = as.numeric(marie$propotion_physics)
marie$`budget (million)` = as.numeric(marie$`budget (million)`)


df_europe = marie %>% 
  summarise_at(vars(propotion_physics, researchers, `budget (million)`, organisations), list(mean)) %>%
  mutate(country = "EU") %>%
  select(country, everything())

marie = bind_rows(marie, df_europe)

marie$propotion_physics = round(marie$propotion_physics, 2)

# plot
marie_plot = marie %>%
  ggplot(aes(propotion_physics, fct_reorder(country, propotion_physics))) + 
  geom_point(stat = "identity", aes(size = organisations), color = "dodgerblue1") + 
  geom_segment(aes(x = 0, y = country, xend = propotion_physics-0.05, yend = country), color = "dodgerblue1") + 
  labs(x = "Share of physics projects of MC total budget (%)", y = "", 
       title = "Horizon 2020 Marie Curie Actions", 
       subtitle = "Importance of physics projects within Marie Curie actions", 
       size = "Number of organisations in the country") +
  geom_text(aes(label = propotion_physics),
            size = 4.5, hjust = -0.65,  color = "dodgerblue1") +
  scale_x_continuous(limits = c(0, 50), breaks = c(10, 20, 30, 40, 50)) +
  scale_size_continuous( range = c(.5,12.5)) + 
  theme_minimal() + 
  theme(legend.position = "bottom",
        text= element_text(size = 16, family="Avenir"),  # modify font size here
        plot.title = element_text(color="black", face="bold")) +
  guides( size = guide_legend(title.position="top", title.hjust = 0.5))


# save 
ggsave( "marie_plot.png", width = 25, height = 30, units = "cm", marie_plot)



