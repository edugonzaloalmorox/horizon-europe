#####################################
# Budget breakdown - Horizon Europe
# July 2019
# @EdudinGonzalo
####################################

# Data used: 

#     - data_horizon_europe.xlsx
# -----------------------------------


library(tidyverse)
library(readxl)
library (ggpubr)



# --------------
# Figure 1 
# --------------


# Load data

horizon = read_excel("data/data_horizon_europe.xlsx", sheet = 1)

horizon$budget = as.numeric(horizon$budget)



# Plot

pillars = horizon %>%
  ggplot(aes(budget, fct_reorder(cluster, budget), color = cluster)) +
  geom_point(stat = "identity",size = 5.75) + 
  geom_segment(aes(x = 0, y = cluster, xend = budget-0.5, yend = cluster), color = "grey50") + 
  labs(x = "Share of budget (€ billion)", y = "", 
       title = "", 
       subtitle = "Pillars and other main elements of the budget included.\nTotal budget = €100 billion") +
  geom_text(aes(color = cluster, label = budget),
            size = 6.5, hjust = -0.3) +
  scale_x_continuous(limits = c(0, 100), breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  theme_minimal(base_size = 18) + # modify font size here
  theme(legend.title=element_blank(), 
        axis.title.x = element_text(margin = margin(t = 1.2, unit = "cm")), # modify length axis-legend here
        legend.position = "none",
        text=element_text(size = 18, family="Avenir"), 
        plot.title = element_text(color="black", face="bold")) + 
  font("subtitle", size = 16)


# save
ggsave( "he_pillars.png", pillars)




breakdown = read_excel("data/data_horizon_europe.xlsx", sheet = 2)


# --------------
# Figure 2 
# --------------

# Pillar 1 ---------------------


pillar1 = breakdown %>%
  filter(type == "Open Science") %>%
  ggplot(aes(cluster, quantity, fill = programme)) + 
  geom_bar( stat = "identity", position ="dodge",  alpha = 0.85) +
  scale_fill_manual(values = "#bae4bc") + 
  coord_flip()  +
  labs( title = "",
        subtitle = "Total budget = €25.8 billion",
       y = "Budget (billion €) ", 
       x = "", 
       legend= "") +
  theme_minimal(base_size = 18, "Avenir" ) + # modify font size here
  theme(legend.position = "none",
        axis.title.x = element_text(margin = margin(t = 0.9, unit = "cm")), # modify length axis-legend here
        legend.title = element_blank(),
        text=element_text(size = 18, family="Avenir"))  + # modify font size here
  font("subtitle", size = 16) 

  


ggsave( "breakdown_pillar1.png", pillar1)


# --------------
# Figure 3 
# --------------

# Pillar 2 ---------------------


pillar2 = breakdown %>%
  filter(type == "Global Challenges") %>%
  ggplot(aes(cluster, quantity, fill = programme)) + 
  geom_bar( stat = "identity", position ="dodge",  alpha = 0.85) +
  scale_fill_manual(values = "#bae4bc") + 
  coord_flip()  +
  labs( title = "",
        subtitle = "Total budget = €52.7 billion",
        y = "Budget (billion €) ", 
        x = "", 
        legend= "") +
  theme_minimal(base_size = 18, "Avenir") +
  theme(legend.position="none",
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 0.9, unit = "cm")), # modify length axis-legend here
        text=element_text(size = 18, family="Avenir"))  +
  font("subtitle", size = 16)

ggsave( "breakdown_pillar2.png", pillar2)


# --------------
# Figure 4 
# --------------

# Pillar 3 ---------------------


pillar3 = breakdown %>%
  filter(type == "Open Innovation") %>%
  ggplot(aes(cluster, quantity, fill = programme)) + 
  geom_bar( stat = "identity", position ="dodge",  alpha = 0.85, width = 0.5) +
  scale_fill_manual(values = "#bae4bc") + 
  coord_flip()  +
  labs( title = "",
        subtitle = "Total budget = €13.5 billion",
        y = "Budget (billion €) ", 
        x = "", 
        legend= "") +
  theme_minimal(base_size = 20,"Avenir") + # modify font size here
  theme(legend.position="none",
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 0.9, unit = "cm")), # modify length axis-legend here
        text=element_text(size = 20, family="Avenir"))  + # modify font size here
  font("subtitle", size = 16)

ggsave( "breakdown_pillar3.png", pillar3)


# --------------
# Figure 5
# --------------


# Breakdown  common elements ---------------------

horizon_breakdown = breakdown %>%
  group_by(cluster) %>%
  filter(n()>1) %>%
  arrange(cluster) %>%
  ggplot(aes(cluster, quantity, fill = programme)) + 
  geom_bar( stat = "identity", position ="dodge",  alpha = 0.85) +
  scale_fill_manual(values = c("#bae4bc", "#0868ac")) + 
  coord_flip()  +
  labs( y = "Budget (billion €) ", 
        x = "", 
        legend= "") +
  theme_minimal(base_size = 18) +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        text=element_text(size = 18, family="Avenir"),  # modify font size here
        axis.title.x = element_text(margin = margin(t = 0.9, unit = "cm")))  + # modify length axis-legend here
  font("title", size = 14) + 
  font("subtitle", size = 12)

ggsave( "breakdown_horizon.png", horizon_breakdown)
  



# --------------
# Figure 6
# --------------

# Breakdown  single elements ---------------------


horizon_breakdown_single = breakdown %>%
  group_by(cluster) %>%
  filter(n() == 1) %>%
  arrange(cluster) %>%
  ggplot(aes(fct_reorder(cluster, quantity), quantity, fill = programme)) + 
  geom_bar( stat = "identity", position ="dodge",  alpha = 0.85) +
  scale_fill_manual(values = c("#bae4bc", "#0868ac")) + 
  coord_flip()  +
  labs( y = "Budget (billion €) ", 
        x = "",
        legend= "") +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        text=element_text(size = 16, family="System Font"), # modify font size here
        axis.title.x = element_text(margin = margin(t = 0.9, unit = "cm")))  + # modify length axis-legend here
  font("title", size = 12) + 
  font("subtitle", size = 10)

ggsave( "breakdown_horizon_single.png", horizon_breakdown_single)

 
  
