#############################
# Analysis ERC grants per country and institutions
# July 2019
# @EdudinGonzalo
#############################
library(tidyverse)
library(lubridate)
library(readxl)

# ERC projects  - Countries 

advanced =  read_excel("data/erc/erc_advanced_country.xlsx", sheet =  1) %>% gather(year, advanced_grant, -Country) %>% arrange(year)
consolidator =  read_excel("data/erc/erc_consolidator_country.xlsx", sheet =  1) %>% gather(year, consolidator_grant, -Country) %>% arrange(year)
proof =  read_excel("data/erc/erc_proof_of_concept_country.xlsx", sheet =  1) %>% gather(year, proof_grant, -Country) %>% arrange(year)
starting =  read_excel("data/erc/erc_starting_country.xlsx", sheet =  1) %>% gather(year, starting_grant, -Country) %>% arrange(year)
synergy =  read_excel("data/erc/erc_synergy_grants_country.xlsx", sheet =  1) %>% gather(year, synergy_grant, -Country) %>% arrange(year)

# ------


# ------------
# Figure 7
# -----------


# starting grant


# data prep ----------------------------
starting_change = starting %>%
  filter(!is.na(starting_grant)) %>%
  group_by(Country) %>%
  filter(row_number() %in% c(1, n())) %>%
  arrange(Country)
  


# plot  -----------------------------
  starting_country_plot = starting_change %>%
    ggplot(., aes(starting_grant, Country)) +
    geom_line(aes(group = Country), color = 'grey50', alpha = 0.5) +
    geom_point(aes(color = year), alpha = 0.875, size = 5) +
    scale_colour_viridis_d(name ="Year",
                           guide = guide_legend(title.position = "top",
                                           nrow = 5, title.hjust = 0.5, option = "cividis")) +
    labs(x = "Number of projects", y = "", 
         #title = "Projects funded under the Starting Grant (StG)",
         #title = "Variation over time", 
         caption = "Note: Countries with a single point show no time variation between first and last year") +
    theme_minimal(base_size = 18, "Avenir") + 
    theme(legend.position = "right",
          text= element_text(size = 18, family="Avenir"), 
          plot.title = element_text(color="black", face="bold"),
          axis.title.x = element_text(margin = margin(t = 0.9, unit = "cm"))) + 
    font("caption", 12)
  
  ggsave( "starting_country_plot.png", width = 25, height = 30, units = "cm", starting_country_plot)


  
# ------------
# Figure 9
# -----------
  
# consolidator grants
  
# data prep ----------------------------
  consolidator_change = consolidator %>%
    filter(!is.na(consolidator_grant)) %>%
    group_by(Country) %>%
    filter(row_number() %in% c(1, n())) %>%
    arrange(Country)

# plot --------------------------------  
  consolidator_country_plot = consolidator_change %>%
    ggplot(., aes(consolidator_grant, Country)) +
    geom_line(aes(group = Country), color = 'grey50', alpha = 0.5) +
    geom_point(aes(color = year), alpha = 0.875, size = 5) +
    scale_colour_viridis_d(name ="Year",
                           guide = guide_legend(title.position = "top",
                                                nrow = 3, title.hjust = 0.5, option = "cividis")) +
    labs(x = "Number of projects", y = "", 
         #title = "Projects funded under the Consolidator Grant (CoG)",
         subtitle = "Variation over time", 
         caption = "Note: Countries with a single point show no time variation between first and last year") +
    theme_minimal() + 
    theme(legend.position = "right",
          text= element_text(size = 18, family="Avenir"), 
          plot.title = element_text(color="black", face="bold"), 
          axis.title.x = element_text(margin = margin(t = 0.9, unit = "cm"))) + 
    font("caption", 12)
  
  ggsave( "consolidator_country_plot.png", width = 25, height = 30, units = "cm", consolidator_country_plot)
  
  
# -----------
# Figure 11
# -----------
  
# advanced grants 
  
# data prep ---------------------------------
  
  advanced_change = advanced %>%
    filter(!is.na(advanced_grant)) %>%
    group_by(Country) %>%
    filter(row_number() %in% c(1, n())) %>%
    arrange(Country)
 
# plot ------------------------------------   
  advanced_country_plot = advanced_change %>%
    ggplot(., aes(advanced_grant, Country)) +
    geom_line(aes(group = Country), color = 'grey50', alpha = 0.5) +
    geom_point(aes(color = year), alpha = 0.875, size = 5) +
    scale_colour_viridis_d(name ="Year",
                           guide = guide_legend(title.position = "top",
                                                nrow = 4, title.hjust = 0.5, option = "cividis")) +
    labs(x = "Number of projects", y = "", 
         #title = "Projects funded under the Advanced Grant (AdG)",
         #subtitle = "Variation over time", 
         caption = "Note: Countries with a single point show no time variation between first and last year") +
    theme_minimal() + 
    theme(legend.position = "right",
          text= element_text(size = 18, family="Avenir"), 
          plot.title = element_text(color="black", face="bold")) + 
    font("caption", 12)
  
  ggsave( "advanced_country_plot.png", width = 25, height = 30, units = "cm", advanced_country_plot)
  
  
  

# ERC projects  - Institutions # -----------------------------------

# ----------------
# Table 2
# ---------------- 
  
  

starting = read_csv("data/erc/erc_starting.csv") %>% mutate(grant = "starting")
advanced = read_csv("data/erc/erc_advanced.csv") %>% mutate(grant = "advanced")
consolidator = read_csv("data/erc/erc_consolidator.csv") %>% mutate(grant = "consolidator")


grants = bind_rows(advanced, consolidator, starting) %>% rename(grant_type  =  grant)

grants = grants %>%
    mutate(duration_project = round(date_end_project - date_start_project)/(365.25/12),
     duration_project = round(duration_project,0)) # duration months 
 
# analysis ------------------------

figures_grants = grants %>%  
   group_by(grant_type) %>%
   summarise_at(vars(budget_project, duration_project),
                    list(sum, mean, min, max))


names(figures_grants) = c("grant_type", "sum_budget", "sum_duration", 
                          "mean_budget", "mean_duration", "min_budget",
                          "min_duration", "max_budget", "max_duration")


# main instituions 


# by number of projects

grants %>%
group_by(institution) %>%
 tally() %>%
arrange(desc(n)) %>%
top_n(15) %>%
mutate(institution = to_upper_camel_case(institution, sep_out = " ")) %>%
  #slice(c(1:10, seq(n() - 10, n()))) %>%
  mutate(institution = fct_reorder(institution, n)) %>%
  ggplot(aes(institution, n))+
  geom_point() 
  
  
    
  





