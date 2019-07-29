##############################
# Scrapping ERC grants
# May 2019
# Version: 1.0
# @Edu Gonzalo
# First page has a diffrent url
################################

#################################
# CONSOLIDATOR GRANTS
##################################


# page 1 (100 results)



library(rvest)
library(tidyverse)

get_grants=  function(url){
  
  data <- read_html(url)
  
  # Scrap variables
  project_acronym =  data %>%  html_nodes(xpath = '//*[@class="views-field views-field-acronym"]') %>% html_text() %>% as.data.frame()
  project_name = data %>%  html_nodes(xpath = '//*[@class="views-field views-field-title"]') %>% html_text() %>% as.data.frame()
  researcher = data %>%  html_nodes(xpath = '//*[@class="views-field views-field-xml-researcher"]') %>% html_text() %>% as.data.frame()
  institution =  data %>%  html_nodes(xpath = '//*[@class="views-field views-field-hostInstitution-name"]') %>% html_text() %>% as.data.frame()
  call_details = data %>%  html_nodes(xpath = '//*[@class="views-field views-field-call-details"]') %>% html_text() %>% as.data.frame()
  summary = data %>%  html_nodes(xpath = '//*[@class="views-field views-field-objective"]') %>% html_text() %>% as.data.frame()
  names(summary) = "details_project"
  
  # get dates and budget 
  budget = data %>% html_nodes(xpath = '//*[@class="collapse"]') %>% html_text() %>% as.data.frame()
  
  names(budget) = "funding"
  
  budget = budget %>% mutate(funding_clean = gsub(".*Max ERC Funding", "", funding), 
                             funding_clean2 = str_split(funding_clean, "\r\n", 4),
                             money = str_extract(funding_clean, ".*€"),
                             start_date = str_extract(funding_clean, ".*,"),
                             end_date = str_extract(funding_clean, ",.*"))
  
  budget_clean = budget %>%
    select(money, start_date, end_date) %>%
    mutate(budget_project = gsub("€" ,"" , money),
           budget_project = gsub("[[:blank:]]", "", budget_project),
           date_start_project = gsub("Start date: ", "", start_date),
           date_start_project = gsub(",", "", date_start_project), 
           date_end_project = gsub(", End date: ", "", end_date)) %>%
    select(budget_project, date_start_project, date_end_project) %>%
    as_tibble()
  
  consolidator_grant = bind_cols(project_acronym,
                                 project_name, 
                                 researcher,
                                 institution, 
                                 summary,
                                 budget_clean) %>% as_tibble()
  
  names(consolidator_grant) = c("project_acronym", "project", "main_researcher", 
                                "institution", "details_project", "budget_project",
                                "date_start_project", "date_end_project")
  
  
  # cleaning 
  
  consolidator_grant = consolidator_grant %>% 
    mutate(project_acronym = gsub(" Project acronym", "", project_acronym),
           project_acronym = str_trim(project_acronym, side = "both"),
           project = gsub("   Project    ", "", project),
           project = str_trim(project, "both"),
           main_researcher  = gsub("   Researcher ", "", main_researcher),
           main_researcher = gsub(" ^(PI) ", "", main_researcher),
           institution = gsub("    Host Institution", "", institution))
  
  
  
  
  
  consolidator_grant = consolidator_grant %>% 
    mutate(main_researcher = gsub(".*)", "", main_researcher),
           main_researcher = str_trim(main_researcher, "both"),
           institution = gsub(".*)", "", institution),
           institution = str_trim(institution, "both"),
           details_project = gsub("Summary", "", details_project), 
           details_project = str_trim(details_project, "both"))
  
  
  return(consolidator_grant)  
  
  
}

url = "https://erc.europa.eu/projects-figures/erc-funded-projects/results?search_api_views_fulltext=&items_per_page=100&f%5B0%5D=tid%253Aparents_all%3A59&f%5B1%5D=tid%253Aparents_all%3A62&f%5B2%5D=tid%253Aparents_all%3A63&f%5B3%5D=tid%253Aparents_all%3A64&f%5B4%5D=tid%253Aparents_all%3A65&f%5B5%5D=tid%253Aparents_all%3A66&f%5B6%5D=tid%253Aparents_all%3A67&f%5B7%5D=tid%253Aparents_all%3A68&f%5B8%5D=tid%253Aparents_all%3A69&f%5B9%5D=tid%253Aparents_all%3A70&f%5B10%5D=tid%253Aparents_all%3A71&f%5B11%5D=funding_scheme%3AConsolidator%20Grant%20%28CoG%29"

consolidator_grant1 = get_grants(url)


# consolidations (101-876)

urls = paste0("https://erc.europa.eu/projects-figures/erc-funded-projects/results?items_per_page=100&search_api_views_fulltext=&f%5B0%5D=tid%253Aparents_all%3A59&f%5B1%5D=tid%253Aparents_all%3A62&f%5B2%5D=tid%253Aparents_all%3A63&f%5B3%5D=tid%253Aparents_all%3A64&f%5B4%5D=tid%253Aparents_all%3A65&f%5B5%5D=tid%253Aparents_all%3A66&f%5B6%5D=tid%253Aparents_all%3A67&f%5B7%5D=tid%253Aparents_all%3A68&f%5B8%5D=tid%253Aparents_all%3A69&f%5B9%5D=tid%253Aparents_all%3A70&f%5B10%5D=tid%253Aparents_all%3A71&f%5B11%5D=funding_scheme%3AConsolidator%20Grant%20%28CoG%29&page=",1:8)


consolidators = map(urls, get_grants)


df_consolidator = map_df(consolidators, bind_rows)

erc_consolidator = bind_rows(consolidator_grant1, df_consolidator)



write.csv(erc_consolidator, "data/erc/erc_consolidator.csv", row.names = FALSE)


##################
# STARTING GRANTS 
##################

# first page

url = "https://erc.europa.eu/projects-figures/erc-funded-projects/results?items_per_page=100&f%5B0%5D=tid%253Aparents_all%3A59&f%5B1%5D=tid%253Aparents_all%3A62&f%5B2%5D=tid%253Aparents_all%3A63&f%5B3%5D=tid%253Aparents_all%3A64&f%5B4%5D=tid%253Aparents_all%3A65&f%5B5%5D=tid%253Aparents_all%3A66&f%5B6%5D=tid%253Aparents_all%3A67&f%5B7%5D=tid%253Aparents_all%3A68&f%5B8%5D=tid%253Aparents_all%3A69&f%5B9%5D=tid%253Aparents_all%3A70&f%5B10%5D=tid%253Aparents_all%3A71&f%5B11%5D=funding_scheme%3AStarting%20Grant%20%28StG%29"

starting1 = get_grants(url)

# subsequent pages
urls_starting = paste0("https://erc.europa.eu/projects-figures/erc-funded-projects/results?items_per_page=100&f%5B0%5D=tid%253Aparents_all%3A59&f%5B1%5D=tid%253Aparents_all%3A62&f%5B2%5D=tid%253Aparents_all%3A63&f%5B3%5D=tid%253Aparents_all%3A64&f%5B4%5D=tid%253Aparents_all%3A65&f%5B5%5D=tid%253Aparents_all%3A66&f%5B6%5D=tid%253Aparents_all%3A67&f%5B7%5D=tid%253Aparents_all%3A68&f%5B8%5D=tid%253Aparents_all%3A69&f%5B9%5D=tid%253Aparents_all%3A70&f%5B10%5D=tid%253Aparents_all%3A71&f%5B11%5D=funding_scheme%3AStarting%20Grant%20%28StG%29&page=", 1:19)


starting = map(urls_starting, get_grants)

df_starting = map_df(starting, bind_rows)

erc_starting = bind_rows(starting1, df_starting)

write.csv(erc_starting, "data/erc/erc_starting.csv", row.names = FALSE)

rm(list=setdiff(ls(), "get_grants"))



###################
# ADVANCED GRANTS 
###################

# first page 

url = "https://erc.europa.eu/projects-figures/erc-funded-projects/results?items_per_page=100&f%5B0%5D=tid%253Aparents_all%3A59&f%5B1%5D=tid%253Aparents_all%3A62&f%5B2%5D=tid%253Aparents_all%3A63&f%5B3%5D=tid%253Aparents_all%3A64&f%5B4%5D=tid%253Aparents_all%3A65&f%5B5%5D=tid%253Aparents_all%3A66&f%5B6%5D=tid%253Aparents_all%3A67&f%5B7%5D=tid%253Aparents_all%3A68&f%5B8%5D=tid%253Aparents_all%3A69&f%5B9%5D=tid%253Aparents_all%3A70&f%5B10%5D=tid%253Aparents_all%3A71&f%5B11%5D=funding_scheme%3AAdvanced%20Grant%20%28AdG%29"

advanced1 = get_grants(url)

# subsequent pages
urls_advanced = paste0("https://erc.europa.eu/projects-figures/erc-funded-projects/results?items_per_page=100&f%5B0%5D=tid%253Aparents_all%3A59&f%5B1%5D=tid%253Aparents_all%3A62&f%5B2%5D=tid%253Aparents_all%3A63&f%5B3%5D=tid%253Aparents_all%3A64&f%5B4%5D=tid%253Aparents_all%3A65&f%5B5%5D=tid%253Aparents_all%3A66&f%5B6%5D=tid%253Aparents_all%3A67&f%5B7%5D=tid%253Aparents_all%3A68&f%5B8%5D=tid%253Aparents_all%3A69&f%5B9%5D=tid%253Aparents_all%3A70&f%5B10%5D=tid%253Aparents_all%3A71&f%5B11%5D=funding_scheme%3AAdvanced%20Grant%20%28AdG%29&page=", 1:12)


advanced = map(urls_advanced, get_grants)

df_advanced = map_df(advanced, bind_rows)

erc_advanced = bind_rows(advanced1, df_advanced)

write.csv(erc_advanced, "data/erc/erc_advanced.csv", row.names = FALSE)







