### use ICEWS_event_data and trade data to look at pairwise  relations ####
library(tidyverse)
library(forcats)
library(knitr)
library(countrycode)
library(stringr)

# get google countries
g_countries <- read_csv("../../data/supplementary_data/cultural_sim_measures/geo/all_google_countries.csv")

########## EVENT DATA ##############
# read in event data
event_data <- read_tsv("../../data/supplementary_data/cultural_sim_measures/events/ICEWS_event_data/events.2015.20170206133646.tab") 
  
g_event_data <- event_data %>%
            mutate(country_code_1 = countrycode(`Source Country`, 
                                                'country.name', 'iso2c'),
                   country_code_2 = countrycode(`Target Country`, 
                                                'country.name', 'iso2c')) %>%
           filter(country_code_1 %in% g_countries$country_code,
                  country_code_2 %in% g_countries$country_code) %>%
           rename(event_text = `Event Text`, 
                 event_id = `Event ID`)

######## get all events ########   
g_all_events <- g_event_data %>%
  select(event_id, event_text, country_code_1, country_code_2)%>%
  count(country_code_1, country_code_2) %>%
  filter(as.character(country_code_1) != as.character(country_code_2)) %>%
  rename(n_events_all = n)

######## get positive events ########  
# most frequent events
g_event_data %>%
  count(event_text) %>%
    arrange(-n) %>%
    slice(1:150) %>%
    as.data.frame() %>%
  kable()

# this list is somewhat adhoc
positive_frequent_events <- c("Make a visit","Make statement", "Consult", "Express intent to meet or negotiate", 
                              "Praise or Endorse", "Engage in negotiation", "Express intent to cooperate", "Engage in diplomatic cooperation", 
                              "Discuss by telephone", "Make optimistic comment")


g_positive_events <- g_event_data %>%
  filter(event_text %in% positive_frequent_events) %>%
  count(country_code_1, country_code_2) %>%
  filter(as.character(country_code_1) != as.character(country_code_2)) %>%
  rename(n_events_positive = n)

g_events <- full_join(g_all_events, g_positive_events) # bind together

# there is some asymettry a-> b != b->a. Take the average of these.

get_unique_relation_id <- function (x, y){
  pairs = c(x, y)
  ordered = order(pairs)
  paste0(pairs[ordered[1]], pairs[ordered[2]])
}

g_events_final <- g_events %>%
  rowwise() %>%
  mutate(allcodes = get_unique_relation_id(country_code_1, country_code_2))%>%
  group_by(allcodes)%>%
  summarize(n_events_positive = mean(n_events_positive, na.rm = T),
            n_events_all = mean(n_events_all, na.rm = T)) %>% 
  mutate(country_code_1 = unlist(lapply(allcodes, function(x) {substr(x, 1, 2)})),
         country_code_2 = unlist(lapply(allcodes, function(x) {substr(x, 3, 4)}))) %>%
  select(country_code_1, country_code_2, n_events_all, n_events_positive)

# normalize by mean population
pop <- read_csv("../../data/supplementary_data/cultural_sim_measures/events/ICEWS_event_data/factbook_pop_tableid_2119.csv")

g_pop <- pop %>%
  rename(country_name = Name,
         population = Value) %>%
  mutate(country_code = countrycode(country_name, 
                                    'country.name', 'iso2c')) %>%
  filter(country_code %in% g_countries$country_code) %>%
  select(country_code, population)

g_events_normal <- g_events_final %>%
  left_join(g_pop, by = c("country_code_1" = "country_code")) %>%
  rename(pop_1 = population) %>%
  left_join(g_pop, by =c("country_code_2" = "country_code")) %>%
  rename(pop_2 = population) %>%
  rowwise() %>%
  mutate(mean_pop = mean(c(pop_1, pop_2), na.rm = FALSE),
         normalized_n_events_positive = 1000*(n_events_positive/mean_pop),
         normalized_n_events_all = 1000* (n_events_all/mean_pop)) %>% #multiply because so small R writes as 0
  select(-pop_1, -pop_2, -mean_pop)

########## Trade Data ##############
trade <- read_csv("../../data/supplementary_data/cultural_sim_measures/events/trade_data/Dyadic_COW_4.0.csv")

trade_recoded <- trade %>%
  mutate(importer1 = fct_recode(importer1, "Vietnam" = "Republic of Vietnam"),
         importer2 = fct_recode(importer2, "Vietnam" = "Republic of Vietnam"),
         importer1 = fct_recode(importer1, "Yemen" = "Yemen Arab Republic"),
         importer2 = fct_recode(importer2, "Yemen" = "Yemen Arab Republic")) %>%
   mutate(country_code_1 = countrycode(importer1, 
                                      'country.name', 'iso2c'),
         country_code_2 = countrycode(importer2, 
                                      'country.name', 'iso2c')) 

g_trade <- trade_recoded %>%
  select(year, country_code_1, country_code_2, flow1, flow2) %>%
  filter(country_code_1 %in% g_countries$country_code) %>%
  filter(country_code_2 %in% g_countries$country_code) %>%
  rowwise() %>%
  mutate(flow = mean(c(flow1, flow2), na.rm = T)) %>% # mean of a->b and b->a
  group_by(country_code_1, country_code_2) %>%
  summarize(mean_imports_dollars = mean(flow, na.rm = T))

# normalize by GDP
gdp <- read_csv("../../data/supplementary_data/cultural_sim_measures/events/trade_data/factbook_GDP_tableid_2001.csv")
# (gapminder data missing lots of countiries)
g_clean <- gdp %>%
              rename(country_name = Name,
                     gdp_dollars = Value) %>%
              mutate(country_name = fct_recode(country_name, "South Korea" = "Korea")) %>%
              mutate(country_code = countrycode(country_name, 
                                      'country.name', 'iso2c')) %>%
              mutate(gdp_dollars = str_replace_all(gdp_dollars,"[^[:alnum:]]", ""),
                     gdp_dollars = as.numeric(gdp_dollars)) 
              
g_gdp <- g_clean %>%            
    filter(country_code %in% g_countries$country_code) %>%
    select(country_code, gdp_dollars)

g_trade_normal <- g_trade %>%
    left_join(g_gdp, by = c("country_code_1" = "country_code")) %>%
    rename(gdp_1 = gdp_dollars) %>%
    left_join(g_gdp, by =c("country_code_2" = "country_code")) %>%
    rename(gdp_2 = gdp_dollars) %>%
    rowwise() %>%
    mutate(mean_gdp = mean(c(gdp_1, gdp_2), na.rm = FALSE),
           normalized_mean_imports_dollars = 1000* (mean_imports_dollars/mean_gdp)) %>%
    select(-gdp_1, -gdp_2, -mean_gdp)

##### JOIN AND WRITE TO FILE #########
all_events <- full_join(g_events_normal, g_trade_normal)

write_csv(all_events, "../../data/supplementary_data/cultural_sim_measures/events/all_google_event_dists.csv")

