### use ICEWS_event_data and trade data to look at pairwise  relations####
library(tidyverse)
library(forcats)
library(knitr)


# get google countries
g_countries <- read_csv("../../data/supplementary_data/cultural_sim_measures/geo/all_google_countries.csv")
s
########## EVENT DATA ##############
# read in event data
event_data <- read_tsv("../../data/supplementary_data/cultural_sim_measures/events/ICEWS_event_data/events.2015.20170206133646.tab") 
  
g_event_data <- event_data %>%
          mutate(`Source Country` = fct_recode(`Source Country`, "China, Republic of (Taiwan)" = "Taiwan" ,
                                                          "Russia" = "Russian Federation",
                                                           "Korea, South" = "South Korea"),
                  `Target Country` = fct_recode(`Target Country`, "China, Republic of (Taiwan)" = "Taiwan" ,
                                       "Russia" = "Russian Federation",
                                       "Korea, South" = "South Korea")) %>%
            filter(`Source Country` %in% g_countries$country) %>%
            filter(`Target Country` %in% g_countries$country)

g_event <- g_event_data %>%
  select(`Event ID`, `Event Text`, `Source Country`, `Target Country`)%>%
  count(`Source Country`, `Target Country`) %>%
  filter(as.character(`Source Country`) != as.character(`Target Country`))

g_all_events <- g_event_data %>%
  select(`Event ID`, `Event Text`, `Source Country`, `Target Country`)%>%
  count(`Source Country`, `Target Country`) %>%
  filter(as.character(`Source Country`) != as.character(`Target Country`)) %>%
  rename(n_events_all = n)

# most frequent events
g_event_data %>%
  count(`Event Text`) %>%
    arrange(-n) %>%
    slice(1:150) %>%
    as.data.frame() %>%
  kable()

# this list is somewhat adhoc
positive_frequent_events <- c("Make a visit","Make statement", "Consult", "Express intent to meet or negotiate", 
                              "Praise or Endorse", "Engage in negotiation", "Express intent to cooperate", "Engage in diplomatic cooperation", 
                              "Discuss by telephone", "Make optimistic comment")


g_positive_events <- g_event_data %>%
      select(`Event ID`, `Event Text`, `Source Country`, `Target Country`)%>%
  filter(`Event Text` %in% positive_frequent_events) %>%
  count(`Source Country`, `Target Country`) %>%
  filter(as.character(`Source Country`) != as.character(`Target Country`)) %>%
  rename(n_events_positive = n)

events <- full_join(g_all_events, g_positive_events) %>%
  rename(country1 = `Source Country`,
         country2 = `Target Country`) %>%
  left_join(g_countries %>% select(country, country_code), by = c("country1"= "country")) %>%
  rename(country_code1 = country_code) %>%
  left_join(g_countries %>% select(country, country_code), by = c("country2"= "country")) %>%
  rename(country_code2 = country_code) %>%
  select(country_code1, country_code2, n_events_all, n_events_positive)


# there is some asymettry a-> b != b->a. Take the average of these

get_unique_relation_id <- function (x, y){
  pairs = c(x, y)
  ordered = order(pairs)
  paste0(pairs[ordered[1]], pairs[ordered[2]])
}
g_events <- events %>%
  rowwise() %>%
  mutate(allcodes = get_unique_relation_id(country_code1, country_code2))%>%
  group_by(allcodes)%>%
  summarize(n_events_positive = mean(n_events_positive, na.rm = T),
            n_events_all = mean(n_events_all, na.rm = T)) %>% 
  mutate(country_code1 = unlist(lapply(allcodes, function(x) {substr(x, 1, 2)})),
         country_code2 = unlist(lapply(allcodes, function(x) {substr(x, 3, 4)}))) %>%
  select(country_code1, country_code2, n_events_all, n_events_positive)



########## Trade Data ##############
trade <- read_csv("../../data/supplementary_data/cultural_sim_measures/events/trade_data/Dyadic_COW_4.0.csv")

trade_recoded <- trade %>%
                 mutate(importer1 = fct_recode(importer1, "China, Republic of (Taiwan)" = "Taiwan" ,
                                                                "United States" = "United States of America",
                                                                "Korea, South" = "South Korea" ,
                                                                "Serbia" = "Montenegro"),
                        importer2 = fct_recode(importer2, "China, Republic of (Taiwan)" = "Taiwan" ,
                                                               "United States" = "United States of America",
                                                               "Korea, South" = "South Korea" ,
                                                               "Serbia" = "Montenegro")) %>%
                      left_join(g_countries, by = c("importer1" = "country")) %>%
                      rename(country_code1 = country_code)  %>%
                      left_join(g_countries, by = c("importer2" = "country")) %>%
                      rename(country_code2 = country_code) 

g_trade <- trade_recoded %>%
  select(year, country_code1, country_code2, flow1, flow2) %>%
  filter(country_code1 %in% g_countries$country_code) %>%
  filter(country_code2 %in% g_countries$country_code) %>%
  rowwise() %>%
  mutate(flow = mean(c(flow1, flow2), na.rm = T)) %>%
  group_by(country_code1, country_code2) %>%
  summarize(mean_imports_dollars = mean(flow, na.rm = T)) %>%
  filter(country_code1 != country_code2)


##### WRITE TO FILE #########
all_events = full_join(g_events, g_trade)

write_csv(all_events, "../../data/supplementary_data/cultural_sim_measures/events/all_google_event_dists.csv")

