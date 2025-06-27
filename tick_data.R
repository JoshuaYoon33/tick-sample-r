library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)


tick_data <- read_excel("/Users/joshuayoon/Downloads/AllTBD2022_Public.xlsx")
View(tick_data)


#identify states with the highest total disease cases?

states <- tick_data %>% 
  arrange(desc(State)) %>% 
  pivot_longer(-c(State, County), names_to = "disease", values_to = "frequency")

View(states)

state_count <- states %>% 
  group_by(State) %>%
  summarise(disease_count = sum(frequency)) %>%
  arrange(desc(disease_count))

ggplot(state_count, aes(x = State, y = disease_count)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

View(state_count)

#Diseases reported in the most counties

county_count <- states %>% 
  group_by(County) %>%
  summarise(county_disease = sum(frequency)) %>%
  arrange(desc(county_disease))

View(county_count)

#States with the widest variety of diseases

state_var <- states %>%
  filter(frequency > 0) %>%
  group_by(State) %>%
  summarise(disease_variety = n_distinct(disease)) %>%
  arrange(desc(disease_variety))


View(state_var)

#States and counties with zero reported cases


zero_state <- states %>%
  group_by(State) %>%
  summarise(zero_d = sum(frequency)) %>%
  filter(zero_d == 0) %>%
  summarise(no_cases_state = n_distinct(State))

zero_county <- states %>%
  group_by(County) %>%
  summarise(zero_ds = sum(frequency)) %>%
  filter(zero_ds == 0) %>%
  summarise(no_cases_county = n_distinct(County))


View(zero_state)
View(zero_county)

# Top 5 states with the highest overall case counts
top_states <- states %>% 
  group_by(State) %>%
  summarise(case_count = sum(frequency)) %>%
  arrange(desc(case_count)) %>%
  slice_head(n = 5)

View(top_states)

#Top county within each of those states
top_county <- states %>% 
  filter(State %in% top_states$State) %>% #gives the 5 states in top_states, as in top_states it is groupted by State
  group_by(State, County) %>%
  summarise(most_d_county = sum(frequency))%>%
  arrange(desc(most_d_county)) %>% #this does give the same states as top_states just in different order
  slice_head(n = 1)

View(top_county)





