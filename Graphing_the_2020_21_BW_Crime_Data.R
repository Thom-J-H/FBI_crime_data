library(tidyverse)
library(here)



## DATA SOURCE: https://cde.ucr.cjis.gov/LATEST/webapp/#


load(here::here("data", "tidy_data", "crime_dat_bw_2021_2022.rda") )



# Start here --------------------------------------------------------------





## Standard Caption
std_cap <- "CC0 (Public Domain)"


all_2021_crime <- crime_dat_bw_2021_2022 %>%
  ggplot( aes(x = Year, y = Crime_Per, fill = Race)) +
  facet_wrap( ~ Type, scales = "free") +
  geom_col(position = "dodge") +
  scale_fill_manual( values = c( "#333333", "#6495ED")) +
  scale_x_continuous(breaks = c(2020, 2021)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_minimal() +
  labs(y = "Arrest Percentage",
   title = "Adult & Juvenile Crime: Percentage of Arrests by Race for All Crimes, Assault, and Homicide" ,
   subtitle = "Source: FBI Crime Data Explorer",
   caption = std_cap)



all_2021_crime




all_2021_crime_scaled <-  crime_dat_bw_2021_2022 %>%
  ggplot( aes(x = Year, y = Pop_Scale, fill = Race)) +
  facet_wrap( ~ Type, scales = "free") +
  geom_col(position = "dodge") +
  scale_fill_manual( values = c( "#333333", "#6495ED")) +
  scale_x_continuous(breaks = c(2020, 2021)) +
  theme_minimal() +
  labs( y = "Ratio of Arrest % to Population %",
        title = "Adult & Juvenile Crime: Ratio of Arrest % to Population % for All Crimes, Assault, and Homicide",
        subtitle = "Sources: FBI Crime Data Explorer & Census Data",
        caption = std_cap)


all_2021_crime_scaled 



plots_2021 <- c("all_2021_crime", "all_2021_crime_scaled")

save(list = plots_2021, 
     file = here::here("data" , "tidy_data",  "plots_2021.rda"))