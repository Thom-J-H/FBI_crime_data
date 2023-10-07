
# Plots for Total Crime and Juvenile Crime --------------------------------

library(tidyverse)
library(here)



# Data Sets ---------------------------------------------------------------

load(here::here("data", "tidy_data", "Total_Crime_1995_2019_bw.rda") )
load(here::here("data" , "tidy_data",  "Juvenile_Crime_1995_2019_bw.rda"))





## Standard Caption
std_cap <- "CC0 (Public Domain)"

crime_dat_source <- "Source: FBI Uniform Crime Report for 1995-2019"

crime_pop_source <- "Sources: FBI Uniform Crime Report for 1995-2019 & Census Data"



pop_stats <- Total_Crime_1995_2019_bw %>%
  select(Year, Race, Pop_Per) %>% 
  distinct() 


# Population Graph -- FBI Race --------------------------------------------

pop_graph <- Total_Crime_1995_2019_bw %>%
  select(Year, Race, Pop_Per) %>% 
  distinct() %>%
  ggplot( aes(x = Year, y = Pop_Per, color = Race) ) +
  geom_line() +
  geom_point() +
  scale_color_manual( values = c( "#333333", "#6495ED")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) +
  theme_minimal() +
  labs(title = "Population Percentages using the FBI Catagories for Race: Black & White",
       y = "Population Percentage",
       subtitle = crime_pop_source ,
       caption = std_cap)


pop_graph


# All Ages All Crimes -----------------------------------------------------

all_ages_all_crimes <- Total_Crime_1995_2019_bw %>%
  filter(Type == "All") %>%
  ggplot( aes(x = Year, y = Crime_Per, fill = Race) ) +
  geom_col(position = "dodge") +
  scale_fill_manual( values = c( "#333333", "#6495ED")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) +
  theme_minimal() +
  labs(y = "Arrest Percentage",
       title = "Adult & Juvenile Crime: Percentage of Total Arrests by Race" ,
       subtitle = crime_dat_source ,
       caption = std_cap)



all_ages_all_crimes 


## Scaled

all_ages_all_scaled <- Total_Crime_1995_2019_bw %>%
  filter(Type == "All") %>%
  ggplot( aes(x = Year, y = Pop_Scale, fill = Race) ) +
  geom_col(position = "dodge") +
  scale_fill_manual( values = c( "#333333", "#6495ED")) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) +
  theme_minimal() +
  geom_hline( yintercept = 1, color = "red" , lty = 2) +
  labs( y = "Ratio of Arrest % to Population %",
        title = "Adult & Juvenile Crime: Ratio of Arrest % (All Crimes) to Population %",
        subtitle = crime_pop_source ,
        caption = std_cap)



all_ages_all_scaled




# Homicide ----------------------------------------------------------------


all_ages_homicide <- Total_Crime_1995_2019_bw %>%
  filter(Type == "Murder and nonnegligent manslaughter") %>%
  ggplot( aes(x = Year, y = Crime_Per, fill = Race) ) +
  geom_col(position = "dodge") +
  scale_fill_manual( values = c( "#333333", "#6495ED")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")  ,
                     breaks = seq(0, 60, by = 10 ))+
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) +
  theme_minimal() +
  labs(y = "Arrest Percentage",
       title = "Adult & Juvenile Crime: Percentage of Homicide Arrests by Race" ,
       subtitle =  crime_dat_source ,
       caption = std_cap)



all_ages_homicide


##


all_ages_homicide_scaled <- Total_Crime_1995_2019_bw %>%
  filter(Type == "Murder and nonnegligent manslaughter") %>%
  ggplot( aes(x = Year, y = Pop_Scale, fill = Race) ) +
  geom_col(position = "dodge") +
  scale_fill_manual( values = c( "#333333", "#6495ED")) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) +
  theme_minimal() +
  geom_hline( yintercept = 1, color = "red" , lty = 2) +
  labs( y = "Ratio of Arrest % to Population %",
        title = "Adult & Juvenile Crime: Ratio of Homicide Arrest % to Population %",
        subtitle =  crime_pop_source ,
        caption = std_cap)



all_ages_homicide_scaled 



# Raw proportion ----------------------------------------------------------

white_pop <- Total_Crime_1995_2019_bw %>% 
  filter(Race == "White")

black_pop <- Total_Crime_1995_2019_bw %>% 
  filter(Race == "Black")

black_pop <- black_pop %>%
  mutate(gap_arrest = Pop_Scale / white_pop$Pop_Scale)
 
black_pop %>% filter(Type == "All") %>%
  summarize(mean(gap_arrest), sd(gap_arrest))


black_pop %>% filter(Type != "All") %>%
  summarize(mean(gap_arrest), sd(gap_arrest))




# Juvenile Crime ----------------------------------------------------------

juv_all <- Juvenile_Crime_1995_2019_bw %>%
  filter(Type == "All") %>%
  ggplot( aes(x = Year, y = Crime_Per, fill = Race) ) +
  geom_col(position = "dodge") +
  scale_fill_manual( values = c( "#333333", "#6495ED")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) +
  theme_minimal() +
  labs(y = "Arrest Percentage",
       title = "Juvenile (under 18): Percentage of Total Arrests (All Crimes) by Race" ,
       subtitle =  crime_dat_source ,
       caption = std_cap)



juv_all



##


juv_all_scaled <- Juvenile_Crime_1995_2019_bw %>%
  filter(Type == "All") %>%
  ggplot( aes(x = Year, y = Pop_Scale, fill = Race) ) +
  geom_col(position = "dodge") +
  scale_fill_manual( values = c( "#333333", "#6495ED")) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) +
  theme_minimal() +
  geom_hline( yintercept = 1, color = "red" , lty = 2) +
  labs( y = "Ratio of Arrest % to Population %",
        title = "Juvenile (under 18): Ratio of All Crimes Arrest % to Population %",
        subtitle =  crime_pop_source ,
        caption = std_cap)



juv_all_scaled 


# Juvenile_ Murder --------------------------------------------------------


juv_murder <- Juvenile_Crime_1995_2019_bw %>%
  filter(Type == "Murder and nonnegligent manslaughter") %>%
  ggplot( aes(x = Year, y = Crime_Per, fill = Race) ) +
  geom_col(position = "dodge") +
  scale_fill_manual( values = c( "#333333", "#6495ED")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) +
  scale_color_manual( values = c( "#333333", "#6495ED")) +
  theme_minimal() +
  labs(y = "Arrest Percentage",
       title = "Juvenile (under 18): Percentage of Homicide Arrests by Race" ,
       subtitle =  crime_dat_source ,
       caption = std_cap)


juv_murder


#
#


juv_murder_scaled <- Juvenile_Crime_1995_2019_bw  %>%
  filter(Type == "Murder and nonnegligent manslaughter") %>%
  ggplot( aes(x = Year, y = Pop_Scale, fill = Race) ) +
  geom_col(position = "dodge") +
  scale_fill_manual( values = c( "#333333", "#6495ED")) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) +
  theme_minimal() +
  geom_hline( yintercept = 1, color = "red" , lty = 2) +
  labs(y = "Ratio of Arrest % to Population %",
       title = "Juvenile (under 18): Ratio of Homicide Arrest % to Population %",
        subtitle =  crime_pop_source ,
       caption = std_cap)


juv_murder_scaled 


# Gap ---------------------------------------------------------------------


white_juv_pop <- Juvenile_Crime_1995_2019_bw%>% 
  filter(Race == "White")

black_juv_pop <- Juvenile_Crime_1995_2019_bw %>% 
  filter(Race == "Black")

black_juv_pop  <- black_juv_pop  %>%
  mutate(gap_arrest = Pop_Scale / white_juv_pop$Pop_Scale)

black_juv_pop  %>% filter(Type == "All") %>%
  summarize(mean(gap_arrest), sd(gap_arrest))


black_juv_pop  %>% filter(Type != "All") %>%
  summarize(mean(gap_arrest), sd(gap_arrest))




# Save All Plots ----------------------------------------------------------


all_plots_FBI_UCR <- c("pop_graph", "all_ages_all_crimes",
                       "all_ages_all_scaled","all_ages_homicide",
                       "all_ages_homicide_scaled" , "juv_all",
                       "juv_all_scaled", "juv_murder", 
                       "juv_murder_scaled" )



save(list = all_plots_FBI_UCR, 
     file = here::here("data", "tidy_data", 'all_plots_FBI_UCR.rda' ) )

