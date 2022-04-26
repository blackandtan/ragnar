# Purpose: Process and visualize Ragnar results

# set up workspace
  library(readxl)
  library(readr)
  library(lubridate)
  library(forcats)
  library(purrr)
  library(tidyverse)
  library(plotly)

# import data
  raw2022 <- read_csv("./data/Ragnar2022.csv") 
  glimpse(raw2022)

  # tidy data and calculate some additional variables
  r22 <- raw2022 %>% 
    mutate(laptime = as.duration(Time),
           Loopcolor = factor(Loopcolor, c("G", "Y", "R"))) %>% 
    group_by(Runner) %>% 
    mutate(Runnertotal = sum(laptime, na.rm = T),
           Runnertotallbl = seconds_to_period(Runnertotal)) %>%  # label for graphics
    ungroup() %>% 
    mutate(Runner = reorder(Runner, -Runnertotal),  # sort runner var by total time
           Runnertime = paste0(Runner, " (", Runnertotallbl, ")"), 
           Runnertime = fct_reorder(Runnertime, -Runnertotal),
           laplbl = as.character(Time),
           pace2 = duration(laptime/Length) # check pace calc
           )
  glimpse(r22)

  r22 %>% filter(Runner == "Barb") # check times against my watch
  
  # total time for each team
  r22 %>% group_by(Team) %>% 
    summarise(across(Time, sum, na.rm = T) ) %>% 
    mutate(time2 = seconds_to_period(Time))
    
### Visualize

  # time per loop
  ggplot(r22, aes(Time, Runnertime, color = Loopcolor, fill = Loopcolor,label = laplbl)) +
    geom_point(shape = 21, size = 3, stroke = 0.65) +
    geom_text(size = 2, nudge_y = -.35, nudge_x = 0.35, show.legend = FALSE) +
    scale_colour_manual("Loop", values = c("green4",  "orange4", "red4")) +
    scale_fill_manual("Loop", values = c("lightgreen",  "orange", "red")) +
    labs(title = "Atlanta Ragnar Relay 2022",
         subtitle = "16.2 miles total (Green 4.3, Yellow 5.7, Red 6.2)",
         caption = "*Yellow loop adjusted to 5.7 miles; Kyla's green loop adjusted for delayed handoff",
         x = "Time",
         y = "Runner") +
    theme_minimal() +
    theme(legend.position ="none")

  # pace by loop 
  ggplot(r22, aes(pace, Runnertime, color = Loopcolor, fill = Loopcolor, label = pace)) +
    geom_point(shape = 21, size = 3, stroke = 0.65) +
    geom_text(size = 2, nudge_y = -.35, nudge_x = 0.35, show.legend = FALSE) +
    scale_colour_manual("Loop", values = c("green4",  "orange4", "red4")) +
    scale_fill_manual("Loop", values = c("lightgreen",  "orange", "red")) +
    labs(title = "Atlanta Ragnar Relay 2022",
         subtitle = "16.2 miles total (Green 4.3, Yellow 5.7, Red 6.2)",
         caption = "*Yellow loop adjusted to 5.7 miles; Kyla's green loop adjusted for delayed handoff",
         x = "Pace, in minutes per mile",
         y = "Runner") +
    theme_minimal() +
    theme(legend.position="none")

