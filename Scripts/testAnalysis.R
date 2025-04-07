#'''''''''''''''''''''''''''''''''
#'
#' Test Survey Data Analysis
#' @date 2025-04-07
#' @author Cooper Kimball-Rhines
#'''''''''''''''''''''''''''''''''

# Load packages
library(igraph)
library(ggraph)
library(tidyverse)


# Load, subset, and mutate network data
netTest <- read_csv("data/test.csv") |>
  select("What organization do you work for?",
         starts_with("How often do you communicate")) |>
  rename(employer = `What organization do you work for?`) |>
  pivot_longer(names_prefix = "How often do you communicate",
               names_to = "org",
               values_to = "score",
               cols = -employer) |>
  # Translate answers into likert
  mutate(org = str_split_i(org, " - ", i = 2),
         score = str_replace(score, "Never", "0"),
         score = str_replace(score, "Once per year", "1"),
         score = str_replace(score, "3-5 times per year", "2"),
         score = str_replace(score, "Once per month", "3"),
         score = str_replace(score, "Weekly", "4"),
         score = as.numeric(score)) |>
  # Employer type categorization
  mutate(ecat = employer,
         ecat = str_replace(ecat, "Pine Bush", "Nonprofit"),
         ecat = str_replace(ecat, "NHFG", "State Government"),
         ecat = str_replace(ecat, "NOAA", "Federal Government"),
         ecat = str_replace(ecat, "Nature Conservancy", "Nonprofit"),
         ecat = as.factor(ecat)) |>
  # Filter out non-communication
  filter(score > min(score))

# Construct weighted network graph
weighNet <- graph_from_data_frame(netTest, directed = TRUE)

ggraph(weighNet, layout = "gem") + # Basic unweighted graph based on answers
  geom_edge_fan(arrow = arrow(type = "closed", length = unit(3, "mm")),
                aes(color = score),
                end_cap = circle(3, 'mm')) +
  geom_node_point(size = 5) +
  geom_edge_loop(aes(color = score)) +
  theme_void()



