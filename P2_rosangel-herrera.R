library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(ggthemes)

#Carga de la base de datos en formato .CSV
international_matches <- read_csv("World+Cup/international_matches.csv")
X2022_world_cup_groups <- read_csv("World+Cup/2022_world_cup_groups.csv")