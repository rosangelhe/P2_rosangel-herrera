library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(ggthemes)

#Carga de la base de datos en formato .CSV
international_matches <- read_csv("World+Cup/international_matches.csv")
X2022_world_cup_groups <- read_csv("World+Cup/2022_world_cup_groups.csv")
world_cup_matches <- read_csv("World+Cup/world_cup_matches.csv")

#----------- Parte I Limpieza de los datos "Data Cleaning" -------------#

# Filtro de datos "top cinco (5) de equipos mas goleadores en el torneo FIFA World Cup 
# desde el primer torneo en Uruguay 1930"

# Separacion de la tabla por equipo segun los goles obtenidos en el torneo
wcup_goal <- world_cup_matches[c("Home Team", "Home Goals","Away Goals", "Away Team")]

#Creacion de una nueva columna en el data frame
team_column <- c("Team", "Goals")

#filtro y asignacion de goles obtenidos en condicion de Home dentro de team_column
h_cup <- wcup_goal %>% 
  group_by(`Home Team`) %>%
  summarise(`Home Goals` = sum(`Home Goals`),
            )
colnames(h_cup) <- team_column

#filtro y asignacion de goles obtenidos en condicion de Away dentro de team_column
a_cup <- wcup_goal %>%
  group_by(`Away Team`) %>%
  summarise(`Away Goals` = sum(`Away Goals`),
  )
colnames(a_cup) <- team_column

#Union y reodenamiento de las tablas de goles obtenidos en un orden descendente 
total_top_5 <- merge(h_cup, a_cup, by = "Team", all = TRUE)
total_top_5$total <- total_top_5$Goals.x + total_top_5$Goals.y
total_top_5 <- total_top_5[with(total_top_5, order(-total)), ]

#-----------------------------------------------------------------------------#

#Filtro de datos "Historicos de goles anotados en todas sus participaciones en
#el torneo dentro de los cinco (5) equipos favoritos para participar a 
#la final de Qatar 2022 segun la casa de apuesta betfair"

top_fav <- total_top_5 %>% 
  filter(`Team` %in% c("Brazil", "Argentina", "France", "Spain", "England")) %>%
  select(Team, Goals.x, Goals.y, total)

#-----------------------------------------------------------------------------#

