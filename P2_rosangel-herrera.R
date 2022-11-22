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
#agregar año y stage de final y si ganaron o no contra quien

#-----------------------------------------------------------------------------#

#ranking FIFA de como inician al mundial las 32 selecciones

r_fifa22 <- X2022_world_cup_groups
r_fifa22 <- r_fifa22[with(r_fifa22, order(r_fifa22$`Group`)), ]

#-----------------------------------------------------------------------------#

#filtro cantidad de partidos ganados por seleccion e instancias que llegaron
match_w_columns <- c("Team", "Wins")
win_f22 <- world_cup_matches

home_win <- win_f22 %>% 
  group_by(`Home Team`) %>%
  summarise(`Home Goals` = sum(`Home Goals` > `Away Goals`), )
colnames(home_win) <- match_w_columns

away_win <- win_f22 %>% 
  group_by(`Away Team`) %>%
  summarise(`Away Goals` = sum(`Away Goals` > `Home Goals`), )
colnames(away_win) <- match_w_columns

total_matchs <- merge(home_win, away_win, by = "Team", all = TRUE)
total_matchs$Wins <- total_matchs$Wins.x + total_matchs$Wins.y
total_matchs <- total_matchs[with(total_matchs, order(-Wins)), ]

#filtro cantidad de partidos empatados por seleccion e instancias que llegaron
match_t_columns <- c("Team", "Ties")

tie_match <- win_f22 %>% 
  group_by(`Home Team`) %>%
  summarise(`Home Goals` = sum(`Home Goals` == `Away Goals`), )
colnames(tie_match) <- c("Team", "Ties")

tie_match1 <- win_f22 %>% 
  group_by(`Away Team`) %>%
  summarise(`Away Goals` = sum(`Away Goals` == `Home Goals`), )
colnames(tie_match1) <- c("Team", "Ties")

total_matchs1 <- merge(tie_match, tie_match1, by = "Team", all = TRUE)
total_matchs1$Ties <- total_matchs1$Ties.x + total_matchs1$Ties.y
total_matchs1 <- total_matchs1[with(total_matchs1, order(-Ties)), ]


#filtro cantidad de partidos perdidos por seleccion e instancias que llegaron
match_l_columns <- c("Team", "Loss")

home_loss <- win_f22 %>% 
  group_by(`Home Team`) %>%
  summarise(`Home Goals` = sum(`Home Goals` < `Away Goals`), )
colnames(home_loss) <- match_l_columns

away_loss <- win_f22 %>% 
  group_by(`Away Team`) %>%
  summarise(`Away Goals` = sum(`Away Goals` < `Home Goals`), )
colnames(away_loss) <- match_l_columns

total_matchs2 <- merge(home_loss, away_loss, by = "Team", all = TRUE)
total_matchs2$Loss <- total_matchs2$Loss.x + total_matchs2$Loss.y
total_matchs2 <- total_matchs2[with(total_matchs2, order(-Loss)), ]

#Combinacion de tablas
all_matchs <- left_join(total_matchs, total_matchs1, by = "Team") %>%
  left_join(total_matchs2, by = "Team")

#Resultados de historicos de juegos en la historia del FIFA World Cup
all_WC <- select(all_matchs, Team, Wins, Ties, Loss)

#Resultados de historicos de juegos en la historia de los 32 equipos participantes 
#en Qatar 2022

q_2022 <- all_WC %>% 
  filter(`Team` %in% c("Qatar", "Germany", "Denmark", "Brazil", "France",
                       "Belgium", "Croatia", "Spain", "Serbia", "England", "Switzerland", "Netherlands", 
                       "Argentina", "Iran", "South Korea", "Japan", "Saudi Arabia", "Ecuador",
                       "Uruguay", "Canada","Ghana", "Senegal", "Portugal", "Poland", "Tunisia", 
                       "Morocco", "Cameroon","Mexico", "United States", "Wales", "Australia", 
                       "Costa Rica")) %>%
  select(Team, Wins, Ties, Loss)

#Anexion de la seleccion de Qatar dado a su primer torneo es en este torneo

q_2022 <- q_2022 %>%
  rows_upsert(data.frame(Team = "Qatar", Wins = 0, Ties = 0, Loss = 0))


#----------- Parte II Visualizacion de los datos "Data Cleaning" -------------#

#ranking FIFA de como inician al mundial las 32 selecciones

ggplot(r_fifa22) +
  geom_point(aes(x = `Group`, y = `FIFA Ranking`, col = `Team`), size = (5)) +
  theme_bw() + 
  scale_y_continuous(breaks = seq(1, 65, 5)) +
  geom_text(aes(colour = factor(`Team`)),
            label = r_fifa22$Team,
            x = r_fifa22$Group, y = r_fifa22$`FIFA Ranking`,
            hjust = -0.25, size = 5,
            inherit.aes = TRUE
  ) 
labs(title = "Ranking FIFA de las selecciones que disputaran en",
     subtitle = "Qatar 2022",
     caption = "Data de la base de datos otorgado por xxx",
     tag = "Figura 1 prueba",
     x = "Seleccion",
     y = "Posicion",
     colour = "Grupo"
)

#-----------------------------------------------------------------------------#
