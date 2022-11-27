library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(ggthemes)
library(gganimate)

#Carga de la base de datos en formato .CSV
X2022_world_cup_groups <- read_csv("World+Cup/2022_world_cup_groups.csv")
world_cup_matches <- read_csv("World+Cup/world_cup_matches.csv")


#----------- Parte I Limpieza de los datos "Data Cleaning" -------------#

# 1 Filtro de datos "top cinco (5) de equipos mas goleadores en el torneo FIFA World Cup 
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

# 2 Filtro de datos "Promedio de goles anotados en todas sus participaciones en
#el torneo dentro de los cinco (5) equipos favoritos para participar a 
#la final de Qatar 2022 segun la casa de apuesta betfair"

top_fav <- total_top_5 %>% 
  filter(`Team` %in% c("Brazil", "Argentina", "France", "Spain", "England")) %>%
  select(Team, Goals.x, Goals.y, total)

#Calculo del promedio de goles
top_fav$Promedio <- apply(top_fav[ ,c(2,3)], 1, mean, na.rm = TRUE)

#agregar año y stage de final y si ganaron o no contra quien

#-----------------------------------------------------------------------------#

# 3 ranking FIFA de como inician al mundial las 32 selecciones

r_fifa22 <- X2022_world_cup_groups
r_fifa22 <- r_fifa22[with(r_fifa22, order(r_fifa22$`Group`)), ]

#-----------------------------------------------------------------------------#
# 4 Resultados de historicos de juegos en la historia del FIFA World Cup
#Funcion comun para limpiar la data
merge_matches <- function(df, f, cols) {
  temp_cols <- c("Team", "Matches")
  temp <- df
  
  temp1 <- temp %>% 
    group_by(`Home Team`) %>%
    summarise(`Home Goals` = sum(f(`Home Goals`, `Away Goals`)), )
  colnames(temp1) <- temp_cols
  
  temp2 <- temp %>% 
    group_by(`Away Team`) %>%
    summarise(`Away Goals` = sum(f(`Away Goals`, `Home Goals`)), )
  colnames(temp2) <- temp_cols
  
  total <- merge(temp1, temp2, by = "Team", all = TRUE)
  total$Matches <- total$Matches.x + total$Matches.y
  total <- total[with(total, order(-Matches)), ]
  total <- select(total, Team, Matches)
  colnames(total) <- cols
  total
}

#  filtro cantidad de partidos ganados por seleccion e instancias que llegaron
total_wins <- merge_matches(world_cup_matches, `>`, c("Team", "Wins"))

#filtro cantidad de partidos empatados por seleccion e instancias que llegaron
total_ties <- merge_matches(world_cup_matches, `==`, c("Team", "Ties"))

#filtro cantidad de partidos perdidos por seleccion e instancias que llegaron
total_loss <- merge_matches(world_cup_matches, `==`, c("Team", "Loss"))

#Combinacion de tablas
all_matchs <- left_join(total_wins, total_ties, by = "Team") %>%
  left_join(total_loss, by = "Team")

#Resultados de historicos de juegos en la historia del FIFA World Cup
all_WC <- select(all_matchs, Team, Wins, Ties, Loss)

#----------------------------------------------------------------------------------------

# 5 Resultados de historicos de juegos en la historia de los 32 equipos participantes 
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

# 1 GRAFICA top cinco (5) de equipos mas goleadores en el torneo FIFA World Cup 
# desde el primer torneo en Uruguay 1930"


grafica <- total_top_5 %>%
  filter(`Team` %in% c("Brazil", "Argentina", "France", "Spain", "England")) %>%
  select(Team, Goals.x, Goals.y, total)


ggplot(grafica, aes(x = reorder(Team, -total), y = total)) +
  geom_segment(aes(x = reorder(Team, -total),
                   xend = reorder(Team, -total),
                   y = 0, yend = total),
               color = "gray", lwd = 1) +
  geom_point(size = 4, pch = 21, bg = 4, col = 1) +
  xlab("Team") +
  ylab("Goles") +
  coord_flip() +
  theme_minimal() +
  transition_states(total, wrap = FALSE) +
  shadow_mark() +
  enter_grow()
#----------------------------------------------------------------------------------------
# 2 promedio de goles anotados en todas sus participaciones en
#el torneo dentro de los cinco (5) equipos favoritos para participar a 
#la final de Qatar 2022 segun la casa de apuesta betfair"

ggplot(top_fav,aes(x = Team, y = Promedio, fill = Promedio)) +
  geom_col() +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "white"),
    panel.ontop = TRUE
  ) +
  transition_states(Team, wrap = FALSE) +
  shadow_mark() +
  enter_grow() +
  enter_fade()
#------------------- otra version

top_fav %>% 
  ggplot(aes(x = `Team`, y = Promedio, fill = `Team`)) +
  geom_col() +
  scale_fill_manual(values = c("cadetblue1", "seagreen3", "seashell1", "steelblue", "tomato2"), name = "Top 5") +
  scale_y_continuous(breaks = seq(10, 300, 25)) +
  labs(caption = "Fuente: https://www.betfair.com/sport/football") +
  ggtitle("Goles anotados en todas sus participaciones en el torneo del Top 5 de los
          equipos favoritos en ganar la World Cup en Qatar 2022",
          subtitle = "Segun los datos otorgados el 23 de Noviembre") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(colour = "azure4", linetype = 2), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 28, colour = "black", face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 15, colour = "black"),
        plot.title.position = "plot", plot.caption.position = "plot",
        axis.title.x = element_text(colour = "black", size = 16), axis.title.y = element_blank(), axis.text = element_text(colour = "black", size = 14),
        plot.caption = element_text(size = 12, colour = "black")) +
  transition_states(Team, wrap = FALSE) +
  shadow_mark() +
  enter_grow() +
  enter_fade()


#----------------------------------------------------------------------------------------
# 3 ranking FIFA de como inician al mundial las 32 selecciones

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
