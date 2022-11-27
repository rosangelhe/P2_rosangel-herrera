library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(ggthemes)
library(gganimate)
library(reactablefmtr)

#Carga de la base de datos en formato .CSV
world_cup_matches <- read_csv("World+Cup/world_cup_matches.csv")
X2022_world_cup_groups <- read_csv("World+Cup/2022_world_cup_groups.csv")

head(world_cup_matches)
head(X2022_world_cup_groups)

#----------- Parte I Limpieza de los datos "Data Cleaning" -------------#

# 1 Filtro de datos "Total de goles en todas las selecciones en el torneo FIFA World Cup 
# desde el primer torneo oficial en Uruguay 1930"

# Separacion de la tabla por equipo segun los goles obtenidos en el torneo
wcup_goal <- world_cup_matches[c("Home Team", "Home Goals","Away Goals", "Away Team")]

#Creacion de una nueva columna en el data frame
team_column <- c("Team", "Goals")

#filtro y asignacion de goles obtenidos en condicion de Home dentro de team_column
h_cup <- wcup_goal %>% 
  group_by(`Home Team`) %>%
  summarise(`Home Goals` = sum(`Home Goals`),
            )
colnames(h_cup) <- c("Team", "Home")

#filtro y asignacion de goles obtenidos en condicion de Away dentro de team_column
a_cup <- wcup_goal %>%
  group_by(`Away Team`) %>%
  summarise(`Away Goals` = sum(`Away Goals`),
  )
colnames(a_cup) <- c("Team", "Away")

#Union y reodenamiento de las tablas de goles obtenidos en un orden descendente 
total_gol <- merge(h_cup, a_cup, by = "Team", all = TRUE)
total_gol$total <- total_gol$Home + total_gol$Away
total_gol <- total_gol[with(total_gol, order(-total)), ]

#-----------------------------------------------------------------------------#

# 2 Top 05 selecciones más goleadoras

total_top_5 <- total_gol %>%
  filter(`Team` %in% c("Brazil", "Argentina", "France", "Spain", "England")) %>%
  select(Team, Home, Away, total)

#-----------------------------------------------------------------------------#

# 3 Filtro de datos "Promedio de goles anotados en todas sus participaciones en
#el torneo dentro de los cinco (5) equipos favoritos para participar a 
#la final de Qatar 2022 segun la casa de apuesta betfair"

top_fav <- total_gol %>% 
  filter(`Team` %in% c("Brazil", "Argentina", "France", "Spain", "England")) %>%
  select(Team, Home, Away, total)

#Calculo del promedio de goles
top_fav$Promedio <- apply(top_fav[ ,c(2,3)], 1, mean, na.rm = TRUE)


#-----------------------------------------------------------------------------#

# 4 ranking FIFA de como inician al mundial las 32 selecciones

r_fifa22 <- X2022_world_cup_groups
r_fifa22 <- r_fifa22[with(r_fifa22, order(r_fifa22$`Group`)), ]

#-----------------------------------------------------------------------------#
# 5 Resultados de historicos de juegos en la historia del FIFA World Cup
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

# 6 Resultados de historicos de juegos en la historia de los 32 equipos participantes 
#en Qatar 2022

q_2022 <- all_WC %>% 
  filter(`Team` %in% c("Qatar", "Germany", "Denmark", "Brazil", "France",
                       "Belgium", "Croatia", "Spain", "Serbia", "England", "Switzerland", "Netherlands", 
                       "Argentina", "Iran", "South Korea", "Japan", "Saudi Arabia", "Ecuador",
                       "Uruguay", "Canada","Ghana", "Senegal", "Portugal", "Poland", "Tunisia", 
                       "Morocco", "Cameroon","Mexico", "United States", "Wales", "Australia", 
                       "Costa Rica")) %>%
  select(Team, Wins, Ties, Loss)

#Anexion de la seleccion de Qatar dado a que es su primera participacion en este torneo

q_2022 <- q_2022 %>%
  rows_upsert(data.frame(Team = "Qatar", Wins = 0, Ties = 0, Loss = 0))


#----------- Parte II Visualizacion de los datos "Data Cleaning" -------------#

#----------------------------------------------------------------------------------------
# GRAFICA 1 Tabla "Total de goles en todas las selecciones en el torneo FIFA World Cup 
# desde el primer torneo oficial en Uruguay 1930"

reactable(
  total_gol,
  defaultSorted = "total",
  defaultSortOrder = "desc",
  defaultColDef = colDef(
    cell = data_bars(total_gol, text_position = "outside-base")
  )
)

# GRAFICA 2 Top 05 selecciones más goleadoras

ggplot(total_top_5, aes(x = reorder(Team, -total), y = total)) +
  geom_segment(aes(x = reorder(Team, -total),
                   xend = reorder(Team, -total),
                   y = 0, yend = total),
               color = "gray", lwd = 1) +
  geom_point(size = 7.5, pch = 21, bg = 4, col = 1) +
  geom_text(aes(label = total), color = "white", size = 3) +
  xlab("Team") +
  ylab("") +
  coord_flip() +
  theme_minimal() +
  shadow_mark() +
  enter_grow() +
  transition_states(total, wrap = FALSE) +
  labs(title = "Top 05 de selecciones más goleadoras",
       subtitle = "Qatar 2022",
       caption = "Fuente: Archivo de la Copa Mundial de la FIFA y RSSSF",
       tag = "Figura 1",
       x = "Selecciones",
       y = "Cantidad de Goles",
  )

#----------------------------------------------------------------------------------------
# GRAFICA 3 promedio de goles anotados en todas sus participaciones en
#el torneo dentro de los cinco (5) equipos favoritos para participar a 
#la final de Qatar 2022 segun la casa de apuesta betfair"

top_fav %>% 
  ggplot(aes(x = `Team`, y = Promedio, fill = `Team`)) +
  geom_col() +
  scale_fill_manual(values = c("cadetblue1", "seagreen3", "seashell1", "steelblue", "tomato2"), name = "Top 5") +
  scale_y_continuous(breaks = seq(10, 300, 25)) +
  geom_point(size = -1, pch = 20, bg = 4, col = 1) +
  geom_text(aes(label = total),
            vjust = 1.2, size = 4,
            inherit.aes = TRUE,
  ) +
  xlab("Team") +
  ylab("") +
  theme_minimal() +
  shadow_mark() +
  enter_grow() +
  transition_states(total, wrap = FALSE) +
  labs(title = "Promedio de goles anotado en todas sus participaciones en el",
       subtitle = "torneo dentro de los 05 equipos favoritos a participar a la final de Qatar 2022",
       caption = "Fuente: Archivo de la Copa Mundial de la FIFA y RSSSF",
       tag = "Figura 3",
       x = "Selecciones",
       y = "Promedio de Goles",
  )

#----------------------------------------------------------------------------------------
# Grafica 4 ranking FIFA de como inician al mundial las 32 selecciones

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
) #POR HACER

#-----------------------------------------------------------------------------#
# GRAFICA 5 Tabla de Resultados de historicos de juegos en la historia del FIFA World Cup

reactable(
  all_WC,
  defaultSorted = "Wins",
  defaultSortOrder = "desc",
  defaultColDef = colDef(
    cell = data_bars(all_WC, text_position = "outside-base")
  )
)

#-----------------------------------------------------------------------------#
# GRAFICA 6 Resultados de historicos de juegos en la historia de los 32 equipos participantes 
#en Qatar 2022
q_2022 %>% 
reactable(
  q_2022,
  pagination = FALSE,
  theme = void(cell_padding = 1, header_font_size = 0, font_color = "#000000"),
  columns = list(
  spenders = colDef(show = FALSE),
  spender_pal = colDef(show = FALSE),
  city = colDef(maxWidth = 120, align = "right"),
  spend_per_resident_data = colDef(
      cell = data_bars(q_2022, fill_color_ref = "spender_pal"))
            )
          )
