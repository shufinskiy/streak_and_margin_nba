library(data.table)
library(tidyverse)
library(ggthemes)
library(gridExtra)


x <- fread(":./data/pbp_TOR_GSW_3.csv")

calculate_streak_and_margin <- function(table, away, home) {
  table1 <- table[, .(PERIOD, PCTIMESTRING, SCORE, PLAYER1_TEAM_ABBREVIATION)]
  
  table1 <- table1[, c("min", "sec") := tstrsplit(PCTIMESTRING, ":", fixed = TRUE)][
    , PCTIMESTRING := NULL][
      , c("min", "sec") := lapply(.SD, as.numeric), .SDcols = c("min", "sec")][
        , "Time" := abs((min * 60 + sec) - 720 * PERIOD)
        ][, c("PERIOD", "min", "sec") := NULL][
          , c("Away", "Home") := tstrsplit(SCORE, "-", fixed = TRUE)][
          !is.na(Home)][, SCORE := NULL][
            , c("Away", "Home") := lapply(.SD, as.numeric), .SDcols = c("Away", "Home")][
            , ":=" (Away = Away - lag(Away, default = 0),
                    Home = Home - lag(Home, default = 0))][Away != 0 | Home != 0][
                      , Away1 := cumsum(Away)][, Away1 := if_else(Away == 0, 0, Away1)][
                      , Away1 := if_else(lead(Away1, default = 0) == 0, Away1, 0)]
  
  table2 <- table1[Away1 == 0]
  table3 <- table1[Away1 != 0]
  
  table3 <- table3[, Away1 := Away1 - lag(Away1, default =0)]
  
  table1 <- bind_rows(table2, table3)
  
  table1 <- table1[order(Time)][, Home1 := cumsum(Home)][
    , Home1 := if_else(Home == 0, 0, Home1)][
    , Home1 := if_else(lead(Home1, default = 0) == 0, Home1, 0)]
  
  table2 <- table1[Home1 == 0]
  table3 <- table1[Home1 != 0]
  
  table3 <- table3[, Home1 := Home1 - lag(Home1, default =0)]
  
  table1 <- bind_rows(table2, table3)
  
  table1 <- table1[order(Time)][, Home1 := -(Home1)][Home1 != 0 | Away1 != 0][
    , streak := Away1 + Home1][, .(Time, streak, PLAYER1_TEAM_ABBREVIATION)][
      , Margin := cumsum(streak)]
  
  gtable <- table1[, TYPE := if_else(abs(streak) <= 3, "1POSS",
                                       if_else(abs(streak) <= 6, 
                                               "2POSS", "bigstreak"))][
                                                 , .(POSS1 = sum(abs(streak))), 
                    by = .(TYPE, PLAYER1_TEAM_ABBREVIATION)][
                      , PTS := sum(POSS1), by = PLAYER1_TEAM_ABBREVIATION][
                        , PRECENT_PTS := round(POSS1/PTS*100, digits = 2)][order(TYPE)][
    , .(TEAM = PLAYER1_TEAM_ABBREVIATION, TYPE, PRECENT_PTS)]
  
  gtable <- tableGrob(gtable)
  
  ty <- table1[, .(.N), by = .(PLAYER1_TEAM_ABBREVIATION, abs(streak))]
  
  table_color <- data.table(PLAYER1_TEAM_ABBREVIATION = c("ATL", "BOS", "BKN", "CHA", "CHI", 
                                                          "CLE", "DAL", "DEN", "DET", "GSW", 
                                                          "HOU", "IND", "LAC", "LAL", "MEM", 
                                                          "MIA", "MIL", "MIN", "NOP", "NYK", 
                                                          "OKC", "ORL", "PHI", "PHX", "POR", 
                                                          "SAC", "SAS", "TOR", "UTA", "WAS"),
                            TEAM_color = c("#E03A3E", "#007A33", "#000000", "#1D1160", "#CE1141", "#6F263D",
                                           "#00538C", "#0E2240", "#C8102E", "#006BB6", "#CE1141", "#002D62",
                                           "#C8102E", "#552583", "#5D76A9", "#98002E", "#00471B", "#0C2340",
                                           "#0C2340", "#006BB6", "#007AC1", "#0077C0", "#006BB6", "#1D1160",
                                           "#E03A3E", "#5A2D81", "#C4CED4", "#CE1141", "#002B5C", "#002B5C"))
  
  table1 <- left_join(table1, table_color)
  
  table1 <- setDT(table1)
  
  away_color <- table1[PLAYER1_TEAM_ABBREVIATION == away, unique(TEAM_color)]
  home_color <- table1[PLAYER1_TEAM_ABBREVIATION == home, unique(TEAM_color)]
  
  t <- sort(unique(table1$PLAYER1_TEAM_ABBREVIATION))
  first <- t[1]
  second <- t[2]
  
  first_color <- table1[PLAYER1_TEAM_ABBREVIATION == first, unique(TEAM_color)]
  second_color <- table1[PLAYER1_TEAM_ABBREVIATION == second, unique(TEAM_color)]

  cols <- as.vector(c(first = first_color, second = second_color))
  
  gg <- ggplot(table1, aes(x = seq_along(streak), y = streak, 
                          fill = PLAYER1_TEAM_ABBREVIATION, label = abs(streak))) + 
    geom_bar(stat = "identity") +
    geom_text(nudge_y = if_else(table1$streak > 0, 1, -1), size = 2) +
    scale_fill_manual(values = cols) +
    theme_tufte() +
    labs(fill = "Team") +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "top")
  
  gg1 <- ggplot(table1, aes(x = seq_along(Margin), y = Margin, label = abs(Margin))) +
    geom_line(col = "gray80") +
    geom_point(col = if_else(table1$Margin > 0, away_color, home_color), size = 3) +
    geom_text(nudge_y = if_else(table1$Margin > 0, 1, -1), size = 2) +
    theme_tufte() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  
  gg2 <- ggplot(ty, aes(x = as.factor(abs), y = N, 
                        fill = PLAYER1_TEAM_ABBREVIATION)) +
    geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
    geom_text(aes(label = N, y = N + 0.5), position = position_dodge2(1), size = 2) +
    scale_fill_manual(values = cols) +
    theme_tufte() +
    labs(fill = "Team", x = "Streaks (PTS)") +
    theme(axis.title.y = element_blank(),
          legend.position = "top")
  
  output <- grid.arrange(gg, gg2, gg1, gtable, ncol = 2)
  
  title <- paste("Streak and Margin in Game", away, "vs.", home, sep = " ")
  caption <- "Data sourse: stats.nba.com, Telegram: @NBAatlantic, Twitter: @vshufinskiy"
  
  output <- grid.arrange(top = title, bottom = caption, output)
}

r <- calculate_streak_and_margin(x, away = "TOR", home = "GSW")

ggsave("streak.jpeg", r, width = 10, height = 7, unit = "in")
