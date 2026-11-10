library(hoopR)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(scales)

pbp <- hoopR::load_nba_pbp(seasons = 2024)

clutch_pbp <- pbp %>%
  mutate(score_margin = abs(home_score - away_score),
         secs_left_qtr = start_quarter_seconds_remaining) %>%
  filter(
    period >= 4,
    (period == 4 & secs_left_qtr <= 300) | (period >= 5),
    score_margin <= 5
  )

rebound_events <- c(
  "Defensive Rebound", "Offensive Rebound",
  "Layup Shot Putback", "Putback Dunk Shot", "Tip Shot"
)

clutch_reb <- clutch_pbp %>%
  filter(type_text %in% rebound_events, !str_detect(type_text, "Free Throw")) %>%
  count(game_id, team_id, name = "clutch_reb")

clutch_team <- clutch_pbp %>%
  filter(type_text %in% rebound_events, !str_detect(type_text, "Free Throw")) %>%
  count(game_id, team_id, name = "clutch_reb") %>%
  
  full_join(
    clutch_pbp %>%
      arrange(game_id, sequence_number) %>%
      group_by(game_id) %>%
      mutate(lead_team = dplyr::lead(team_id)) %>%
      ungroup() %>%
      transmute(game_id,
                poss_change = team_id != lead_team,
                poss_team = lead_team) %>%
      filter(!is.na(poss_team), poss_change) %>%
      count(game_id, team_id = poss_team, name = "clutch_poss"),
    by = c("game_id", "team_id")
  ) %>%
  mutate(
    clutch_reb = replace(clutch_reb, is.na(clutch_reb), 0),
    clutch_poss = replace(clutch_poss, is.na(clutch_poss), 0)
  )

endgame <- pbp %>%
  filter(type_text == "End Game") %>%
  select(game_id, home_team_id, away_team_id, home_score, away_score)

clutch_wide <- clutch_team %>%
  left_join(endgame, by = "game_id") %>%
  mutate(team_role = if_else(team_id == home_team_id, "home", "away"),
         opp_id    = if_else(team_role == "home", away_team_id, home_team_id)) %>%
  left_join(clutch_team %>% 
              rename(opp_id = team_id,
                     opp_clutch_reb = clutch_reb,
                     opp_clutch_poss = clutch_poss),
            by = c("game_id","opp_id")) %>%
  mutate(
    # Replace NA with 0 for clutch_reb and clutch_poss
    clutch_reb = replace_na(clutch_reb, 0L),
    clutch_poss = replace_na(clutch_poss, 0L),
    opp_clutch_reb  = replace_na(opp_clutch_reb, 0L),
    opp_clutch_poss = replace_na(opp_clutch_poss, 0L),
    team_score = if_else(team_role=="home", home_score, away_score),
    opp_score  = if_else(team_role=="home", away_score, home_score),
    win = as.integer(team_score > opp_score),
    clutch_diff = as.numeric(clutch_reb - opp_clutch_reb),
    # NEW: Add total clutch possessions column
    total_clutch_poss = clutch_poss + opp_clutch_poss,
    crd_per100  = if_else(total_clutch_poss > 0,
                          100 * clutch_diff / total_clutch_poss,
                          NA_real_)
  ) %>%
  # Manual data correction update
  mutate(
    win = case_when(
      row_number() == 595 ~ 1,
      row_number() == 596 ~ 0,
      TRUE ~ win
    ),
    opp_clutch_reb = case_when(
      row_number() == 595 ~ 3,
      row_number() == 596 ~ 7,
      TRUE ~ opp_clutch_reb
    ),
    opp_clutch_poss = case_when(
      row_number() == 595 ~ 9,
      row_number() == 596 ~ 9,
      TRUE ~ opp_clutch_poss
    )
  ) %>%
  # Recalculating clutch_diff and crd_per100 after manual update
  mutate(
    clutch_diff = as.numeric(clutch_reb - opp_clutch_reb),
    total_clutch_poss = clutch_poss + opp_clutch_poss,
    crd_per100  = if_else(total_clutch_poss > 0,
                          100 * clutch_diff / total_clutch_poss,
                          NA_real_)
  ) %>%
  select(game_id, team_id, win, clutch_reb, opp_clutch_reb, clutch_poss, opp_clutch_poss,
         total_clutch_poss, clutch_diff, crd_per100)

clutch_diff_summary <- clutch_wide %>%
  filter(!is.na(crd_per100)) %>%
  mutate(bin = cut_number(crd_per100, n = 8)) %>%
  group_by(bin) %>%
  summarise(
    games   = dplyr::n(),
    win_pct = mean(win, na.rm = TRUE),
    .groups = "drop"
  )

clutch_wide %>%
  filter(!is.na(crd_per100)) %>%
  mutate(bin = cut_number(crd_per100, n = 8)) %>%
  group_by(bin) %>%
  summarise(win_pct = mean(win), n=n(), .groups="drop") %>%
  ggplot(aes(x = bin, y = win_pct, fill = win_pct)) +
  geom_col() +
  geom_text(aes(label = scales::percent(win_pct, accuracy = 0.1)), vjust = -0.3) +
  geom_text(aes(label = paste0("n=", n), y = win_pct/2), 
            color = "white", size = 3, fontface = "bold") +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_fill_gradient(low = "darkred", high = "green", name = "Win %") +
  labs(title = "Game Win% by Clutch Rebound Differential (per 100 poss, quintiles)",
       x = "CRD/100 quintile", y = "Win %") +
  theme_minimal()

clutch_wide %>%
  filter(!is.na(crd_per100)) %>%
  summarise(
    cor_clutchwin = cor(crd_per100, win, use = "complete.obs")
  )

clutch_diff_summary_raw <- clutch_wide %>%
  filter(!is.na(clutch_diff)) %>%
  group_by(clutch_diff) %>%
  summarise(
    games   = dplyr::n(),
    win_pct = mean(win),
    .groups = "drop"
  )

ggplot(clutch_diff_summary_raw, aes(x = clutch_diff, y = win_pct)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = scales::percent(win_pct, accuracy = 1)),
            vjust = -0.5, size = 3) +
  geom_text(aes(label = paste0("n=", games), y = win_pct/2), 
            angle = 90, hjust = -0.1, color = "white", 
            size = 3, fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title = "Win % by Raw Clutch Rebound Differential (Team − Opponent)",
    x = "Clutch Rebound Differential (raw, not per 100)",
    y = "Win %"
  ) +
  theme_minimal(base_size = 14)

team_game <- clutch_wide %>%
  filter(!is.na(crd_per100)) %>%
  group_by(team_id) %>%
  summarise(
    games = n(),
    clutch_mean = mean(crd_per100),
    team_win_pct = mean(win),
    .groups = "drop"
  )
cor_result <- team_game %>%
  summarise(cor_clutchwin = cor(clutch_mean, team_win_pct, use = "complete.obs")) %>%
  pull(cor_clutchwin)
ggplot(team_game, aes(x = clutch_mean, y = team_win_pct)) +
  geom_point(color = "steelblue", alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred", linetype = "dashed") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Relationship Between Clutch Rebounding (CRD/100) and Win Percentage",
    subtitle = paste("Team-Level Correlation: r =", round(cor_result, 2)),
    x = "Average Clutch Rebound Differential per 100 Possessions (CRD/100)",
    y = "Team's Season Win Percentage"
  ) +
  theme_minimal()

team_clutch_games <- clutch_wide %>%
  filter(!is.na(crd_per100)) %>% # Only games with clutch data
  group_by(team_id) %>%
  summarise(
    clutch_games = n(),
    clutch_win_pct = mean(win),
    avg_crd_per100 = mean(crd_per100),
    .groups = "drop"
  )
clutch_cor_result <- team_clutch_games %>%
  summarise(cor_clutchwin = cor(avg_crd_per100, clutch_win_pct, use = "complete.obs")) %>%
  pull(cor_clutchwin)
ggplot(team_clutch_games, aes(x = avg_crd_per100, y = clutch_win_pct)) +
  geom_point(aes(size = clutch_games), color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred", linetype = "dashed") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Win Percentage in Clutch Games vs Clutch Rebounding",
    subtitle = paste("Correlation in Clutch Games: r =", round(clutch_cor_result, 3)),
    x = "Average CRD/100 in Clutch Games",
    y = "Win Percentage in Clutch Games",
    size = "Number of\nClutch Games"
  ) +
  theme_minimal()