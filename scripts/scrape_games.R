library(rvest)
countries <- c(
  "Belgium",
  "Denmark",
  "England",
  "Spain",
  "Finland",
  "France",
  "Germany",
  "Iceland",
  "Italy",
  "Netherlands",
  "Norway",
  "Poland",
  "Portugal",
  "Sweden",
  "Switzerland",
  "Wales"
)

games <- "https://raw.githubusercontent.com/martj42/womens-international-results/refs/heads/master/results.csv" |>
  readr::read_csv() |>
  dplyr::filter(date >= as.Date("2000-01-01")) |>
  dplyr::filter(
    home_team %in% countries,
    away_team %in% countries
  ) |>
  select(date, home_team, away_team, home_score, away_score, tournament)

saveRDS(games, "app/data/games.rds")
