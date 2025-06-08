library(rvest)
library(dplyr)
library(stringr)
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

url <- "https://www.worldfootball.net/all_matches/frauen-em-2022-england/"

doc <- read_html(url)
schedule_urls <- doc |>
  html_element("#site select") |>
  html_elements("option") |>
  html_attr("value") |>
  paste0("https://www.worldfootball.net/", ... = _)

raw <- schedule_urls |>
  purrr::map(function(url) {
    read_html(url) |>
      html_element(".data .standard_tabelle") |>
      html_table() |>
      janitor::clean_names() |>
      mutate(url = url)
  })

games <- purrr::map(raw, function(x) {
  x <- x[, c(1, 3, 5, 6)]
  names(x) <- c("date", "home_team", "away_team", "result")
  x |>
    mutate(date = na_if(date, "")) |>
    tidyr::fill(date) |>
    mutate(date = as.Date(date, format = "%d/%m/%Y")) |>
    dplyr::filter(!is.na(date)) |>
    mutate(final_score = str_remove(result, "\\s+.*")) |>
    mutate(
      pre_pso_score = case_when(
        str_detect(result, "pso") ~
          str_match(result, ".*\\((.*)\\)")[, 2] %>%
            str_split(",\\s*") %>%
            sapply(function(x) tail(x, 1)),
        TRUE ~ str_trim(str_remove(result, "\\(.*\\)"))
      )
    ) |>
    mutate(year = lubridate::year(date))
}) |>
  bind_rows() |>
  dplyr::filter(result != "-:-") |>
  mutate(
    home_score = as.integer(str_extract(pre_pso_score, "^\\d+")),
    away_score = as.integer(str_extract(pre_pso_score, "\\d+$")),
    hw = as.integer(str_extract(final_score, "^\\d+")),
    aw = as.integer(str_extract(final_score, "\\d+$")),
    winner = if_else(hw > aw, home_team, if_else(hw < aw, away_team, "Draw")),
    looser = if_else(hw > aw, away_team, if_else(hw < aw, home_team, "Draw")),
  )
