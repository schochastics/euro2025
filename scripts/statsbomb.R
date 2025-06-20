euro2022 <- jsonlite::fromJSON(
  "https://raw.githubusercontent.com/statsbomb/open-data/refs/heads/master/data/matches/53/106.json"
)
str(euro2022)
euro2022_matches <- tibble(
  match_id = euro2022$match_id,
  match_date = as.Date(euro2022$match_date),
  homer_team = euro2022$home_team$team_name,
  home_team_id = euro2022$home_team$team_id,
  away_team = euro2022$away_team$team_name,
  away_team_id = euro2022$away_team$team_id,
  home_score = euro2022$home_score,
  away_score = euro2022$away_score,
  competition_stage = euro2022$competition_stage$name
) |>
  arrange(match_date)

get_match <- function(match_id) {
  jsonlite::fromJSON(
    paste0(
      "https://raw.githubusercontent.com/statsbomb/open-data/refs/heads/master/data/events/",
      match_id,
      ".json"
    ),
    flatten = TRUE
  ) |>
    as_tibble()
}

matches <- map_dfr(
  euro2022_matches$match_id,
  get_match
)

passes <- matches |>
  dplyr::filter(type.id == 30) |>
  select(possession_team.name, location, pass.end_location) |>
  mutate(
    x = map_dbl(location, 1),
    y = map_dbl(location, 2),
    end_x = map_dbl(pass.end_location, 1),
    end_y = map_dbl(pass.end_location, 2)
  )

passes <- split(passes, passes$possession_team.name)

library(ggplot2)
ggplot(shots, aes(x = x, y = y, xend = end_x, yend = end_y)) +
  geom_segment() +
  coord_cartesian(xlim = c(0, 120), ylim = c(0, 80)) +
  theme_minimal()

ggplot(
  passes$`Netherlands Women's`,
  aes(x = x, y = y, xend = end_x, yend = end_y)
) +
  geom_segment() +
  coord_cartesian(xlim = c(0, 120), ylim = c(0, 80)) +
  theme_minimal() +
  theme(legend.position = "bottom")
