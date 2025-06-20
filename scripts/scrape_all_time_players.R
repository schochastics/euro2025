library(rvest)

urls <- glue::glue(
  "https://www.worldfootball.net/alltime_top_player/frauen-em/{1:25}/"
)

appearance <- map_dfr(urls, function(url) {
  read_html(url) |>
    html_elements(".data .standard_tabelle") |>
    html_table()
})
appearance |>
  rename("Country" = `Team(s)`) |>
  mutate(Player = str_remove(Player, "\\s+\\*")) |>
  saveRDS("app/data/all_time_appearance.rds")

urls <- glue::glue(
  "https://www.worldfootball.net/alltime_goalgetter/frauen-em/tore/{1:6}/"
)
scorer <- map_dfr(urls, function(url) {
  read_html(url) |>
    html_elements(".data .standard_tabelle") |>
    html_table()
})

scorer |>
  select(Player, "Country" = `Team(s)`, "Matches" = `M.`, "Goals" = goals) |>
  mutate(Player = str_remove(Player, "\\s+\\*")) |>
  saveRDS("app/data/all_time_scorer.rds")

readRDS("app/data/all_time_scorer.rds") |>
  reactable(
    columns = list(
      Player = colDef(name = "Player"),
      Country = colDef(name = "Country"),
      Matches = colDef(name = "Matches"),
      Goals = colDef(name = "Goals")
    ),
    defaultPageSize = 25,
    searchable = FALSE,
    filterable = TRUE
  )
