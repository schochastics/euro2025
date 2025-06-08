tibble(
  country = c("USA", "Canada", "Mexico"),
  value = c(10, 5, 8)
) |>
  reactable(
    columns = list(
      country = colDef(name = "Most Titles"),
      value = colDef(
        name = "",
        cell = function(value, index) {
          style <- if (index == 1) {
            "font-size: 1.2em;" # Slightly larger
          } else {
            NULL
          }
          div(style = style, value)
        }
      )
    ),
    rowStyle = function(index) {
      if (index == 1) {
        list(color = "white", background = "#211431")
      } else {
        NULL
      }
    },
    theme = reactableTheme(
      tableStyle = list(
        border = "2px solid #ccc", # Stroke around the table
        borderRadius = "10px", # Rounded corners
        overflow = "hidden" # Ensures rounding is visible
      ),
      headerStyle = list(
        background = "#211431",
        color = "white",
        borderBottom = "none"
      )
    )
  )


games <- "https://raw.githubusercontent.com/martj42/womens-international-results/refs/heads/master/results.csv" |>
  readr::read_csv() |>
  dplyr::filter(date >= as.Date("2000-01-01"))

flags <- readRDS("app/data/flags.rds")


first <- c(
  "China PR",
  "Republic of Ireland",
  "FR Yugoslavia",
  "Czech Republic",
  "Åland",
  "Ivory Coast",
  "Tahiti",
  "Serbia and Montenegro",
  "Congo",
  "Netherlands Antilles",
  "Macedonia",
  "U.S. Virgin Islands",
  "East Timor",
  "Eswatini",
  "Bonaire",
  "Curacao"
)

second <- c(
  "China",
  "Ireland",
  "Serbia and Montenegro",
  "Czechia",
  "Åland Islands",
  "Côte d'Ivoire (Ivory Coast)",
  "French Polynesia",
  "Serbia and Montenegro",
  "Republic of the Congo",
  "Caribbean Netherlands",
  "North Macedonia",
  "United States Virgin Islands",
  "Timor-Leste",
  "Eswatini (Swaziland)",
  "Bonaire",
  "Curaçao"
)

trans <- data.frame(
  original = first,
  matched = second
)

dput(unique(games$away_team)[!unique(games$away_team) %in% flags$country])

games$home_team <- ifelse(
  games$home_team %in% trans$original,
  trans$matched[match(games$home_team, trans$original)],
  games$home_team
)

games$away_team <- ifelse(
  games$away_team %in% trans$original,
  trans$matched[match(games$away_team, trans$original)],
  games$away_team
)

el <- games |>
  mutate(
    res = ifelse(
      home_score > away_score,
      "H",
      ifelse(home_score < away_score, "A", "D")
    )
  ) |>
  select(home_team, away_team, res) |>
  # compute losses and draw‐fraction per match
  mutate(
    loss_home = ifelse(res == "A", 1, 0), # home lost if away won
    loss_away = ifelse(res == "H", 1, 0), # away lost if home won
    draw = ifelse(res == "D", 0.5, 0) # each side gets 0.5 on a draw
  ) |>
  # turn each match into two “directed” rows
  rowwise() |>
  do({
    tibble(
      from = c(.$home_team, .$away_team),
      to = c(.$away_team, .$home_team),
      weight = c(.$loss_home, .$loss_away) + .$draw
    )
  }) |>
  ungroup() |>
  # aggregate over all matches
  group_by(from, to) |>
  summarise(weight = sum(weight), .groups = "drop") |>
  # drop any zero‐weight pairs
  filter(weight > 0)

library(igraph)
library(g6R)
g <- el |>
  graph_from_data_frame(directed = TRUE)

xy <- graphlayouts::layout_with_stress(g, weights = 1 / E(g)$weight)
V(g)$x <- xy[, 1]
V(g)$y <- xy[, 2]
g6_igraph(g) |>
  g6_layout(force_atlas2_layout()) |>
  g6_options(autoResize = TRUE, autoFit = "view") |>
  g6_behaviors(drag_element())
