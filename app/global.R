library(dplyr)
library(reactable)
library(htmltools)
library(leaflet)

prob_col <- function(x) {
  get_color <- colorRamp(c("#ffffff", "#ff9e33"))
  rgb(get_color(x), maxColorValue = 255)
}

# Define country codes and names for the flags
country_codes <- c(
  BEL = "Belgium",
  DEN = "Denmark",
  ENG = "England",
  ESP = "Spain",
  FIN = "Finland",
  FRA = "France",
  GER = "Germany",
  ISL = "Iceland",
  ITA = "Italy",
  NED = "Netherlands",
  NOR = "Norway",
  POL = "Poland",
  POR = "Portugal",
  SWE = "Sweden",
  SUI = "Switzerland",
  WAL = "Wales"
)

name_to_code <- c(
  Belgium = "BEL",
  Denmark = "DEN",
  England = "ENG",
  Spain = "ESP",
  Finland = "FIN",
  France = "FRA",
  Germany = "GER",
  Iceland = "ISL",
  Italy = "ITA",
  Netherlands = "NED",
  Norway = "NOR",
  Poland = "POL",
  Portugal = "POR",
  Sweden = "SWE",
  Switzerland = "SUI",
  Wales = "WAL"
)

# Data for map
switzerland_sf <- readRDS("data/switzerland_sf.rds")
hotels <- readr::read_csv("data/geocoded_hotels.csv")
stadiums <- readr::read_csv("data/stadiums.csv")
# Data for standings table
standings_complete <- readRDS("data/standings_complete.rds")
fifa_ranking <- readRDS("data/fifa_ranking.rds")
forecast <- readRDS("data/tournament_probabilities.rds")
games <- readRDS("data/games.rds")
player_appearance <- readRDS("data/all_time_appearance.rds")
player_scorer <- readRDS("data/all_time_scorer.rds")

hotels$flag_url <- paste0("https://flagcdn.com/w20/", hotels$iso2, ".png")

flag_icons <- makeIcon(
  iconUrl = hotels$flag_url,
  iconWidth = 20,
  iconHeight = 15
)

logo <- makeIcon(
  iconUrl = "https://upload.wikimedia.org/wikipedia/commons/e/e7/UEFA_Women%27s_Euro_Switzerland_2025_Logo.png",
  iconWidth = 30 / 2,
  iconHeight = 36 / 2
)

country_cell <- function(...) {
  colDef(
    minWidth = 100,
    name = "",
    html = TRUE,
    cell = function(value) {
      code <- name_to_code[[value]]
      if (is.null(code)) return(value)
      img_tag <- img(
        src = sprintf(
          "flags/%s.png",
          code
        ),
        style = "height: 20px; margin-right: 8px;",
        alt = code
      )
      tagList(
        div(
          style = "display: inline-flex; align-items: center;",
          img_tag,
          value
        )
      )
    },
    ...
  )
}
make_table <- function(standings_complete, group) {
  standings_complete |>
    dplyr::filter(Group == group) |>
    select(-Group) |>
    reactable(
      columns = list(
        Team = country_cell(),
        Played = colDef(name = "Played"),
        Won = colDef(name = "Won"),
        Drawn = colDef(name = "Drawn"),
        Lost = colDef(name = "Lost"),
        For = colDef(name = "For"),
        Against = colDef(name = "Against"),
        GoalDifference = colDef(name = "Diff."),
        Points = colDef(name = "Points", style = list(fontWeight = "bold"))
      ),
      bordered = FALSE,
      highlight = TRUE,
      striped = FALSE,
      fullWidth = TRUE,
      style = list(border = "none"),
      defaultColDef = colDef(
        minWidth = 50,
        style = list(borderRight = "none"),
        headerStyle = list(fontWeight = "normal")
      ),
      rowStyle = function(index) {
        if (index %in% c(1, 2)) {
          list(background = "#ffe1c1")
        } else {
          NULL
        }
      }
    )
}

make_fifa <- function(fifa_ranking) {
  fifa_ranking |>
    mutate(group = LETTERS[group]) |>
    arrange(rank) |>
    reactable(
      columns = list(
        name = country_cell(),
        rank = colDef(name = "Global Rank", align = "center"),
        totalPoints = colDef(
          name = "Points",
          align = "right",
          style = list(fontWeight = "bold")
        ),
        group = colDef(name = "Group", align = "center")
      ),
      defaultPageSize = 16,
      striped = FALSE,
      highlight = TRUE
    )
}

forecast_cell <- function(name, ...) {
  colDef(
    name = name,
    format = colFormat(digits = 2),
    style = function(value) {
      txt <- ifelse(value < 45, "black", "white")
      list(color = txt, background = prob_col(value / 100))
    },
    ...
  )
}

make_forecast <- function(df) {
  reactable(
    arrange(df, -winner),
    defaultPageSize = 16,
    columns = list(
      team = country_cell(),
      winner = forecast_cell("Winner"),
      final = forecast_cell("Final"),
      semi = forecast_cell("Semi"),
      quarter = forecast_cell("Quarter"),
      group_first = forecast_cell("Group 1st"),
      group_second = forecast_cell("Group 2nd")
    ),
    defaultColDef = colDef(
      minWidth = 50
    )
  )
}

make_simple_table <- function(df, ...) {
  reactable(
    df,
    defaultPageSize = 25,
    searchable = FALSE,
    filterable = TRUE,
    highlight = TRUE,
    ...
  )
}
