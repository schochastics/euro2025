library(dplyr)
library(reactable)
library(htmltools)
library(leaflet)

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

# Data for map
switzerland_sf <- readRDS("data/switzerland_sf.rds")
hotels <- readr::read_csv("data/geocoded_hotels.csv")
stadiums <- readr::read_csv("data/stadiums.csv")

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

# Data for standings table
standings_complete <- readRDS("data/standings_complete.rds")
fifa_rankings <- readRDS("data/fifa_rankings.rds")

make_table <- function(standings_complete, group) {
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
  standings_complete |>
    dplyr::filter(Group == group) |>
    select(-Group) |>
    reactable(
      columns = list(
        Team = colDef(
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
          }
        ),
        Played = colDef(name = "Played"),
        Won = colDef(name = "Won"),
        Drawn = colDef(name = "Drawn"),
        Lost = colDef(name = "Lost"),
        For = colDef(name = "For"),
        Against = colDef(name = "Against"),
        GoalDifference = colDef(name = "Goal Difference"),
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
