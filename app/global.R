library(dplyr)
library(reactable)
library(htmltools)
library(leaflet)

prob_col <- function(x) {
  get_color <- colorRamp(c("#ffffff", "#ff9e33"))
  rgb(get_color(x), maxColorValue = 255)
}

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

country_map <- c(
  be = "Belgium",
  dk = "Denmark",
  `gb-eng` = "England",
  es = "Spain",
  fi = "Finland",
  fr = "France",
  de = "Germany",
  is = "Iceland",
  it = "Italy",
  nl = "Netherlands",
  no = "Norway",
  pl = "Poland",
  pt = "Portugal",
  se = "Sweden",
  ch = "Switzerland",
  `gb-wls` = "Wales"
)

# Data for map
switzerland_sf <- readRDS("data/switzerland_sf.rds")
hotels <- readr::read_csv("data/geocoded_hotels.csv")
stadiums <- readr::read_csv("data/stadiums.csv")
flags <- readRDS("data/flags.rds")

# Data for standings table
standings_complete <- readRDS("data/standings_complete.rds")
fifa_ranking <- readRDS("data/fifa_ranking.rds")
forecast <- readRDS("data/tournament_probabilities.rds")
P_group <- readRDS("data/P_group.rds")
games <- readRDS("data/games.rds")
player_appearance <- readRDS("data/all_time_appearance.rds")
player_scorer <- readRDS("data/all_time_scorer.rds")
tournament_summary <- readRDS("data/tournament_summary.rds")

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

swiss_map <- function() {
  leaflet(data = switzerland_sf) |>
    addTiles() |>
    addMarkers(
      data = stadiums,
      lng = ~Longitude,
      lat = ~Latitude,
      popup = ~ paste(
        "<strong>Stadium:</strong>",
        Name,
        "<br>",
        "<strong>City:</strong>",
        City,
        "<br>",
        "<strong>Capacity:</strong>",
        Capacity
      ),
      icon = logo
    ) |>
    addMarkers(
      data = hotels,
      lng = ~longitude,
      lat = ~latitude,
      popup = ~ paste(
        "<strong>Country:</strong>",
        Land,
        "<br>",
        "<strong>Hotel:</strong>",
        Hotel,
        "<br>",
        "<strong>City:</strong>",
        HotelCity
      ),
      icon = flag_icons
    ) |>
    setView(lng = 8.2275, lat = 46.8182, zoom = 8) |>
    addPolygons(
      color = "#444444",
      weight = 1,
      fillColor = "#ff9e33",
      fillOpacity = 0.25
    )
}

country_cell <- function(name = "", ...) {
  colDef(
    minWidth = 100,
    name = name,
    html = TRUE,
    cell = function(value) {
      code <- flags$iso2[flags$country == value]
      if (is.null(code)) {
        return(value)
      }
      img_tag <- img(
        src = sprintf(
          "flags/%s.svg",
          code
        ),
        class = "flag-table",
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

rounded_theme <- function(...) {
  reactableTheme(
    tableStyle = list(
      border = "2px solid #ccc",
      borderRadius = "10px",
      overflow = "hidden"
    ),
    headerStyle = list(
      background = "#211431",
      color = "white",
      borderBottom = "none"
    ),
    ...
  )
}

make_schedule <- function(schedule, group) {
  schedule |>
    dplyr::filter(Group == group) |>
    rowwise() |>
    mutate(
      home_win = round(
        100 *
          P_group[
            which(rownames(P_group[,, 1]) == HomeTeam),
            which(colnames(P_group[,, 1]) == AwayTeam),
            1
          ],
        2
      ),
      draw = round(
        100 *
          P_group[
            which(rownames(P_group[,, 2]) == HomeTeam),
            which(colnames(P_group[,, 2]) == AwayTeam),
            2
          ],
        2
      ),
      away_win = round(
        100 *
          P_group[
            which(rownames(P_group[,, 1]) == AwayTeam),
            which(colnames(P_group[,, 1]) == HomeTeam),
            1
          ],
        2
      )
    ) |>
    select(-Group) |>
    mutate(Result = ifelse(Result == "-:-", Time, Result)) |>
    select(-Time) |>
    reactable(
      columns = list(
        Date = colDef(name = "Date"),
        HomeTeam = country_cell(name = ""),
        AwayTeam = country_cell(name = ""),
        Result = colDef(name = "Result", align = "center"),
        home_win = forecast_cell("Home Win", align = "right"),
        draw = forecast_cell("Draw", align = "right"),
        away_win = forecast_cell("Away Win", align = "right")
      ),
      # ,
      # columnGroups = list(
      #   colGroup(
      #     name = "Probabilities",
      #     columns = c("home_win", "draw", "away_win"),
      #     headerClass = "myGroupHeader",
      #   )
      # ),
      bordered = FALSE,
      highlight = TRUE,
      striped = FALSE,
      fullWidth = TRUE,
      style = list(border = "none"),
      defaultColDef = colDef(
        minWidth = 100,
        style = list(borderRight = "none"),
        headerStyle = list(fontWeight = "normal")
      ),
      theme = rounded_theme()
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
      },
      theme = rounded_theme()
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
      theme = rounded_theme(),
      defaultPageSize = 16,
      striped = FALSE,
      highlight = TRUE
    )
}

forecast_cell <- function(name, ...) {
  colDef(
    name = name,
    maxWidth = 80,
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
    ),
    theme = rounded_theme(),
    highlight = TRUE
  )
}

make_simple_table <- function(df, ...) {
  reactable(
    df,
    columns = list(
      Country = country_cell()
    ),
    defaultPageSize = 25,
    searchable = FALSE,
    filterable = TRUE,
    highlight = TRUE,
    ...
  )
}

make_cup_summary <- function(data, name) {
  reactable(
    data,
    columns = list(
      country = country_cell(name = name),
      n = colDef(
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
    ),
    defaultPageSize = 5
  )
}
