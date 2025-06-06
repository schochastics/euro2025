
library(tidyverse)
library(reactable)
library(rvest)
library(htmltools)
library(leaflet)

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

url <- "https://www.worldfootball.net/all_matches/frauen-em-2025-schweiz/"

tab <- url |> 
  read_html() |> 
  html_elements("table") |> 
  html_table() |> 
  _[[2]]

names(tab) <- c(
  "Date",
  "Time",
  "HomeTeam",
  "not1",
  "AwayTeam",
  "Result",
  "not2"
)

schedule <- tab |> 
  dplyr::filter(!str_detect(Date, "Group")) |> 
  select(-contains("not")) |> 
  mutate(Date = na_if(Date,"")) |> 
  tidyr::fill(Date) |> 
  _[1:(4*6),] |> 
  mutate(Group=rep(LETTERS[1:4],each=6)) |> 
  mutate(
    Date = as.Date(Date,"%d/%m/%Y"),
    Time = str_replace(Time, "^\\d{1,2}", function(x) {
      sprintf("%02d", (as.integer(x) + 1) %% 24)})
  )

# Get full list of teams per group
teams <- schedule |>
  select(Group, Team = HomeTeam) |>
  bind_rows(schedule |> select(Group, Team = AwayTeam)) |>
  distinct()

# Filter played matches
played_matches <- schedule |>
  filter(Result != "-:-") |>
  separate(
    Result,
    into = c("HomeGoals", "AwayGoals"),
    sep = ":",
    convert = TRUE
  )

# Create long format with one row per team
long_matches <- played_matches |>
  mutate(
    HomePoints = case_when(
      HomeGoals > AwayGoals ~ 3,
      HomeGoals == AwayGoals ~ 1,
      TRUE ~ 0
    ),
    AwayPoints = case_when(
      AwayGoals > HomeGoals ~ 3,
      HomeGoals == AwayGoals ~ 1,
      TRUE ~ 0
    )
  ) |>
  select(
    Group,
    HomeTeam,
    AwayTeam,
    HomeGoals,
    AwayGoals,
    HomePoints,
    AwayPoints
  ) |>
  pivot_longer(
    cols = c(HomeTeam, AwayTeam),
    names_to = "Location",
    values_to = "Team"
  ) |>
  mutate(
    GoalsFor = if_else(Location == "HomeTeam", HomeGoals, AwayGoals),
    GoalsAgainst = if_else(Location == "HomeTeam", AwayGoals, HomeGoals),
    Points = if_else(Location == "HomeTeam", HomePoints, AwayPoints),
    Won = if_else(GoalsFor > GoalsAgainst, 1, 0),
    Drawn = if_else(GoalsFor == GoalsAgainst, 1, 0),
    Lost = if_else(GoalsFor < GoalsAgainst, 1, 0),
    Played = 1
  )

# Summarise to standings
standings <- long_matches |>
  group_by(Group, Team) |>
  summarise(
    Played = sum(Played),
    Won = sum(Won),
    Drawn = sum(Drawn),
    Lost = sum(Lost),
    For = sum(GoalsFor),
    Against = sum(GoalsAgainst),
    GoalDifference = For - Against,
    Points = sum(Points),
    .groups = "drop"
  )  

standings_complete <- teams |>
  left_join(standings, by = c("Group", "Team")) |>
  mutate(across(Played:Points, ~ replace_na(., 0))) |>
  arrange(Group, desc(Points), desc(GoalDifference), desc(For))


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