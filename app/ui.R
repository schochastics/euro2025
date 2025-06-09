# ui.R

library(shiny)
library(bslib)

# ───────────────────────────────────────────────────────────────────────────────
# 1. Build a bslib theme and inject custom CSS rules via bs_add_rules()
# ───────────────────────────────────────────────────────────────────────────────

my_theme <- bs_theme(
  bg = "#F3F3F3",
  fg = "black",
  primary = "#ff9e33",
  secondary = "red",
  base_font = font_google("Roboto"),
  heading_font = font_google("Saira"),
  # code_font = font_google("Press Start 2P"),
  "font-size-base" = "0.75rem",
  "enable-rounded" = FALSE
) |>
  bs_add_rules(
    list(
      sass::sass_file("www/custom.scss")
    )
  )
# ───────────────────────────────────────────────────────────────────────────────
# 2. UI: page_fluid with navset_pill()
# ───────────────────────────────────────────────────────────────────────────────

ui <- page_fluid(
  theme = my_theme,

  # ─── NAVSET_PILL: the pill navigation itself ────────────────────────────────
  navset_pill(
    id = "tab",
    # ───────────────────────────────────────────────────────────────────────────
    nav_panel(
      title = "Home",
      value = "home_tab",
      fluidRow(
        column(2),
        column(
          8,
          offset = 2,
          img(src = "logo.svg", height = "600px")
        ),
        column(2)
      )
    ),
    # ───────────────────────────────────────────────────────────────────────────
    nav_panel(
      title = "Teams",
      value = "teams_tab",
      layout_sidebar(
        uiOutput("country_ui"),
        sidebar = sidebar(
          width = 200,
          tags$div(
            class = "flag-grid",
            lapply(countries, function(ctr) {
              ccode <- flags$iso2[flags$country == ctr]
              actionButton(
                inputId = ccode,
                label = HTML(
                  paste0(
                    "<img src='flags/",
                    ccode,
                    ".svg' alt='",
                    ctr,
                    "' height = '40px'/>"
                  )
                ),
                class = "btn-flag"
              )
            })
          )
        )
      )
    ),
    # ───────────────────────────────────────────────────────────────────────────
    nav_panel(
      title = "Standings",
      value = "standings_tab",
      h2("Group Standings", class = "header"),
      fluidRow(
        column(
          width = 6,
          div(
            h2("Group A", class = "country"),
            tags$details(
              tags$summary(strong("Show Schedule")),
              div(
                reactableOutput("scheduleA", width = "100%")
              )
            ),
            reactableOutput("tableA", width = "100%"),
            class = "table-container",
          )
        ),
        column(
          width = 6,
          div(
            h2("Group B", class = "country"),
            tags$details(
              tags$summary(strong("Show Schedule")),
              div(
                reactableOutput("scheduleB", width = "100%")
              )
            ),
            reactableOutput("tableB", width = "100%"),
            class = "table-container",
          )
        )
      ),
      fluidRow(
        column(
          width = 6,
          div(
            h2("Group C", class = "country"),
            tags$details(
              tags$summary(strong("Show Schedule")),
              div(
                reactableOutput("scheduleC", width = "100%")
              )
            ),
            reactableOutput("tableC", width = "100%"),
            class = "table-container",
          )
        ),
        column(
          width = 6,
          div(
            h2("Group D", class = "country"),
            tags$details(
              tags$summary(strong("Show Schedule")),
              div(
                reactableOutput("scheduleD", width = "100%")
              )
            ),
            reactableOutput("tableD", width = "100%"),
            class = "table-container",
          )
        )
      )
    ),

    # ───────────────────────────────────────────────────────────────────────────
    nav_panel(
      title = "Predictions",
      value = "prediction_tab",
      h2("FIFA Ranking and Win Probabilities", class = "header"),
      fluidRow(
        column(
          width = 6,
          div(
            h2("Current FIFA Ranking", class = "country"),
            reactableOutput("fifa_ranking", width = "100%"),
            class = "table-container",
          )
        ),
        column(
          width = 6,
          div(
            h2("Prediction", class = "country"),
            reactableOutput("forecast", width = "100%"),
            class = "table-container",
          )
        )
      )
    ),
    # ───────────────────────────────────────────────────────────────────────────
    nav_panel(
      title = "Locations",
      value = "loc_tab",
      h2("Hotels and Stadiums", class = "header"),
      leafletOutput("map", height = "600px", width = "100%")
    ),
    # ───────────────────────────────────────────────────────────────────────────
    # nav_panel(
    #   title = "Pill 5",
    #   value = "pill5_tab",
    #   "Content for Pill 5…"
    # ),
    nav_spacer(),
    # ───────────────────────────────────────────────────────────────────────────
    nav_menu(
      title = "More",
      nav_panel(
        "Head-to-Head",
        value = "head_to_head_tab",
        fluidPage(
          h2("Head-to-Head Comparison", class = "header"),

          fluidRow(
            column(1),

            column(
              3,
              div(
                style = "text-align: center;",
                selectInput(
                  "country1",
                  "Select Country 1:",
                  choices = countries,
                  selected = countries[1]
                ),
                uiOutput("flag1")
              )
            ),

            column(
              4,
              div(
                style = "text-align: center;",
                tags$h3("VS", style = "text-align: center; margin-top: 60px;"),
                uiOutput("stats_box")
              )
            ),

            column(
              3,
              div(
                style = "text-align: center;",
                selectInput(
                  "country2",
                  "Select Country 2:",
                  choices = countries,
                  selected = countries[2]
                ),
                uiOutput("flag2")
              )
            ),
            column(1)
          )
        )
      ),
      nav_panel(
        "All-time Tournament Stats",
        value = "tournament_tab",
        h2("All-time Tournament Statistics", class = "header"),
        fluidRow(
          column(1), # Left spacer
          column(
            10,
            fluidRow(
              column(
                4,
                reactableOutput("most_titles", width = "100%")
              ),
              column(
                4,
                reactableOutput("matches_played", width = "100%")
              ),
              column(
                4,
                reactableOutput("matches_won", width = "100%")
              ),
            ),
            fluidRow(
              column(
                3,
                reactableOutput("matches_drawn", width = "100%")
              ),
              column(
                3,
                reactableOutput("matches_lost", width = "100%")
              ),
              column(
                3,
                reactableOutput("goals_scored", width = "100%")
              ),
              column(
                3,
                reactableOutput("goals_conceded", width = "100%")
              )
            ),
          ),
          column(1) # Right spacer
        )
      ),
      nav_panel(
        "All-time Player stats",
        value = "player_stats_tab",
        h2("All-time Player Statistics", class = "header"),
        fluidRow(
          column(1), # Left spacer
          column(
            10,
            fluidRow(
              column(
                6,
                h4("All Time Appearances"),
                reactableOutput("all_time_appearance", width = "100%")
              ),
              column(
                6,
                h4("All Time Goal Scorers"),
                reactableOutput("all_time_scorer", width = "100%")
              )
            )
          ),
          column(1) # Right spacer
        )
      ),
      "----",
      nav_item(
        a(
          href = "https://github.com/schochastics/euro2025",
          target = "_blank",
          icon("github", class = "me-1"),
          "GitHub"
        )
      )
    ) # end nav_menu()
  ) # end navset_pill()
) # end page_fluid()
