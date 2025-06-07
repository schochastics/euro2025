# ui.R

library(shiny)
library(bslib)

# ───────────────────────────────────────────────────────────────────────────────
# 1. Build a bslib theme and inject custom CSS rules via bs_add_rules()
# ───────────────────────────────────────────────────────────────────────────────

my_theme <- bs_theme(
  bg = "white",
  fg = "black",
  primary = "#ff9e33",
  secondary = "red",
  # base_font = font_google("Press Start 2P"),
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
      h2("Women's Euro 2025", class = "country"),
      img(src = "logo.svg", height = "400px")
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
            lapply(names(country_codes), function(ccode) {
              actionButton(
                inputId = ccode,
                label = HTML(
                  paste0(
                    "<img src='flags/",
                    ccode,
                    ".png' alt='",
                    country_codes[ccode],
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
      title = "Locations",
      value = "loc_tab",
      card(
        card_header(
          class = "bg-purple",
          "Stadiums and Team Hotels"
        ),
        leafletOutput("map", height = "600px", width = "100%")
      )
    ),

    # ───────────────────────────────────────────────────────────────────────────
    nav_panel(
      title = "Standings",
      value = "standings_tab",
      fluidRow(
        column(
          width = 6,
          div(
            h2("Group A", class = "country"),
            reactableOutput("tableA", width = "100%"),
            class = "table-container",
          )
        ),
        column(
          width = 6,
          div(
            h2("Group B", class = "country"),
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
            reactableOutput("tableC", width = "100%"),
            class = "table-container",
          )
        ),
        column(
          width = 6,
          div(
            h2("Group D", class = "country"),
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
      title = "Pill 5",
      value = "pill5_tab",
      "Content for Pill 5…"
    ),
    nav_spacer(),
    # ───────────────────────────────────────────────────────────────────────────
    nav_menu(
      title = "Other links",
      nav_panel("Panel D", "Panel D content…"),
      "----",
      "Description:",
      nav_item(
        a("Shiny website", href = "https://shiny.posit.co", target = "_blank")
      )
    )
  ) # end navset_pill()
) # end page_fluid()
