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
      # Panel 1: Teams (flag grid in content area)
      nav_panel(
        title = "Teams",
        value = "teams_tab",
        layout_sidebar(
          uiOutput("country_ui"),
          sidebar = sidebar(
            width = 200,
            tags$div(
              class = "flag-grid",
              actionButton(
                inputId = "BEL",
                label = HTML(
                  "<img src='flags/BEL.png' height='40px'/>"
                ),
                class = "btn-flag"
              ),
              actionButton(
                inputId = "DEN",
                label = HTML(
                  "<img src='flags/DEN.png' height='40px'/>"
                ),
                class = "btn-flag"
              ),
              actionButton(
                inputId = "ENG",
                label = HTML(
                  "<img src='flags/ENG.png' height='40px'/>"
                ),
                class = "btn-flag"
              ),
              actionButton(
                inputId = "ESP",
                label = HTML(
                  "<img src='flags/ESP.png' height='40px'/>"
                ),
                class = "btn-flag"
              ),
              actionButton(
                inputId = "FIN",
                label = HTML(
                  "<img src='flags/FIN.png' height='40px'/>"
                ),
                class = "btn-flag"
              ),
              actionButton(
                inputId = "FRA",
                label = HTML(
                  "<img src='flags/FRA.png' height='40px'/>"
                ),
                class = "btn-flag"
              ),
              actionButton(
                inputId = "GER",
                label = HTML(
                  "<img src='flags/GER.png' height='40px'/>"
                ),
                class = "btn-flag"
              ),
              actionButton(
                inputId = "ISL",
                label = HTML(
                  "<img src='flags/ISL.png' height='40px'/>"
                ),
                class = "btn-flag"
              ),
              actionButton(
                inputId = "ITA",
                label = HTML(
                  "<img src='flags/ITA.png' height='40px'/>"
                ),
                class = "btn-flag"
              ),
              actionButton(
                inputId = "NED",
                label = HTML(
                  "<img src='flags/NED.png' height='40px'/>"
                ),
                class = "btn-flag"
              ),
              actionButton(
                inputId = "NOR",
                label = HTML(
                  "<img src='flags/NOR.png' height='40px'/>"
                ),
                class = "btn-flag"
              ),
              actionButton(
                inputId = "POL",
                label = HTML(
                  "<img src='flags/POL.png' height='40px'/>"
                ),
                class = "btn-flag"
              ),
              actionButton(
                inputId = "POR",
                label = HTML(
                  "<img src='flags/POR.png' height='40px'/>"
                ),
                class = "btn-flag"
              ),
              actionButton(
                inputId = "SWE",
                label = HTML(
                  "<img src='flags/SWE.png' height='40px'/>"
                ),
                class = "btn-flag"
              ),
              actionButton(
                inputId = "SUI",
                label = HTML(
                  "<img src='flags/SUI.png' height='40px'/>"
                ),
                class = "btn-flag"
              ),
              actionButton(
                inputId = "WAL",
                label = HTML(
                  "<img src='flags/WAL.png' height='40px'/>"
                ),
                class = "btn-flag"
              )
            )
          )
        )
      ),

      # Panel 2: Placeholder
      nav_panel(
        title = "Pill 2",
        value = "pill2_tab",
        "Content for Pill 2…"
      ),

      # Panel 3: Placeholder
      nav_panel(
        title = "Pill 3",
        value = "pill3_tab",
        "Content for Pill 3…"
      ),

      # Panel 4: Placeholder
      nav_panel(
        title = "Pill 4",
        value = "pill4_tab",
        "Content for Pill 4…"
      ),

      # Panel 5: Placeholder
      nav_panel(
        title = "Pill 5",
        value = "pill5_tab",
        "Content for Pill 5…"
      ),

      # Example of a nav_menu at the end
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
