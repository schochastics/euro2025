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

        # FLAG GRID (15 flags + captions):
        div(
          class = "flag-row",

          # Belgium
          div(
            class = "flag-item",
            tags$img(src = "flags/BEL.png", alt = "Belgium"),
            div(class = "caption", "Belgium")
          ),
          # Denmark
          div(
            class = "flag-item",
            tags$img(src = "flags/DEN.png", alt = "Denmark"),
            div(class = "caption", "Denmark")
          ),
          # England
          div(
            class = "flag-item",
            tags$img(src = "flags/ENG.png", alt = "England"),
            div(class = "caption", "England")
          ),
          # France
          div(
            class = "flag-item",
            tags$img(src = "flags/FRA.png", alt = "France"),
            div(class = "caption", "France")
          ),
          # Finland
          div(
            class = "flag-item",
            tags$img(src = "flags/FIN.png", alt = "Finland"),
            div(class = "caption", "Finland")
          ),
          # Germany
          div(
            class = "flag-item",
            tags$img(src = "flags/GER.png", alt = "Germany"),
            div(class = "caption", "Germany")
          ),
          # Iceland
          div(
            class = "flag-item",
            tags$img(src = "flags/ISL.png", alt = "Iceland"),
            div(class = "caption", "Iceland")
          ),
          # Italy
          div(
            class = "flag-item",
            tags$img(src = "flags/ITA.png", alt = "Italy"),
            div(class = "caption", "Italy")
          ),
          # Netherlands
          div(
            class = "flag-item",
            tags$img(src = "flags/NED.png", alt = "Netherlands"),
            div(class = "caption", "Netherlands")
          ),
          # Norway
          div(
            class = "flag-item",
            tags$img(src = "flags/NOR.png", alt = "Norway"),
            div(class = "caption", "Norway")
          ),
          # Portugal
          div(
            class = "flag-item",
            tags$img(src = "flags/POR.png", alt = "Portugal"),
            div(class = "caption", "Portugal")
          ),
          # Spain
          div(
            class = "flag-item",
            tags$img(src = "flags/ESP.png", alt = "Spain"),
            div(class = "caption", "Spain")
          ),
          # Sweden
          div(
            class = "flag-item",
            tags$img(src = "flags/SWE.png", alt = "Sweden"),
            div(class = "caption", "Sweden")
          ),
          # Switzerland
          div(
            class = "flag-item",
            tags$img(src = "flags/SUI.png", alt = "Switzerland"),
            div(class = "caption", "Switzerland")
          ),
          # Wales
          div(
            class = "flag-item",
            tags$img(src = "flags/WAL.png", alt = "Wales"),
            div(class = "caption", "Wales")
          )
        ),

        # Optional separator + placeholder text
        tags$hr(),
        tags$p("Click a flag above for more details… (placeholder)")
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
