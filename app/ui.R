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
        "Test"
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
