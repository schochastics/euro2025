library(shiny)
library(bslib)

# 1. Define a Bootstrap 5 theme via bslib
my_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly"
)

# 2. UI
ui <- fluidPage(
  theme = my_theme,

  # 3. Include external CSS from www/styles.css
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  # 4. Tabset with pill‐style navigation and five placeholders
  tabsetPanel(
    id = "pill_nav",
    type = "pills",

    tabPanel(
      title = "Pill 1",
      value = "pill1"
      # (no content yet)
    ),

    tabPanel(
      title = "Pill 2",
      value = "pill2"
      # (no content yet)
    ),

    tabPanel(
      title = "Pill 3",
      value = "pill3"
      # (no content yet)
    ),

    tabPanel(
      title = "Pill 4",
      value = "pill4"
      # (no content yet)
    ),

    tabPanel(
      title = "Pill 5",
      value = "pill5"
      # (no content yet)
    )
  )
)