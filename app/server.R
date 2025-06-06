# server.R

library(shiny)

server <- function(input, output) {
  v <- reactiveValues(country = "Belgium")

  observeEvent(input$BEL, {
    v$country <- "Belgium"
  })
  observeEvent(input$DEN, {
    v$country <- "Denmark"
  })
  observeEvent(input$ENG, {
    v$country <- "England"
  })
  observeEvent(input$ESP, {
    v$country <- "Spain"
  })
  observeEvent(input$FIN, {
    v$country <- "Finland"
  })
  observeEvent(input$FRA, {
    v$country <- "France"
  })
  observeEvent(input$GER, {
    v$country <- "Germany"
  })
  observeEvent(input$ISL, {
    v$country <- "Iceland"
  })
  observeEvent(input$ITA, {
    v$country <- "Italy"
  })
  observeEvent(input$NED, {
    v$country <- "Netherlands"
  })
  observeEvent(input$NOR, {
    v$country <- "Norway"
  })
  observeEvent(input$POL, {
    v$country <- "Poland"
  })
  observeEvent(input$POR, {
    v$country <- "Portugal"
  })
  observeEvent(input$SWE, {
    v$country <- "Sweden"
  })
  observeEvent(input$SUI, {
    v$country <- "Switzerland"
  })
  observeEvent(input$WAL, {
    v$country <- "Wales"
  })

  output$country_ui <- renderUI({
    switch(
      v$country,
      "Belgium" = h2("Belgium", class = "country"),
      "Denmark" = h2("Denmark", class = "country"),
      "England" = h2("England", class = "country"),
      "France" = h2("France", class = "country"),
      "Finland" = h2("Finland", class = "country"),
      "Germany" = h2("Germany", class = "country"),
      "Iceland" = h2("Iceland", class = "country"),
      "Italy" = h2("Italy", class = "country"),
      "Netherlands" = h2("Netherlands", class = "country"),
      "Norway" = h2("Norway", class = "country"),
      "Poland" = h2("Poland", class = "country"),
      "Portugal" = h2("Portugal", class = "country"),
      "Spain" = h2("Spain", class = "country"),
      "Sweden" = h2("Sweden", class = "country"),
      "Switzerland" = h2("Switzerland", class = "country"),
      "Wales" = h2("Wales", class = "country")
    )
  })

  output$map <- renderLeaflet({
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
          "<strong>Hotel:</strong>",
          Hotel,
          "<br>",
          "<strong>City:</strong>",
          HotelCity
        ),
        icon = flag_icons
      ) |>
      setView(lng = 8.2275, lat = 46.8182, zoom = 9) |>
      addPolygons(
        color = "#444444",
        weight = 1,
        fillColor = "#ff9e33",
        fillOpacity = 0.25
      )
  })
}
