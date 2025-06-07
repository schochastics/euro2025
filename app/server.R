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
  })

  output$tableA <- renderReactable({
    make_table(standings_complete, "A")
  })

  output$tableB <- renderReactable({
    make_table(standings_complete, "B")
  })

  output$tableC <- renderReactable({
    make_table(standings_complete, "C")
  })

  output$tableD <- renderReactable({
    make_table(standings_complete, "D")
  })

  output$fifa_ranking <- renderReactable({
    make_fifa(fifa_ranking)
  })

  output$forecast <- renderReactable({
    make_forecast(forecast)
  })
}
