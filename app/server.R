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

  output$flag1 <- renderUI({
    code <- names(country_codes)[country_codes == input$country1]
    img(src = paste0("flags/", code, ".png"), height = "100px")
  })

  output$flag2 <- renderUI({
    code <- names(country_codes)[country_codes == input$country2]
    img(src = paste0("flags/", code, ".png"), height = "100px")
  })

  observeEvent(c(input$country1, input$country2), {
    req(input$country1, input$country2)
    country1 <- input$country1
    country2 <- input$country2

    head_to_head <- games |>
      dplyr::filter(
        (home_team == country1 & away_team == country2) |
          (home_team == country2 & away_team == country1)
      ) |>
      dplyr::mutate(
        country1_score = dplyr::if_else(
          home_team == country1,
          home_score,
          away_score
        ),
        country2_score = dplyr::if_else(
          home_team == country2,
          home_score,
          away_score
        ),
        result = dplyr::case_when(
          country1_score > country2_score ~ "W",
          country1_score < country2_score ~ "L",
          TRUE ~ "D"
        )
      )

    summary_stats <- head_to_head |>
      dplyr::summarise(
        Wins = sum(result == "W"),
        Draws = sum(result == "D"),
        Losses = sum(result == "L"),
        Goals_For = sum(country1_score),
        Goals_Against = sum(country2_score),
        Last_3_Results = paste0(rev(tail(result, 3)), collapse = "")
      )

    # Defensive check in case there's no data
    if (nrow(summary_stats) == 0) {
      output$stats_box <- renderUI({
        div("No matches found.", class = "stat-section")
      })
      return()
    }

    # Extract values
    wins <- summary_stats$Wins
    draws <- summary_stats$Draws
    losses <- summary_stats$Losses
    goals_text <- paste0(
      summary_stats$Goals_For,
      " : ",
      summary_stats$Goals_Against
    )
    results <- strsplit(summary_stats$Last_3_Results, "")[[1]]

    # Render UI
    output$stats_box <- renderUI({
      tagList(
        div(
          class = "stat-section",
          div(
            span("Wins", class = "circle-label"),
            span("Draws", class = "circle-label"),
            span("Losses", class = "circle-label")
          ),
          div(
            span(wins, class = "circle"),
            span(draws, class = "circle"),
            span(losses, class = "circle")
          ),
          tags$h3("Goal Difference", style = "margin-top: 20px;"),
          div(class = "goals-text", goals_text),
          tags$h3("Last 3 Matches", style = "margin-top: 20px;"),
          div(
            lapply(results, function(r) {
              span(r, class = paste("result-circle", paste0("result-", r)))
            })
          )
        )
      )
    })
  })

  output$all_time_appearance <- renderReactable({
    make_simple_table(player_appearance)
  })

  output$all_time_scorer <- renderReactable({
    make_simple_table(player_scorer)
  })
}
