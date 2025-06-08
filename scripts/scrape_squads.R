library(rvest)

url <- "https://www.worldfootball.net/players/frauen-em-2022-england/"

team_urls <- read_html(url) |>
  html_elements("td:nth-child(6) a") |>
  html_attr("href")

team_urls <- paste0("https://www.worldfootball.net", team_urls)

players <- team_urls |>
  map_dfr(function(url) {
    doc <- read_html(url)
    df <- doc |>
      html_elements(".data .standard_tabelle") |>
      html_table() |>
      _[[1]]
    names(df) <- c(
      "not1",
      "Number",
      "Name",
      "not2",
      "Team",
      "Birthdate",
      "not3"
    )
    df |>
      mutate(
        Country = doc |>
          html_element("h1") |>
          html_text()
      )
  })

players <- players |>
  mutate(
    Country = str_remove(Country, "\\s+\\[.*")
  ) |>
  dplyr::filter(not1 == "") |>
  select(-contains("not")) |>
  mutate(Birthdate = as.Date(Birthdate, format = "%d/%m/%Y"))
