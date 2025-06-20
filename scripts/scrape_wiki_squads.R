library(rvest)
library(dplyr)
library(purrr)

url <- "https://en.wikipedia.org/wiki/UEFA_Women%27s_Euro_2025_squads"
page <- read_html(url)

nodes <- page |>
  html_elements(xpath = '//*[(self::h3 or self::table)]')

squad_tables <- list()
current_country <- NULL

for (node in nodes) {
  tag <- html_name(node)

  # If it's a heading, update current_country
  if (tag %in% c("h3")) {
    current_country <- node |> html_text2()
    next
  }

  if (tag == "table" && !is.null(current_country)) {
    table <- tryCatch(
      {
        html_table(node, fill = TRUE)
      },
      error = function(e) NULL
    )

    if (!is.null(table) && nrow(table) > 1) {
      table$Country <- current_country
      table <- table |>
        janitor::clean_names() |>
        mutate(
          birthday = stringr::str_extract(
            date_of_birth_age,
            "(?<=\\()[^()]+(?=\\))"
          )
        ) |>
        select(pos, player, birthday, caps, goals, club, country)

      squad_tables[[length(squad_tables) + 1]] <- table
    }
  }
}

squad_tables[[1]]

# Combine all tables into one
all_squads <- bind_rows(squad_tables)
all_squads |>
  mutate(
    srt = minty::parse_number(pos),
    pos = stringr::str_extract(pos, "[A-Z]+")
  ) |>
  arrange(country, srt) |>
  select(-srt) |>
  saveRDS("app/data/squads.rds")
