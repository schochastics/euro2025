library(tidyverse)

flags <- jsonlite::fromJSON("https://flagcdn.com/en/codes.json") |>
  as_tibble() |>
  pivot_longer(
    cols = everything(),
    names_to = "iso2",
    values_to = "country"
  )

saveRDS(flags, "app/data/flags.rds")
