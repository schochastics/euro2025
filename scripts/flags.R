
library(tidyverse)
flags <- tibble(
  url = c(
    "https://img.uefa.com/imgml/flags/64x64/BEL.png",
    "https://img.uefa.com/imgml/flags/64x64/DEN.png",
    "https://img.uefa.com/imgml/flags/64x64/ENG.png",
    "https://img.uefa.com/imgml/flags/64x64/FRA.png",
    "https://img.uefa.com/imgml/flags/64x64/FIN.png",
    "https://img.uefa.com/imgml/flags/64x64/GER.png",
    "https://img.uefa.com/imgml/flags/64x64/ISL.png",
    "https://img.uefa.com/imgml/flags/64x64/ITA.png",
    "https://img.uefa.com/imgml/flags/64x64/NED.png",
    "https://img.uefa.com/imgml/flags/64x64/NOR.png",
    "https://img.uefa.com/imgml/flags/64x64/POL.png",
    "https://img.uefa.com/imgml/flags/64x64/POR.png",
    "https://img.uefa.com/imgml/flags/64x64/ESP.png",
    "https://img.uefa.com/imgml/flags/64x64/SWE.png",
    "https://img.uefa.com/imgml/flags/64x64/SUI.png",
    "https://img.uefa.com/imgml/flags/64x64/WAL.png"
  ),
  country = c(
    "Belgium",
    "Denmark",
    "England",
    "France",
    "Finland",
    "Germany",
    "Iceland",
    "Italy",
    "Netherlands",
    "Norway",
    "Poland",
    "Portugal",
    "Spain",
    "Sweden",
    "Switzerland",
    "Wales"
  ),
  short = c(
    "BEL",
    "DEN",
    "ENG",
    "FRA",
    "FIN",
    "GER",
    "ISL",
    "ITA",
    "NED",
    "NOR",
    "POL",
    "POR",
    "ESP",
    "SWE",
    "SUI",
    "WAL"
  )
)

map(flags$url, \(x)
  {
    if (!file.exists(paste0("app/www/flags/", basename(x)))) {
      download.file(x, destfile = paste0("app/www/flags/", basename(x)), mode = "wb")
    }
  }
)
