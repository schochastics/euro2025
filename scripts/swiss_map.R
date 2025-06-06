library(leaflet)
library(sf)
library(rnaturalearth)

# hotels <- readr::read_csv("app/data/hotels.csv")
# hotels <- hotels  |> 
#   mutate(full_address = paste(Hotel, HotelCity, sep = ", "))

# geocoded_hotels <- hotels %>%
#   tidygeocoder::geocode(address = full_address, method = 'osm', lat = latitude, long = longitude)

# write_csv(geocoded_hotels, "app/data/geocoded_hotels.csv")

switzerland_sf <- ne_countries(
  scale = "large",
  country = "Switzerland",
  returnclass = "sf"
)

saveRDS(switzerland_sf, "app/data/switzerland_sf.rds")

hotels <- readr::read_csv("app/data/geocoded_hotels.csv")
stadiums <- readr::read_csv("app/data/stadiums.csv")
hotels$flag_url <- paste0("https://flagcdn.com/w20/", hotels$iso2, ".png")
switzerland_sf <- readRDS("app/data/switzerland_sf.rds")

flag_icons <- makeIcon(
  iconUrl    = hotels$flag_url,
  iconWidth  = 20,  
  iconHeight = 15
)

logo <- makeIcon(
  iconUrl = "https://upload.wikimedia.org/wikipedia/commons/e/e7/UEFA_Women%27s_Euro_Switzerland_2025_Logo.png",
  iconWidth = 30 / 2,
  iconHeight = 36 / 2
)

leaflet(
  data = switzerland_sf
) |>
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
  setView(lng = 8.2275, lat = 46.8182, zoom = 7) |>
  addPolygons(
    color = "#444444",
    weight = 1,
    fillColor = "#ff9e33",
    fillOpacity = 0.25
  )
