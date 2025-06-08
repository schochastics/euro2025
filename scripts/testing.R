tibble(
  country = c("USA", "Canada", "Mexico"),
  value = c(10, 5, 8)
) |>
  reactable(
    columns = list(
      country = colDef(name = "Country"),
      value = colDef(
        name = "Value",
        cell = function(value, index) {
          style <- if (index == 1) {
            "font-size: 1.2em;" # Slightly larger
          } else {
            NULL
          }
          div(style = style, value)
        }
      )
    ),
    rowStyle = function(index) {
      if (index == 1) {
        list(color = "white", background = "#211431")
      } else {
        NULL
      }
    },
    theme = reactableTheme(
      tableStyle = list(
        border = "2px solid #ccc", # Stroke around the table
        borderRadius = "10px", # Rounded corners
        overflow = "hidden" # Ensures rounding is visible
      ),
      headerStyle = list(
        background = "#211431",
        color = "white",
        borderBottom = "none"
      )
    )
  )
