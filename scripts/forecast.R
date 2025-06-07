library(tidyverse)

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
)

group <- c(2, 3, 4, 4, 1, 3, 1, 2, 4, 1, 3, 2, 2, 3, 1, 4)

rank_tbl <- jsonlite::fromJSON(
  "https://inside.fifa.com/api/ranking-overview?locale=en&dateId=ranking_20250306&rankingType=football"
)[[1]][[1]] |>
  select(rank, name, totalPoints) |>
  dplyr::filter(name %in% country) |>
  mutate(
    group = group[match(name, country)]
  )

rank_tbl_grp <- rank_tbl |>
  arrange(group, name)

prob <- function(pts_i, pts_j) {
  h <- 1.0
  delta <- 1
  rho_i <- 1 / (1 + 10^((pts_j - pts_i) / 200))
  rho_j <- 1 - rho_i

  win_j <- rho_j / (h * rho_i + delta * sqrt(rho_i * rho_j) + rho_j)
  draw <- delta *
    sqrt(rho_i * rho_j) /
    (h * rho_i + delta * sqrt(rho_i * rho_j) + rho_j)
  win_i <- h * rho_i / (h * rho_i + delta * sqrt(rho_i * rho_j) + rho_j)
  c(win_i, draw, win_j)
}

# Calculate probabilities for each matchup
probs_long <- expand_grid(
  Team1 = rank_tbl_grp$name,
  Team2 = rank_tbl_grp$name
) |>
  left_join(rank_tbl_grp, by = c("Team1" = "name")) |>
  left_join(rank_tbl_grp, by = c("Team2" = "name"), suffix = c("_i", "_j")) |>
  select(-contains(c("rank", "group"))) |>
  mutate(
    prob = map2(totalPoints_i, totalPoints_j, prob),
    win_i = map_dbl(prob, ~ .x[1]),
    draw = map_dbl(prob, ~ .x[2]),
    win_j = map_dbl(prob, ~ .x[3])
  )

P <- array(
  0,
  dim = c(16, 16, 3),
  dimnames = list(
    Team1 = rank_tbl_grp$name,
    Team2 = rank_tbl_grp$name,
    c("win_i", "draw", "win_j")
  )
)
P[,, 1] <- probs_long |>
  select(Team1, Team2, win_i) |>
  pivot_wider(names_from = Team2, values_from = win_i) |>
  column_to_rownames("Team1") |>
  as.data.frame() |>
  as.matrix()

P[,, 2] <- probs_long |>
  select(Team1, Team2, draw) |>
  pivot_wider(names_from = Team2, values_from = draw) |>
  column_to_rownames("Team1") |>
  as.data.frame() |>
  as.matrix()

P[,, 3] <- probs_long |>
  select(Team1, Team2, win_j) |>
  pivot_wider(names_from = Team2, values_from = win_j) |>
  column_to_rownames("Team1") |>
  as.data.frame() |>
  as.matrix()

P_group <- P
P_knockout <- matrix(
  0,
  16,
  16,
  dimnames = list(
    Team1 = rank_tbl_grp$name,
    Team2 = rank_tbl_grp$name
  )
)
P_knockout <- P_group[,, 1] + 0.5 * P_group[,, 2]


saveRDS(rank_tbl_grp, "app/data/fifa_ranking.rds")
