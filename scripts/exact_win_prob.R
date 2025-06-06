source("scripts/forecast.R")
n_teams <- 16
teams_per_group <- 4
n_groups <- n_teams / teams_per_group # = 8

group_teams <- lapply(1:n_groups, function(g) {
  seq(from = (g - 1) * teams_per_group + 1, to = g * teams_per_group)
})

match_pairs <- matrix(
  c(1, 2, 1, 3, 1, 4, 2, 3, 2, 4, 3, 4),
  ncol = 2,
  byrow = TRUE
)

all_perms <- as.matrix(gtools::permutations(n = 4, r = 4, v = 1:4))

G <- matrix(0.0, nrow = n_teams, ncol = n_teams)

# ------------------------------------------------------------------------
# 1. ENUMERATE ALL GROUP‐STAGE OUTCOMES → BUILD “G” MATRIX
# ------------------------------------------------------------------------

to_base3 <- function(x, length_out = 6) {
  digits <- integer(length_out)
  for (k in seq_len(length_out)) {
    digits[k] <- x %% 3
    x <- x %/% 3
  }
  return(digits)
}


for (g in seq_len(n_groups)) {
  teams_g <- group_teams[[g]]

  for (code in 0:(3^6 - 1)) {
    o <- to_base3(code, 6)
    pts <- integer(4)
    p_seq <- 1.0

    for (m in 1:6) {
      a_local <- match_pairs[m, 1]
      b_local <- match_pairs[m, 2]
      a_glob <- teams_g[a_local]
      b_glob <- teams_g[b_local]

      if (o[m] == 0) {
        # “first team wins”
        pts[a_local] <- pts[a_local] + 3
        p_seq <- p_seq * P_group[a_glob, b_glob, 1]
      } else if (o[m] == 1) {
        # “draw”
        pts[a_local] <- pts[a_local] + 1
        pts[b_local] <- pts[b_local] + 1
        p_seq <- p_seq * P_group[a_glob, b_glob, 2]
      } else {
        # “second team wins”
        pts[b_local] <- pts[b_local] + 3
        p_seq <- p_seq * P_group[a_glob, b_glob, 3]
      }
    }

    # If this outcome‐probability is zero (some P_group was zero), skip tie‐break logic
    if (p_seq == 0) {
      next
    }

    # Among all 24 permutations of {1,2,3,4}, keep only those with nonincreasing points
    valid_idx <- logical(nrow(all_perms))
    for (r in seq_len(nrow(all_perms))) {
      perm <- all_perms[r, ]
      ok <- TRUE
      for (k in 1:3) {
        if (pts[perm[k]] < pts[perm[k + 1]]) {
          ok <- FALSE
          break
        }
      }
      valid_idx[r] <- ok
    }
    valid_perms <- all_perms[valid_idx, , drop = FALSE]
    n_valid <- nrow(valid_perms)

    tie_prob_each <- p_seq / n_valid

    for (r in seq_len(n_valid)) {
      pi <- valid_perms[r, ]
      i_top <- pi[1]
      j_second <- pi[2]
      i_glob <- teams_g[i_top]
      j_glob <- teams_g[j_second]
      G[i_glob, j_glob] <- G[i_glob, j_glob] + tie_prob_each
    }
  }
}

# At this point, G[i,j] > 0 if and only if i and j are in the SAME group.
#   For each group g, ∑_{i,j∈group g, i≠j} G[i,j] = 1
#   (i.e. exactly one winner & runner‐up must emerge).

# ------------------------------------------------------------------------------
# 2. QUARTERFINAL JOINT PROBABILITIES → “Q” matrix (16×16)
#
# Build Q[i,j] = Pr(team i and team j both win their QF matches → both reach the Semifinals).
# ------------------------------------------------------------------------------

Q <- matrix(0.0, nrow = n_teams, ncol = n_teams)

g1_teams <- group_teams[[1]]
g2_teams <- group_teams[[2]]

for (i in g1_teams) {
  for (k in g2_teams) {
    for (j in setdiff(g1_teams, i)) {
      for (l in setdiff(g2_teams, k)) {
        p <- G[i, j] * G[k, l]
        Q[i, j] <- Q[i, j] + p * (P_knockout[i, k] * P_knockout[j, l])
        Q[i, l] <- Q[i, l] + p * (P_knockout[i, k] * P_knockout[l, j])
        Q[k, j] <- Q[k, j] + p * (P_knockout[k, i] * P_knockout[j, l])
        Q[k, l] <- Q[k, l] + p * (P_knockout[k, i] * P_knockout[l, j])
      }
    }
  }
}

g3_teams <- group_teams[[3]]
g4_teams <- group_teams[[4]]

for (i in g3_teams) {
  for (k in g4_teams) {
    for (j in setdiff(g3_teams, i)) {
      for (l in setdiff(g4_teams, j)) {
        p <- G[i, j] * G[k, l]
        Q[i, j] <- Q[i, j] + p * (P_knockout[i, k] * P_knockout[j, l])
        Q[i, l] <- Q[i, l] + p * (P_knockout[i, k] * P_knockout[l, j])
        Q[k, j] <- Q[k, j] + p * (P_knockout[k, i] * P_knockout[j, l])
        Q[k, l] <- Q[k, l] + p * (P_knockout[k, i] * P_knockout[l, j])
      }
    }
  }
}

SF <- rep(0.0, n_teams)

s1_teams <- unlist(group_teams[1:2])
s2_teams <- unlist(group_teams[3:4])

for (i in s1_teams) {
  for (j in s1_teams) {
    SF[i] <- SF[i] + Q[i, j] * P_knockout[i, j]
    SF[j] <- SF[j] + Q[i, j] * P_knockout[j, i]
  }
}
for (i in s2_teams) {
  for (j in s2_teams) {
    SF[i] <- SF[i] + Q[i, j] * P_knockout[i, j]
    SF[j] <- SF[j] + Q[i, j] * P_knockout[j, i]
  }
}

WF <- rep(0.0, n_teams)
for (i in s1_teams) {
  for (j in s2_teams) {
    WF[i] <- WF[i] + SF[i] * SF[j] * P_knockout[i, j]
    WF[j] <- WF[j] + SF[i] * SF[j] * P_knockout[j, i]
  }
}

WF

tibble(
  team = rank_tbl_grp$name,
  winner = WF,
  final = SF,
  semi = rowSums(Q),
  group_first = rowSums(G),
  group_second = colSums(G)
) |>
  reactable::reactable(
    defaultPageSize = 16,
    columns = list(
      team = reactable::colDef(name = "Team"),
      winner = reactable::colDef(
        name = "Win Tournament",
        format = reactable::colFormat(digits = 4)
      ),
      final = reactable::colDef(
        name = "Reach Final",
        format = reactable::colFormat(digits = 4)
      ),
      semi = reactable::colDef(
        name = "Reach Semifinal",
        format = reactable::colFormat(digits = 4)
      ),
      group_first = reactable::colDef(
        name = "Group Winner",
        format = reactable::colFormat(digits = 4)
      ),
      group_second = reactable::colDef(
        name = "Group Runner-up",
        format = reactable::colFormat(digits = 4)
      )
    )
  )
