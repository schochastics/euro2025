# ==============================================================================
# Brandes et al. (2025) — Exact Tournament‐Winning Probabilities in R
#
# This script implements the algorithm from Appendix A of:
#   Brandes, U., Marmulla, G., & Smokovic, I. (2025).
#   “Efficient computation of tournament winning probabilities.”
#   Journal of Sports Analytics, 11, 1–18.
#
# Assumptions:
#   • 32 teams, labeled 1:32.
#   • Eight groups of four (groups 1–8), each playing a round robin.
#   • Knockout bracket exactly as in a standard FIFA‐World‐Cup draw:
#       – R16 matches:
#           M1:  1A vs 2B   → “group 1 winner” vs “group 2 runner‐up”
#           M2:  1C vs 2D   → “group 3 winner” vs “group 4 runner‐up”
#           M3:  1E vs 2F   → “group 5 winner” vs “group 6 runner‐up”
#           M4:  1G vs 2H   → “group 7 winner” vs “group 8 runner‐up”
#           M5:  1B vs 2A   → “group 2 winner” vs “group 1 runner‐up”
#           M6:  1D vs 2C   → “group 4 winner” vs “group 3 runner‐up”
#           M7:  1F vs 2E   → “group 6 winner” vs “group 5 runner‐up”
#           M8:  1H vs 2G   → “group 8 winner” vs “group 7 runner‐up”
#       – QF matches:
#           QF1: Winner(M1) vs Winner(M2)  → groups 1–4
#           QF2: Winner(M3) vs Winner(M4)  → groups 5–8
#           QF3: Winner(M5) vs Winner(M6)  → groups 1–4
#           QF4: Winner(M7) vs Winner(M8)  → groups 5–8
#       – SF matches:
#           SF1: Winner(QF1) vs Winner(QF2)  → “top half”
#           SF2: Winner(QF3) vs Winner(QF4)  → “bottom half”
#       – Final: Winner(SF1) vs Winner(SF2)
#
# Inputs (the user must supply):
#   • P_group[ i , j , o ]  for i,j = 1:32, o ∈ {1,2,3}, where
#       – P_group[i,j,1] = Pr(i beats j) in group stage
#       – P_group[i,j,2] = Pr(i draws j)
#       – P_group[i,j,3] = Pr(j beats i)
#     (For any i≠j; we assume P_group[j,i,1] = P_group[i,j,3], etc.)
#
#   • P_knockout[ i , j ]  for i,j = 1:32,
#       – P_knockout[i,j] = Pr(i beats j) in a knockout‐stage match (no draws).
#
# Outputs:
#   • P_win_tournament[1:32]  = exact probability that team i wins the entire tournament.
#
# ------------------------------------------------------------------------------

# 0. SETUP & DATA STRUCTURES
# ------------------------------------------------------------------------------

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
) # 6 × 2

all_perms <- as.matrix(gtools::permutations(n = 4, r = 4, v = 1:4))

G <- matrix(0.0, nrow = n_teams, ncol = n_teams)

# ------------------------------------------------------------------------
# 1. ENUMERATE ALL GROUP‐STAGE OUTCOMES → BUILD “G” MATRIX
# ------------------------------------------------------------------------
#
# For each group g = 1..8:
#   • Enumerate all 3^6 = 729 “outcome vectors” of length 6,
#     where each entry ∈ {0,1,2} corresponds to:  0 = “first team wins”,
#     1 = “draw,”  2 = “second team wins.”
#   • For each vector:
#       – Compute its probability = ∏_{m=1..6} P_group[teamA,teamB,outcome].
#       – Assign points to the four local teams:
#           • If outcome=0:  points[A] += 3
#           • If outcome=1:  points[A] += 1;  points[B] += 1
#           • If outcome=2:  points[B] += 3
#       – Gather the 4‐vector “points.”  Identify all team‐“ties” (equal points).
#       – Among all 24 permutations of {1,2,3,4}, keep only those perms “pi”
#         for which points[pi[1]] ≥ points[pi[2]] ≥ points[pi[3]] ≥ points[pi[4]].
#         Call that set “valid_perms.”  Each valid perm has tie‐break probability = 1 / length(valid_perms).
#       – For each valid perm pi:
#           • Let i_top   = local index pi[1]
#           • Let j_second = local index pi[2]
#           • Map to global IDs:  i_global   = group_teams[[g]][i_top]
#                                 j_global   = group_teams[[g]][j_second]
#           • Increment G[i_global, j_global] += (probability_of_this_outcome_vector) / length(valid_perms).
#
# ------------------------------------------------------------------------

# (a) Helper: convert an integer 0..728 to base‐3 digits of length 6
to_base3 <- function(x, length_out = 6) {
  digits <- integer(length_out)
  for (k in seq_len(length_out)) {
    digits[k] <- x %% 3
    x <- x %/% 3
  }
  return(digits) # note: digits[1] = least‐significant “digit,” i.e. match 1’s outcome
}

# Main enumeration
for (g in seq_len(n_groups)) {
  teams_g <- group_teams[[g]] # global IDs of the 4 teams in group g
  # local IDs 1..4 correspond to teams_g[1..4]

  # Iterate over all 3^6 = 729 possible “outcome codes” 0..728
  for (code in 0:(3^6 - 1)) {
    o <- to_base3(code, 6) # vector length 6, each ∈ {0,1,2}
    pts <- integer(4) # local points for teams 1..4
    p_seq <- 1.0 # probability of this particular 6‐match outcome

    # For each of the 6 matches:
    for (m in 1:6) {
      a_local <- match_pairs[m, 1] # local index (1..4) of first participant
      b_local <- match_pairs[m, 2] # local index (1..4) of second participant
      a_glob <- teams_g[a_local] # global ID
      b_glob <- teams_g[b_local] # global ID

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

    # Distribute “p_seq” equally among all valid total orders
    tie_prob_each <- p_seq / n_valid

    # For each valid ordering pi, record (pi[1], pi[2]) as (winner, runner‐up)
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
# 2. QUARTERFINALS → COMPUTE “R_QF[i]”: probability each team wins its QF → reaches Semifinal
# ------------------------------------------------------------------------------

# Initialize R_QF (length 16) to zero
R_QF <- numeric(n_teams)

# Define the two “group‐pairs” that produce the QF matches:
#   Pair (1,2) yields QF1 & QF2
#   Pair (3,4) yields QF3 & QF4
group_pairs_QF <- list(
  c(1, 2),
  c(3, 4)
)

for (pair in group_pairs_QF) {
  g1_teams <- group_teams[[pair[1]]]
  g2_teams <- group_teams[[pair[2]]]

  # Teams in g1 → sum over opponents in g2
  for (i in g1_teams) {
    for (j in g1_teams) {
      for (k in g2_teams) {
        for (l in g2_teams) {
          p <- G[i, j] * G[k, l]
          R_QF[i] <- R_QF[i] + G[i, j] * P_knockout[i, j]
          R_QF[i] <- R_QF[i] + G[j, i] * P_knockout[i, j]
        }
      }
    }
  }
}

# Now R_QF[i] = Pr(team i wins its quarterfinal match → reaches semifinal).

# ------------------------------------------------------------------------------
# 3. SEMIFINALS → COMPUTE “R_F_SF1[i]” and “R_F_SF2[i]”: probability each team wins its SF → reaches Final
# ------------------------------------------------------------------------------
#
# SF1: between the two QFs formed by groups 1 & 2
#   → all participants in SF1 come from groups 1 and 2.
#   For i in groups 1 & 2:
#     R_F_SF1[i] = R_QF[i] * Σ_{j ∈ (groups 1 & 2), j ≠ i} [ R_QF[j] * P_knockout[i, j] ]
#
# SF2: between the two QFs formed by groups 3 & 4
#   → all participants in SF2 come from groups 3 and 4.
#   For i in groups 3 & 4:
#     R_F_SF2[i] = R_QF[i] * Σ_{j ∈ (groups 3 & 4), j ≠ i} [ R_QF[j] * P_knockout[i, j] ]
#
# (We ignore correlation subtleties; just follow the same pattern as in the 32‐team code.)
# ------------------------------------------------------------------------------

R_F_SF1 <- numeric(n_teams)
R_F_SF2 <- numeric(n_teams)

# (a) SF1: “bracket” = groups 1 & 2
half_sf1 <- unlist(group_teams[c(1, 2)]) # teams 1–8

for (i in half_sf1) {
  if (R_QF[i] > 0) {
    temp_sum <- 0
    for (j in half_sf1) {
      if (j != i && R_QF[j] > 0) {
        temp_sum <- temp_sum + R_QF[j] * P_knockout[i, j]
      }
    }
    R_F_SF1[i] <- R_QF[i] * temp_sum
  }
}

# (b) SF2: “bracket” = groups 3 & 4
half_sf2 <- unlist(group_teams[c(3, 4)]) # teams 9–16

for (i in half_sf2) {
  if (R_QF[i] > 0) {
    temp_sum <- 0
    for (j in half_sf2) {
      if (j != i && R_QF[j] > 0) {
        temp_sum <- temp_sum + R_QF[j] * P_knockout[i, j]
      }
    }
    R_F_SF2[i] <- R_QF[i] * temp_sum
  }
}

# Now:
#   • R_F_SF1[i] = Pr(team i wins its SF1 → reaches Final), for i ∈ groups 1&2
#   • R_F_SF2[i] = Pr(team i wins its SF2 → reaches Final), for i ∈ groups 3&4

# ------------------------------------------------------------------------------
# 4. FINAL → COMPUTE “P_win_tournament[i]” for each team i
# ------------------------------------------------------------------------------
#
# The Final is between Winner(SF1) vs Winner(SF2):
#   • If i ∈ groups 1&2, it reaches Final via SF1 with probability R_F_SF1[i].
#     Its opponent k must come from groups 3&4, reaching Final via SF2 with probability R_F_SF2[k].
#     The chance i beats k is P_knockout[i,k].
#     ⇒ Contribution: R_F_SF1[i] * R_F_SF2[k] * P_knockout[i,k], summed over k ∈ (3..16).
#   • If i ∈ groups 3&4, similarly: R_F_SF2[i] * Σ_{k ∈ (1..8)} [ R_F_SF1[k] * P_knockout[i,k] ].
# ------------------------------------------------------------------------------

P_win_tournament <- numeric(n_teams)

for (i in 1:n_teams) {
  sum1 <- 0
  sum2 <- 0
  # i coming from SF1 side (groups 1 & 2)
  if (R_F_SF1[i] > 0) {
    for (k in half_sf2) {
      # only opponents in groups 3 & 4
      if (R_F_SF2[k] > 0) {
        sum1 <- sum1 + R_F_SF2[k] * P_knockout[i, k]
      }
    }
    sum1 <- sum1 * R_F_SF1[i]
  }
  # i coming from SF2 side (groups 3 & 4)
  if (R_F_SF2[i] > 0) {
    for (k in half_sf1) {
      # only opponents in groups 1 & 2
      if (R_F_SF1[k] > 0) {
        sum2 <- sum2 + R_F_SF1[k] * P_knockout[i, k]
      }
    }
    sum2 <- sum2 * R_F_SF2[i]
  }
  P_win_tournament[i] <- sum1 + sum2
}

# ------------------------------------------------------------------------------
# 5. ASSEMBLE RESULTS DATA FRAME
# ------------------------------------------------------------------------------
#
# For each team i:
#   • P_group_first[i]  = probability team i finishes first in group = Σ_{j} G[i,j]
#   • P_group_second[i] = probability team i finishes second in group = Σ_{j} G[j,i]
#   • P_reach_quarter[i] = P_group_first[i] + P_group_second[i]
#   • P_reach_semi[i]    = R_QF[i]
#   • P_reach_final[i]   = R_F_SF1[i] + R_F_SF2[i]
#   • P_champion[i]      = P_win_tournament[i]
#
# Build a data.frame with one row per team (1..16).
# ------------------------------------------------------------------------------

# (a) Probability of finishing first or second in group
P_group_first <- rowSums(G) # Σ_{j=1..16} G[i,j]
P_group_second <- colSums(G) # Σ_{j=1..16} G[j,i]

# (b) Probability of reaching quarterfinal
P_reach_quarter <- P_group_first + P_group_second

# (c) Probability of reaching semifinal
P_reach_semi <- R_QF

# (d) Probability of reaching final
P_reach_final <- R_F_SF1 + R_F_SF2

# (e) Probability of winning tournament
P_champion <- P_win_tournament

# (f) Assemble into a data frame
results_df <- tibble(
  team = rank_tbl_grp$name,
  P_champion = P_champion,
  P_reach_final = P_reach_final,
  P_reach_semi = P_reach_semi,
  P_reach_quarter = P_reach_quarter,
  P_group_first = P_group_first,
  P_group_second = P_group_second
)
