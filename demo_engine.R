# Demo of the generic engine on the level 5 Zealot from the Obsidian note.
# Run from the project root.
source("R/engine.R")

events <- list(
  damage_event("Greataxe",    n_dice = 1, die = 12, flat = 4 + 2, attacks = 2),
  damage_event("Divine Fury", n_dice = 1, die = 6,  flat = 2, once_per_turn = TRUE)
)

off <- round_dpr(events, attack_bonus = 7, target_ac = 15, roll_mode = "advantage")
cat("Offensive DPR:", round(off$total, 2), "\n")
print(off$breakdown)

foe <- enemy_attack("Longsword", n_attacks = 2, attack_bonus = 6, die = 8, flat = 4)
def <- damage_taken(foe, ac = ac_unarmored_defense(dex_mod = 2, con_mod = 3),
                    roll_mode = "advantage", resist = PHYSICAL_TYPES)
cat("\nExpected damage taken per round:", round(def$total, 2), "\n")
print(def$breakdown)

# DPR across target AC — the Trade-off tab in miniature
tibble::tibble(ac = 10:25) %>%
  dplyr::mutate(
    reckless = purrr::map_dbl(ac, ~ round_dpr(events, 7, .x, "advantage")$total),
    flat     = purrr::map_dbl(ac, ~ round_dpr(events, 7, .x, "flat")$total)
  ) %>%
  print(n = Inf)
