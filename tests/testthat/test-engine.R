# Lock the engine to the hand-verified numbers in the two Obsidian notes:
#   "Barbarian (Level 1) — Offensive & Defensive DPR"
#   "Barbarian (Path of the Zealot) — Offensive & Defensive DPR"
source(file.path("..", "..", "R", "engine.R"))

test_that("hit_probs: flat, advantage, clamps, crit range", {
  p <- hit_probs(7, 15)
  expect_equal(unname(p), c(0.65, 0.05, 0.60))
  p <- hit_probs(7, 15, "advantage")
  expect_equal(unname(p), c(0.8775, 0.0975, 0.78))
  expect_equal(hit_probs(0, 30)[["hit"]], 0.05)          # nat 20 always hits
  expect_equal(hit_probs(30, 0)[["hit"]], 0.95)          # nat 1 always misses
  expect_equal(hit_probs(5, 15, crit_min = 19)[["crit"]], 0.10)
  expect_equal(hit_probs(5, 15, "disadvantage")[["hit"]], 0.55^2)
  expect_equal(hit_probs(5, 15, "elven_accuracy")[["crit"]], 1 - 0.95^3)
})

test_that("die_mean with and without Great Weapon Fighting", {
  expect_equal(die_mean(12), 6.5)
  expect_equal(die_mean(6, gwf = TRUE), (3.5 * 2 + 3 + 4 + 5 + 6) / 6)   # 4.1667
  expect_equal(die_mean(0), 0)
})

# ---- Level 1 barbarian note ------------------------------------------------
lvl1_axe <- function(rage = TRUE)
  damage_event("Greataxe", n_dice = 1, die = 12, flat = 3 + if (rage) 2 else 0)

test_that("level 1 barbarian offense: 7.8 raging, 6.5 not", {
  expect_equal(round_dpr(lvl1_axe(), attack_bonus = 5, target_ac = 13)$total, 7.8)
  expect_equal(round_dpr(lvl1_axe(FALSE), 5, 13)$total, 6.5)
})

test_that("level 1 barbarian defense: 3.2 raging, 6.4 otherwise", {
  foe <- enemy_attack("Scimitar", n_attacks = 2, attack_bonus = 4, die = 6, flat = 2)
  ac  <- ac_unarmored_defense(dex_mod = 2, con_mod = 2)
  expect_equal(ac, 14)
  expect_equal(damage_taken(foe, ac, resist = PHYSICAL_TYPES)$total, 3.2)
  expect_equal(damage_taken(foe, ac)$total, 6.4)
  fire <- enemy_attack("Fire", 2, 4, die = 6, flat = 2, damage_type = "fire")
  expect_equal(damage_taken(fire, ac, resist = PHYSICAL_TYPES)$total, 6.4)
})

# ---- Zealot note -------------------------------------------------------------
zealot_events <- function(level, str_mod) {
  rage   <- if (level >= 16) 4 else if (level >= 9) 3 else 2
  brutal <- if (level >= 17) 3 else if (level >= 13) 2 else if (level >= 9) 1 else 0
  list(
    damage_event("Greataxe", 1, 12, str_mod + rage,
                 attacks = if (level >= 5) 2 else 1, crit_dice_bonus = brutal),
    damage_event("Divine Fury", 1, 6, floor(level / 2), once_per_turn = TRUE)
  )
}

test_that("level 5 Zealot offense reproduces 29.0 (Reckless) and 21.8 (flat)", {
  r <- round_dpr(zealot_events(5, 4), attack_bonus = 7, target_ac = 15, roll_mode = "advantage")
  expect_equal(r$total, 29.00552, tolerance = 1e-6)
  expect_equal(r$breakdown$expected[r$breakdown$label == "Greataxe"], 23.205)
  expect_equal(r$breakdown$expected[r$breakdown$label == "Divine Fury"], 5.800519, tolerance = 1e-6)
  expect_equal(round_dpr(zealot_events(5, 4), 7, 15)$total, 21.8, tolerance = 0.05)
})

test_that("level 9 Zealot offense reproduces 37.6", {
  r <- round_dpr(zealot_events(9, 5), attack_bonus = 9, target_ac = 15, roll_mode = "advantage")
  expect_equal(r$total, 37.55578, tolerance = 1e-6)
})

test_that("level 5 Zealot defense reproduces 7.58 / 5.33 / 15.16 / Bear 7.58", {
  foe <- enemy_attack("Longsword", 2, attack_bonus = 6, die = 8, flat = 4)
  ac  <- ac_unarmored_defense(2, 3)
  expect_equal(damage_taken(foe, ac, "advantage", resist = PHYSICAL_TYPES)$total, 7.57875)
  expect_equal(damage_taken(foe, ac, "flat",      resist = PHYSICAL_TYPES)$total, 5.325)
  fire <- enemy_attack("Fire breath", 2, 6, die = 8, flat = 4, damage_type = "fire")
  expect_equal(damage_taken(fire, ac, "advantage", resist = PHYSICAL_TYPES)$total, 15.1575)
  bear <- setdiff(c(PHYSICAL_TYPES, "fire", "cold", "lightning", "acid", "poison",
                    "necrotic", "radiant", "thunder", "force"), "psychic")
  expect_equal(damage_taken(fire, ac, "advantage", resist = bear)$total, 7.57875)
})

# ---- generic engine behaviour -------------------------------------------------
test_that("p_resource scales an event linearly (the smite/maneuver slider)", {
  full <- round_dpr(damage_event("Smite", 2, 8), 7, 15)$total
  half <- round_dpr(damage_event("Smite", 2, 8, p_resource = 0.5), 7, 15)$total
  expect_equal(half, full / 2)
})

test_that("per-event overrides: GWM -5 on one attack only", {
  ev <- list(damage_event("Main", 1, 12, 4),
             damage_event("GWM swing", 1, 12, 14, attack_bonus = 2))
  r <- round_dpr(ev, attack_bonus = 7, target_ac = 15)
  expect_equal(r$breakdown$p_hit, c(0.65, 0.40))
})

test_that("rider with no per-attack events contributes nothing", {
  expect_equal(round_dpr(damage_event("Hex", 1, 6, once_per_turn = TRUE), 7, 15)$total, 0)
})

test_that("save-based event: half damage on save", {
  # DC 15 vs +2 save: fails on 1-12 -> p_fail = 0.6; 2d6 = 7
  r <- round_dpr(damage_event("Breath", 2, 6, requires_hit = FALSE,
                              save_dc = 15, save_bonus = 2, half_on_save = TRUE), 0, 0)
  expect_equal(r$total, 7 * (0.6 + 0.4 * 0.5))
})

test_that("flat reduction (Heavy Armor Master) applies before resistance", {
  foe <- enemy_attack("Club", 1, attack_bonus = 100, die = 4, flat = 0)   # always hits (p=0.95)
  r <- damage_taken(foe, 10, flat_reduction = 3, resist = PHYSICAL_TYPES)
  # d_hit = max(2.5-3,0)=0 ; d_crit = max(5-3,0)=2 ; 0.05*2*0.5
  expect_equal(r$total, 0.05)
})

# ---- Phase 3 engine extensions --------------------------------------------------
test_that("crit_only rider: smite on crits only", {
  ev <- list(damage_event("Longsword", 1, 8, 5, attacks = 2),
             damage_event("Smite (crit)", 2, 8, 0, crit_only = TRUE))
  r <- round_dpr(ev, 7, 15)
  expect_equal(r$breakdown$expected[2], 2 * 0.05 * (4 * 4.5))     # 2 attacks, 5% crit, 4d8 on crit
})

test_that("prior_crit trigger: GWM bonus attack", {
  ev <- list(damage_event("Greataxe", 1, 12, 6, attacks = 2),
             damage_event("GWM bonus", 1, 12, 6, trigger = "prior_crit"))
  r <- round_dpr(ev, 7, 15)
  p_any_crit <- 1 - 0.95^2
  expect_equal(r$breakdown$expected[2], p_any_crit * (0.60 * 12.5 + 0.05 * 19))
})

test_that("prior_miss trigger: Stalker's Flurry", {
  ev <- list(damage_event("Longbow", 1, 8, 4, attacks = 2),
             damage_event("Stalker's Flurry", 1, 8, 4, trigger = "prior_miss"))
  r <- round_dpr(ev, 7, 15)
  expect_equal(r$breakdown$expected[2], (1 - 0.65^2) * (0.60 * 8.5 + 0.05 * 13))
})

test_that("first-hit halving (Uncanny Dodge) is exact for a single attack", {
  foe <- enemy_attack("Bite", 1, attack_bonus = 6, die = 8, flat = 4)
  base <- damage_taken(foe, 15)$total
  ud   <- damage_taken(foe, 15, first_hit = list(mode = "halve"))$total
  expect_equal(ud, base / 2)
})

test_that("first-hit reduce (Deflect Missiles) caps at the hit's damage", {
  foe <- enemy_attack("Arrow", 1, attack_bonus = 6, die = 8, flat = 4)
  r <- damage_taken(foe, 15, first_hit = list(mode = "reduce", amount = 100))
  expect_equal(r$total, 0)
})

test_that("per_hit rider rides every real attack, including triggered ones", {
  ev <- list(damage_event("Longsword", 1, 8, 5, attacks = 2),
             damage_event("Improved Divine Smite", 1, 8, 0, per_hit = TRUE))
  r <- round_dpr(ev, 7, 15)
  expect_equal(r$breakdown$expected[r$breakdown$label == "Improved Divine Smite"],
               2 * (0.60 * 4.5 + 0.05 * 9))
  # a rider on a p_resource=0.5 Action Surge attack set counts half
  ev2 <- c(ev, list(damage_event("Surge", 1, 8, 5, attacks = 2, p_resource = 0.5)))
  r2 <- round_dpr(ev2, 7, 15)
  expect_equal(r2$breakdown$expected[r2$breakdown$label == "Improved Divine Smite"],
               3 * (0.60 * 4.5 + 0.05 * 9))
})

test_that("riders ride only the attack kinds they are allowed to", {
  ev <- list(damage_event("Mace", 1, 6, 4),
             damage_event("Spiritual Weapon", 1, 8, 4, attack_kind = "spell"),
             damage_event("Divine Strike", 1, 8, 0, once_per_turn = TRUE, rides = "weapon"),
             damage_event("Hex", 1, 6, 0, per_hit = TRUE))
  r <- round_dpr(ev, 7, 15)
  ds <- r$breakdown[r$breakdown$label == "Divine Strike", ]
  expect_equal(ds$p_hit, 0.65)                                   # only the mace can trigger it
  hx <- r$breakdown[r$breakdown$label == "Hex", ]
  expect_equal(hx$attacks, 2)                                    # rides both attacks
})
