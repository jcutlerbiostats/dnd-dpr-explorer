source(file.path("..", "..", "R", "engine.R"))
source(file.path("..", "..", "R", "simulate.R"))

N <- 200000
within_mc <- function(sim, exact, k = 4) {
  se <- sd(sim) / sqrt(length(sim))
  expect_lt(abs(mean(sim) - exact), k * se + 1e-9,
            label = sprintf("sim mean %.4f vs exact %.4f (se %.4f)", mean(sim), exact, se))
}

zealot5 <- list(
  damage_event("Greataxe", 1, 12, 6, attacks = 2),
  damage_event("Divine Fury", 1, 6, 2, once_per_turn = TRUE)
)

test_that("simulated offense matches closed form: level 5 Zealot, advantage", {
  sim <- simulate_round(zealot5, 7, 15, "advantage", n = N, seed = 1)
  within_mc(sim, round_dpr(zealot5, 7, 15, "advantage")$total)
})

test_that("simulated offense matches closed form: flat, crit range 19, Brutal Critical", {
  ev <- list(damage_event("Greataxe", 1, 12, 8, attacks = 2, crit_dice_bonus = 1),
             damage_event("Rider", 1, 6, 4, once_per_turn = TRUE))
  sim <- simulate_round(ev, 9, 16, "flat", crit_min = 19, n = N, seed = 2)
  within_mc(sim, round_dpr(ev, 9, 16, "flat", crit_min = 19)$total)
})

test_that("simulated offense matches closed form: GWF reroll and p_resource", {
  ev <- damage_event("Greatsword", 2, 6, 5, attacks = 2, gwf = TRUE, p_resource = 0.4)
  sim <- simulate_round(ev, 7, 15, n = N, seed = 3)
  within_mc(sim, round_dpr(ev, 7, 15)$total)
})

test_that("mixed attack bonuses: closed-form rider approximation is close", {
  ev <- list(damage_event("Main", 1, 12, 4, attacks = 2),
             damage_event("Offhand", 1, 6, 0, attack_bonus = 2),
             damage_event("Sneak", 3, 6, 0, once_per_turn = TRUE))
  sim   <- simulate_round(ev, 7, 15, n = N, seed = 4)
  within_mc(sim, round_dpr(ev, 7, 15)$total)          # exact since the ordered first-hit walk
})

test_that("save-based event matches closed form", {
  ev <- damage_event("Breath", 2, 6, 0, requires_hit = FALSE, save_dc = 15, save_bonus = 2,
                     half_on_save = TRUE)
  sim <- simulate_round(ev, 0, 0, n = N, seed = 5)
  # closed form uses exact halving; sim floors -> allow the floor bias (~0.25 * 0.4)
  expect_lt(abs(mean(sim) - round_dpr(ev, 0, 0)$total), 0.3)
})

test_that("simulated damage taken matches closed form (Zealot defense, resist physical)", {
  foe <- enemy_attack("Longsword", 2, 6, die = 8, flat = 4)
  sim <- simulate_damage_taken(foe, 15, "advantage", resist = PHYSICAL_TYPES, n = N, seed = 6)
  # simulation floors halved damage; closed form does not. Bias <= 0.5 per hit.
  exact <- damage_taken(foe, 15, "advantage", resist = PHYSICAL_TYPES)$total
  expect_lt(abs(mean(sim) - exact), 0.5)
  sim_flat <- simulate_damage_taken(foe, 15, "flat", n = N, seed = 7)
  within_mc(sim_flat, damage_taken(foe, 15)$total)
})

test_that("summaries and rounds_to_threshold behave", {
  sim <- simulate_round(zealot5, 7, 15, "advantage", n = 20000, seed = 8)
  s <- summarize_sim(sim, at_least = 30)
  expect_true(all(c("mean", "sd", "q50", "p_ge_30") %in% names(s)))
  expect_true(s$p_zero > 0 && s$p_zero < 0.05)
  rt <- rounds_to_threshold(sim, hp = 60, n = 5000, seed = 9)
  expect_equal(rt$censored, 0)
  expect_true(rt$summary$median >= 2 && rt$summary$median <= 3)
})

# ---- Phase 3: policies agree between closed form and simulation ------------------
test_that("crit_only, triggers and resource_scope='hit' match closed form", {
  ev <- list(damage_event("Greataxe", 1, 12, 6, attacks = 2),
             damage_event("Smite (crit)", 2, 8, 0, crit_only = TRUE),
             damage_event("GWM bonus", 1, 12, 6, trigger = "prior_crit"),
             damage_event("Flurry", 1, 12, 6, trigger = "prior_miss"),
             damage_event("Hex", 1, 6, 0, once_per_turn = TRUE),
             damage_event("Smite (always)", 2, 8, 0, per_hit = TRUE, p_resource = 0.5, resource_scope = "hit"))
  sim <- simulate_round(ev, 7, 15, n = N, seed = 11)
  within_mc(sim, round_dpr(ev, 7, 15)$total)
})

test_that("first-hit halving matches closed form (no resistance, so no floor bias)", {
  foe <- list(enemy_attack("Claw", 2, 6, die = 8, flat = 4), enemy_attack("Bite", 1, 6, die = 10, flat = 6))
  exact <- damage_taken(foe, 15, "flat", first_hit = list(mode = "halve"))$total
  sim   <- simulate_damage_taken(foe, 15, "flat", first_hit = list(mode = "halve"), n = N, seed = 12)
  expect_lt(abs(mean(sim) - exact), 0.3)   # floor on halving -> small downward bias
})

test_that("trigger probabilities respect p_resource (Action Surge + GWM bonus attack)", {
  ev <- list(damage_event("Greatsword", 2, 6, 15, attacks = 2, attack_bonus = 4, gwf = TRUE),
             damage_event("Surge", 2, 6, 15, attacks = 2, attack_bonus = 4, gwf = TRUE, p_resource = 0.2),
             damage_event("GWM bonus", 2, 6, 15, attack_bonus = 4, gwf = TRUE, trigger = "prior_crit"))
  sim <- simulate_round(ev, 9, 16, "advantage", n = N, seed = 13)
  within_mc(sim, round_dpr(ev, 9, 16, "advantage")$total)
})

test_that("kind-scoped riders match closed form", {
  ev <- list(damage_event("Mace", 1, 6, 4),
             damage_event("Spiritual Weapon", 1, 8, 4, attack_kind = "spell"),
             damage_event("Divine Strike", 1, 8, 0, once_per_turn = TRUE, rides = "weapon"),
             damage_event("Blessed Strikes", 1, 8, 0, once_per_turn = TRUE),
             damage_event("Hex", 1, 6, 0, per_hit = TRUE),
             damage_event("Hunter's Mark", 1, 6, 0, per_hit = TRUE, rides = "weapon"))
  sim <- simulate_round(ev, 7, 15, n = N, seed = 14)
  within_mc(sim, round_dpr(ev, 7, 15)$total)
})

test_that("GWM bonus attack + once-per-turn rider is exact (was a 0.4% approximation)", {
  ev <- list(damage_event("Greataxe", 1, 12, 9, attacks = 2, crit_dice_bonus = 1, attack_bonus = 4),
             damage_event("GWM bonus", 1, 12, 9, crit_dice_bonus = 1, attack_bonus = 4, trigger = "prior_crit"),
             damage_event("Divine Fury", 1, 6, 4, once_per_turn = TRUE))
  sim <- simulate_round(ev, 9, 16, "advantage", n = N, seed = 15)
  within_mc(sim, round_dpr(ev, 9, 16, "advantage")$total)
})

test_that("Stalker's Flurry can carry the first hit for a rider; exact", {
  ev <- list(damage_event("Longbow", 1, 8, 4, attacks = 2),
             damage_event("Flurry", 1, 8, 4, trigger = "prior_miss"),
             damage_event("Colossus Slayer", 1, 8, 0, once_per_turn = TRUE))
  sim <- simulate_round(ev, 7, 18, n = N, seed = 16)
  within_mc(sim, round_dpr(ev, 7, 18)$total)
})

test_that("rides_label and p_hit_add agree between closed form and simulation", {
  ev <- list(damage_event("Rapier", 1, 8, 4, p_hit_add = 0.1),
             damage_event("Booming Blade", 1, 8, 4),
             damage_event("Booming Blade dice", 2, 8, 0, per_hit = TRUE, rides_label = "Booming Blade"),
             damage_event("Sneak Attack", 3, 6, 0, once_per_turn = TRUE))
  sim <- simulate_round(ev, 7, 15, n = N, seed = 17)
  within_mc(sim, round_dpr(ev, 7, 15)$total)
})

test_that("a rider scoped to weapon+companion fires in the simulation (Arcane Jolt regression)", {
  ev <- list(damage_event("Hand crossbow", 1, 6, 4, attacks = 2),
             damage_event("Steel Defender", 1, 8, 4, attack_bonus = 8, attack_kind = "companion"),
             damage_event("Arcane Jolt", 2, 6, 0, once_per_turn = TRUE, rides = c("weapon", "companion"), p_resource = 0.5),
             damage_event("Hunter's Mark", 1, 6, 0, per_hit = TRUE, rides = "weapon"))
  sim <- simulate_round(ev, 7, 15, n = N, seed = 31)
  within_mc(sim, round_dpr(ev, 7, 15)$total)
})
