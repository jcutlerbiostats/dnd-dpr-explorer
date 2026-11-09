source(file.path("..", "..", "R", "engine.R"))
source(file.path("..", "..", "R", "simulate.R"))
source(file.path("..", "..", "R", "build_events.R"))
source(file.path("..", "..", "R", "combat.R"))

foe <- enemy_attack("Enemy", 2, 7, die = 8, flat = 4)

test_that("with a huge target, no pools and no nova, combat round 1 damage matches simulate_round", {
  b <- barbarian_build(5, "Zealot", str_mod = 4, dex_mod = 2, con_mod = 3)
  cb <- simulate_combat(b, foe, 15, target_hp = 1e6, char_hp = 1e6, enemy_roll_mode = "advantage", n = 100000, max_rounds = 1, seed = 1, nova = FALSE)
  exact <- round_dpr(b$events, b$attack_bonus, 15, b$roll_mode)$total
  expect_lt(abs(mean(cb$dealt_total) - exact), 4 * sd(cb$dealt_total) / sqrt(100000))
})

test_that("damage is truncated at the kill and kill_round is recorded", {
  b <- barbarian_build(5, "Zealot", str_mod = 4)
  cb <- simulate_combat(b, foe, 15, target_hp = 30, char_hp = 200, n = 5000, max_rounds = 10, seed = 2)
  expect_true(all(cb$dealt_total <= 30))
  expect_true(all(!is.na(cb$kill_round)))
  expect_true(cb$summary$p_win > 0.95)
})

test_that("pools are spent greedily: a paladin with 2 first-level slots smites at most twice", {
  b <- build_character(list(class = "Paladin", subclass = "Devotion", level = 2, str = 4, weapon = "Longsword", armor = "Chain mail",
                            smite_policy = "every", smite_slot = 1, smite_p = 1))
  expect_equal(b$pools$slots, 2)
  cb <- simulate_combat(b, foe, 10, target_hp = 1e6, char_hp = 1e6, n = 20000, max_rounds = 6, seed = 3)
  # per-round mean without smites = plain longsword; with 2 smites over 6 rounds the total is bounded
  plain <- round_dpr(b$events[[1]], b$attack_bonus, 10)$total * 6
  expect_lt(mean(cb$dealt_total), plain + 2 * 9 + 1)
  expect_gt(mean(cb$dealt_total), plain + 2 * 9 * 0.8)
})

test_that("concentration can drop Hex; with no enemy damage it never does", {
  b <- build_character(list(class = "Warlock", subclass = "Fiend", level = 5, cha = 4, routine = "Eldritch Blast", hex = TRUE))
  nothing <- enemy_attack("Nothing", 1, -20, die = 1, flat = 0)
  calm <- simulate_combat(b, nothing, 15, 1e6, 1e6, n = 20000, max_rounds = 5, seed = 4, con_save = 0)
  hot  <- simulate_combat(b, enemy_attack("Brute", 3, 15, 4, 10, 10), 15, 1e6, 1e6, n = 20000, max_rounds = 5, seed = 4, con_save = -5)
  expect_gt(mean(calm$dealt_total), mean(hot$dealt_total))
})

test_that("nova: Assassinate auto-crits round 1 when surprised; Dread Ambusher adds a round-1 attack", {
  a <- build_character(list(class = "Rogue", subclass = "Assassin", level = 5, dex = 4, weapon = "Rapier", sneak_p = 1))
  s1 <- simulate_combat(a, foe, 15, 1e6, 1e6, n = 20000, max_rounds = 1, seed = 5, nova = TRUE, surprised = TRUE)
  s0 <- simulate_combat(a, foe, 15, 1e6, 1e6, n = 20000, max_rounds = 1, seed = 5, nova = FALSE)
  expect_gt(mean(s1$dealt_total), 1.6 * mean(s0$dealt_total))
  gsk <- build_character(list(class = "Ranger", subclass = "Gloom Stalker", level = 5, dex = 4, weapon = "Longbow", mark = "none"))
  g1 <- simulate_combat(gsk, foe, 15, 1e6, 1e6, n = 20000, max_rounds = 1, seed = 6, nova = TRUE)
  g0 <- simulate_combat(gsk, foe, 15, 1e6, 1e6, n = 20000, max_rounds = 1, seed = 6, nova = FALSE)
  expect_gt(mean(g1$dealt_total), mean(g0$dealt_total) * 1.3)
})

test_that("the character can drop; p_win falls against a brutal enemy", {
  b <- build_character(list(class = "Rogue", subclass = "Thief", level = 3, dex = 3, weapon = "Rapier"))
  cb <- simulate_combat(b, enemy_attack("Ogre", 2, 9, 2, 8, 6), 15, target_hp = 120, char_hp = 20, n = 5000, max_rounds = 20, seed = 7)
  expect_lt(cb$summary$p_win, 0.2)
  expect_true(all(!is.na(cb$drop_round) | !is.na(cb$kill_round)))
})
