
source(file.path("..", "..", "R", "engine.R"))
source(file.path("..", "..", "R", "simulate.R"))
source(file.path("..", "..", "R", "build_events.R"))
dpr <- function(b, ac = 15) round_dpr(b$events, b$attack_bonus, ac, b$roll_mode, b$crit_min, b$to_hit_die %||% 0)$total
N <- 200000
within_mc <- function(sim, exact, k = 4) { se <- sd(sim) / sqrt(length(sim)); expect_lt(abs(mean(sim) - exact), k * se + 1e-9) }

test_that("Bless averages the hit probability over a d4; closed form matches simulation", {
  ev <- damage_event("Longsword", 1, 8, 4, attacks = 2)
  exact <- round_dpr(ev, 7, 15, to_hit_die = 4)$total
  expect_equal(exact, 2 * mean(sapply(1:4, function(k) { p <- hit_probs(7 + k, 15); p[["noncrit"]] * 8.5 + p[["crit"]] * 13 })))
  within_mc(simulate_round(ev, 7, 15, to_hit_die = 4, n = N, seed = 21), exact)
  b <- build_character(list(class = "Fighter", subclass = "Champion", level = 5, str = 4, weapon = "Longsword", armor = "Chain mail", bless = TRUE))
  expect_equal(b$to_hit_die, 4)
})

test_that("Haste, advantage source and magic dice are applied by apply_common", {
  b <- build_character(list(class = "Paladin", subclass = "Devotion", level = 5, str = 4, weapon = "Longsword", armor = "Plate",
                            haste = TRUE, adv_source = TRUE, magic_ndice = 2, magic_die = 6, smite_policy = "none"))
  labs <- map_chr(b$events, "label")
  expect_true(any(grepl("Haste", labs))); expect_true(any(grepl("Magic weapon \\+2d6", labs)))
  expect_equal(b$roll_mode, "advantage")
})

test_that("Tasha's companions attack as 'companion' kind and are not ridden by Hunter's Mark", {
  b <- build_character(list(class = "Ranger", subclass = "Beast Master", level = 11, dex = 4, wis = 3, weapon = "Longbow", armor = "Studded leather",
                            mark = "hunters_mark", command = TRUE, companion = "Beast of the Land"))
  r <- round_dpr(b$events, b$attack_bonus, 15, b$roll_mode)
  comp <- r$breakdown[r$breakdown$label == "Beast of the Land", ]
  expect_equal(comp$attacks, 2); expect_equal(comp$kind, "companion")
  hm <- r$breakdown[r$breakdown$label == "Hunter's Mark", ]
  expect_equal(hm$attacks, 2)                                  # the ranger's two attacks only
  sd <- build_character(list(class = "Artificer", subclass = "Battle Smith", level = 9, int = 4, weapon = "Longsword", armor = "Half plate", command = TRUE, jolt_p = 1))
  expect_true(any(map_chr(sd$events, "label") == "Steel Defender rend"))
  within_mc(simulate_round(b$events, b$attack_bonus, 15, n = N, seed = 22), r$total)
})

test_that("reaction attacks are injected with the trigger probability", {
  b <- build_character(list(class = "Barbarian", subclass = "Berserker", level = 14, str = 5, weapon = "Greataxe", bonus_attack = FALSE))
  expect_false(is.null(b$reaction))
  b2 <- with_reaction(b, 0.8)
  expect_equal(tail(b2$events, 1)[[1]]$p_resource, 0.8)
  gk <- build_character(list(class = "Ranger", subclass = "Hunter", level = 3, dex = 4, weapon = "Rapier", hunter3 = "Giant Killer", target_large = TRUE))
  expect_false(is.null(gk$reaction))
})

test_that("Booming Blade dice ride only the Booming Blade attack; GFB credits a 2nd target", {
  b <- build_character(list(class = "Wizard", subclass = "Bladesinging", level = 6, int = 4, dex = 4, weapon = "Rapier", routine = "Weapon + cantrip",
                            cantrip = "Booming Blade", bladesong = TRUE))
  r <- round_dpr(b$events, b$attack_bonus, 15, b$roll_mode)
  bb <- r$breakdown[grepl("extra dice", r$breakdown$label), ]
  expect_equal(bb$attacks, 1)
  g2 <- build_character(list(class = "Rogue", subclass = "Arcane Trickster", level = 5, dex = 4, weapon = "Rapier", weapon_cantrip = "Green-Flame Blade", extra_targets = 1))
  expect_true(any(grepl("2nd target", map_chr(g2$events, "label"))))
  within_mc(simulate_round(b$events, b$attack_bonus, 15, b$roll_mode, n = N, seed = 23), r$total)
})

test_that("Homing Strikes closed-form conversion: AC 15 vs +7, d8 psionic die", {
  b <- build_character(list(class = "Rogue", subclass = "Soulknife", level = 9, dex = 4, weapon = "Rapier", target_ac = 15, homing_p = 1))
  # to-hit +8 (DEX 4, PB 4) -> need 7; misses on 2..6: margins 5..1 -> P(d8 >= m) = 4/8..8/8
  expect_equal(b$events[[1]]$p_hit_add, (1/20) * sum(c(4,5,6,7,8) / 8))
})

test_that("feats: PAM beats GWM for the bonus action; Crossbow Expert; Savage Attacker gain; Piercer; HAM; Lucky", {
  pam <- build_character(list(class = "Fighter", subclass = "Champion", level = 5, str = 4, weapon = "Glaive", armor = "Chain mail", feats = c("feat510", "Polearm Master")))
  labs <- map_chr(pam$events, "label")
  expect_true(any(grepl("Polearm Master", labs))); expect_false(any(grepl("GWM bonus", labs)))
  butt <- keep(pam$events, ~ grepl("Polearm", .x$label))[[1]]
  expect_equal(butt$die, 4); expect_equal(butt$flat, 4 + 10); expect_equal(butt$attack_bonus, 4 + 3 - 5)
  xb <- build_character(list(class = "Fighter", subclass = "Champion", level = 5, dex = 4, weapon = "Hand crossbow", armor = "Chain mail", feats = "Crossbow Expert", fighting_style = "Archery"))
  expect_true(any(grepl("Crossbow Expert", map_chr(xb$events, "label"))))
  expect_equal(reroll_gain(1, 6), mean(pmax(rep(1:6, each = 6), rep(1:6, 6))) - 3.5)
  expect_equal(reroll_gain(2, 6), {x <- expand.grid(1:6,1:6,1:6,1:6); mean(pmax(x[,1]+x[,2], x[,3]+x[,4])) - 7})
  sa <- build_character(list(class = "Fighter", subclass = "Champion", level = 5, str = 4, weapon = "Greatsword", armor = "Chain mail", feats = "Savage Attacker"))
  expect_equal(keep(sa$events, ~ grepl("Savage", .x$label))[[1]]$flat, reroll_gain(2, 6))
  pi <- build_character(list(class = "Rogue", subclass = "Thief", level = 5, dex = 4, weapon = "Rapier", feats = "Piercer"))
  expect_true(any(map_lgl(pi$events, ~ isTRUE(.x$crit_only))))
  ham <- build_character(list(class = "Fighter", subclass = "Champion", level = 5, str = 4, weapon = "Longsword", armor = "Plate", feats = "Heavy Armor Master"))
  expect_equal(ham$flat_reduction, 3)
  expect_equal(build_character(list(class = "Fighter", subclass = "Champion", level = 5, str = 4, weapon = "Longsword", armor = "Half plate", feats = "Heavy Armor Master"))$flat_reduction, 0)
  lk <- build_character(list(class = "Fighter", subclass = "Champion", level = 5, str = 4, weapon = "Longsword", armor = "Chain mail", feats = "Lucky", lucky_p = 1, target_ac = 15))
  expect_equal(lk$events[[1]]$p_hit_add, 0.35 * 0.65)
})

test_that("Crusher split preserves the exact mean and the simulation agrees", {
  b <- build_character(list(class = "Fighter", subclass = "Champion", level = 11, str = 4, weapon = "Maul", armor = "Chain mail", feats = "Crusher", target_ac = 15))
  r <- round_dpr(b$events, b$attack_bonus, 15, b$roll_mode, b$crit_min)
  # attack k has advantage with prob 1-(1-p_crit)^(k-1); p_crit = .10 (Champion)
  pa <- hit_probs(8, 15, "advantage", 19); pf <- hit_probs(8, 15, "flat", 19)   # STR 4 + PB 4
  ea <- pa[["noncrit"]] * 11 + pa[["crit"]] * 18; ef <- pf[["noncrit"]] * 11 + pf[["crit"]] * 18
  expect_equal(r$total, ef + (0.1 * ea + 0.9 * ef) + ((1 - 0.9^2) * ea + 0.9^2 * ef))
  within_mc(simulate_round(b$events, b$attack_bonus, 15, b$roll_mode, b$crit_min, n = N, seed = 24), r$total)
})

test_that("multi-target credit: Horde Breaker, Volley, desert aura, Slashing Flourish", {
  hb <- build_character(list(class = "Ranger", subclass = "Hunter", level = 5, dex = 4, weapon = "Longbow", hunter3 = "Horde Breaker", extra_targets = 1))
  expect_true(any(grepl("Horde Breaker", map_chr(hb$events, "label"))))
  vo <- build_character(list(class = "Ranger", subclass = "Hunter", level = 11, dex = 4, weapon = "Longbow", hunter3 = "Colossus Slayer", extra_targets = 3))
  expect_equal(vo$events[[1]]$attacks, 4)
  sh <- barbarian_build(5, "Storm Herald", option = "Desert", inp = list(extra_targets = 2))
  expect_equal(keep(sh$events, ~ grepl("Desert", .x$label))[[1]]$attacks, 3)
  sw <- build_character(list(class = "Bard", subclass = "Swords", level = 6, dex = 4, weapon = "Rapier", routine = "Weapon", flourish_p = 1, extra_targets = 1))
  expect_true(any(grepl("Slashing Flourish", map_chr(sw$events, "label"))))
})
