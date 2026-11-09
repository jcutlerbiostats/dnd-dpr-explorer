source(file.path("..", "..", "R", "engine.R"))
source(file.path("..", "..", "R", "build_events.R"))

dpr <- function(b, ac = 15) round_dpr(b$events, b$attack_bonus, ac, b$roll_mode, b$crit_min)$total

test_that("Fighter L5 Champion, longsword + shield, Dueling: 14.55 vs AC 15, AC 18", {
  b <- build_character(list(class = "Fighter", subclass = "Champion", level = 5, str = 4, dex = 1,
                            weapon = "Longsword", shield = TRUE, armor = "Chain mail",
                            fighting_style = "Dueling", surge_p = 0))
  # to-hit 7 -> p_hit .65, crit 19-20 -> .10; d_hit 4.5+4+2 = 10.5, crit 15
  expect_equal(dpr(b), 2 * (0.55 * 10.5 + 0.10 * 15))
  expect_equal(b$ac, 18); expect_equal(b$crit_min, 19)
})

test_that("Fighter Action Surge slider and GWM bonus attack appear as events", {
  b <- build_character(list(class = "Fighter", subclass = "Champion", level = 11, str = 5,
                            weapon = "Greatsword", fighting_style = "Great Weapon Fighting", surge_p = 0.25, feat510 = TRUE))
  labs <- map_chr(b$events, "label")
  expect_true(any(grepl("Action Surge", labs))); expect_true(any(grepl("GWM bonus", labs)))
  expect_equal(b$events[[1]]$attacks, 3); expect_true(b$events[[1]]$gwf)
  expect_equal(b$events[[1]]$attack_bonus, 5 + 4 - 5); expect_equal(b$events[[1]]$flat, 5 + 10)
})

test_that("Rogue L5 Swashbuckler, rapier: 13.10 vs AC 15; Uncanny Dodge on", {
  b <- build_character(list(class = "Rogue", subclass = "Swashbuckler", level = 5, str = 0, dex = 4,
                            weapon = "Rapier", armor = "Studded leather", sneak_p = 0.5))
  weapon <- 0.60 * 8.5 + 0.05 * 13
  q <- 0.05 / 0.65
  sneak  <- 0.65 * ((1 - q) * 10.5 + q * 21)          # p_sneak forced to 1 for Swashbuckler
  expect_equal(dpr(b), weapon + sneak)
  expect_equal(b$first_hit$mode, "halve"); expect_equal(b$ac, 16)
})

test_that("Rogue with a non-finesse weapon gets no Sneak Attack; Elusive at 18", {
  b <- build_character(list(class = "Rogue", subclass = "Thief", level = 18, dex = 5, weapon = "Longsword", enemy_mode = "advantage"))
  expect_false(any(grepl("Sneak", map_chr(b$events, "label"))))
  expect_equal(b$enemy_roll_mode, "flat")
})

test_that("Monk L5 Open Hand, dagger (-> d6 Martial Arts), always Flurry: 20.2 vs AC 15, AC 17", {
  b <- build_character(list(class = "Monk", subclass = "Open Hand", level = 5, str = 0, dex = 4, wis = 3,
                            weapon = "Dagger", flurry_p = 1))
  expect_equal(dpr(b), 4 * (0.60 * 7.5 + 0.05 * 11))
  expect_equal(b$ac, 17)
})

test_that("Monk Deflect Missiles only vs ranged enemies; Kensei parry adds AC", {
  inp <- list(class = "Monk", subclass = "Kensei", level = 6, dex = 4, wis = 3, weapon = "Longsword", e_melee = FALSE, kensei_parry = TRUE)
  b <- build_character(inp)
  expect_equal(b$first_hit, list(mode = "reduce", amount = 5.5 + 4 + 6))
  expect_equal(b$ac, 10 + 4 + 3 + 2)
  expect_null(build_character(modifyList(inp, list(e_melee = TRUE)))$first_hit)
})

test_that("Paladin L11 Devotion, smite every hit (1st-level slot) + IDS: 39.3 vs AC 15", {
  b <- build_character(list(class = "Paladin", subclass = "Devotion", level = 11, str = 5, dex = 0, cha = 3,
                            weapon = "Longsword", shield = TRUE, armor = "Plate", fighting_style = "Dueling",
                            smite_policy = "every", smite_slot = 1, smite_p = 1))
  weapon <- 2 * (0.70 * 11.5 + 0.05 * 16)
  smite  <- 2 * (0.70 * 9 + 0.05 * 18)
  ids    <- 2 * (0.70 * 4.5 + 0.05 * 9)
  expect_equal(dpr(b), weapon + smite + ids)
  expect_equal(b$ac, 20)
})

test_that("Paladin smite-on-crit policy and Vow of Enmity advantage", {
  b <- build_character(list(class = "Paladin", subclass = "Vengeance", level = 5, str = 4, weapon = "Greatsword",
                            smite_policy = "crits", smite_slot = 2, vow = TRUE))
  expect_equal(b$roll_mode, "advantage")
  sm <- keep(b$events, ~ .x$crit_only)[[1]]
  expect_equal(sm$n_dice, 3)                       # 2nd-level slot -> 3d8
})

test_that("Ranger L11 Gloom Stalker, longbow, Archery, Hunter's Mark: 26.08 vs AC 15", {
  b <- build_character(list(class = "Ranger", subclass = "Gloom Stalker", level = 11, str = 0, dex = 5, wis = 2,
                            weapon = "Longbow", armor = "Studded leather", fighting_style = "Archery", mark = "hunters_mark"))
  weapon <- 2 * (0.80 * 9.5 + 0.05 * 14)
  flurry <- (1 - 0.85^2) * (0.80 * 9.5 + 0.05 * 14)
  hm     <- (2 + (1 - 0.85^2)) * (0.80 * 3.5 + 0.05 * 7)
  expect_equal(dpr(b), weapon + flurry + hm)
})

test_that("every class / subclass / level combination builds and gives finite DPR and damage taken", {
  foe <- enemy_attack("Enemy", 2, 7, die = 8, flat = 4)
  for (cl in names(CLASS_SUBCLASSES)) for (sc in CLASS_SUBCLASSES[[cl]]) for (lv in c(1, 3, 5, 11, 17, 20)) {
    inp <- list(class = cl, subclass = sc, level = lv, str = 3, dex = 3, con = 2, int = 1, wis = 2, cha = 3,
                weapon = if (cl == "Rogue") "Rapier" else if (cl == "Ranger") "Longbow" else "Longsword",
                shield = FALSE, armor = "None", feat510 = TRUE, offhand = FALSE)
    b <- build_character(inp)
    expect_true(is.finite(dpr(b)), label = paste(cl, sc, lv))
    d <- damage_taken(foe, b$ac, b$enemy_roll_mode, resist = b$resist, first_hit = b$first_hit)$total
    expect_true(is.finite(d) && d >= 0, label = paste(cl, sc, lv, "defense"))
  }
})

test_that("fighting-style fixes: Dueling keeps versatile one-handed; Defense needs armor; off-hand selector", {
  d <- build_character(list(class = "Fighter", subclass = "Champion", level = 1, str = 3, weapon = "Longsword", fighting_style = "Dueling", armor = "Chain mail"))
  expect_equal(d$events[[1]]$die, 8); expect_equal(d$events[[1]]$flat, 5)
  nd <- build_character(list(class = "Fighter", subclass = "Champion", level = 1, str = 3, weapon = "Longsword", fighting_style = "None", armor = "Chain mail"))
  expect_equal(nd$events[[1]]$die, 10)
  expect_equal(build_character(list(class = "Fighter", subclass = "Champion", level = 1, dex = 2, weapon = "Longsword", fighting_style = "Defense", armor = "None"))$ac, 10 + 2)
  expect_equal(build_character(list(class = "Fighter", subclass = "Champion", level = 1, dex = 2, weapon = "Longsword", fighting_style = "Defense", armor = "Chain mail"))$ac, 17)
  tw <- build_character(list(class = "Ranger", subclass = "Hunter", level = 5, dex = 4, weapon = "Scimitar", offhand = TRUE, offhand_weapon = "Dagger",
                             fighting_style = "Two-Weapon Fighting", armor = "Scale mail"))
  oh <- keep(tw$events, ~ grepl("off-hand", .x$label))[[1]]
  expect_equal(oh$die, 4); expect_equal(oh$flat, 4)
  dw <- build_character(list(class = "Fighter", subclass = "Champion", level = 5, str = 4, weapon = "Longsword", offhand = TRUE, offhand_weapon = "Rapier",
                             feats = "Dual Wielder", fighting_style = "None", armor = "Chain mail"))
  expect_true(any(grepl("off-hand", map_chr(dw$events, "label")))); expect_equal(dw$ac, 17)
  un <- build_character(list(class = "Fighter", subclass = "Champion", level = 1, str = 3, weapon = "Unarmed strike", fighting_style = "Unarmed Fighting", armor = "Chain mail"))
  expect_equal(un$events[[1]]$die, 8)
})
