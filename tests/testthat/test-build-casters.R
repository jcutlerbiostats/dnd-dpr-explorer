source(file.path("..", "..", "R", "engine.R"))
source(file.path("..", "..", "R", "build_events.R"))

dpr <- function(b, ac = 15) round_dpr(b$events, b$attack_bonus, ac, b$roll_mode, b$crit_min)$total

test_that("Warlock L5 Eldritch Blast + Agonizing Blast + Hex: 17.8 vs AC 15", {
  b <- build_character(list(class = "Warlock", subclass = "Fiend", level = 5, cha = 4, routine = "Eldritch Blast", hex = TRUE, agonizing = TRUE))
  beams <- 2 * (0.60 * 9.5 + 0.05 * 15)
  hex   <- 2 * (0.60 * 3.5 + 0.05 * 7)
  expect_equal(dpr(b), beams + hex)
})

test_that("Hexblade L5 pact longsword + shield, Curse on, Hex: 21.1 vs AC 15; crit 19", {
  b <- build_character(list(class = "Warlock", subclass = "Hexblade", level = 5, cha = 4, str = 0, dex = 1, routine = "Pact weapon",
                            weapon = "Longsword", shield = TRUE, armor = "Scale mail", curse = TRUE, hex = TRUE, thirsting = TRUE))
  weapon <- 2 * (0.55 * 8.5 + 0.10 * 13)
  curse  <- 2 * (0.65 * 3)
  hex    <- 2 * (0.55 * 3.5 + 0.10 * 7)
  expect_equal(dpr(b), weapon + curse + hex)
  expect_equal(b$crit_min, 19); expect_equal(b$ac, 14 + 1 + 2)
})

test_that("Cleric L8 War, mace, Divine Strike, Spiritual Weapon: 13.95 vs AC 15 (Divine Strike ignores SW)", {
  b <- build_character(list(class = "Cleric", subclass = "War", level = 8, str = 4, wis = 4, weapon = "Mace", shield = TRUE,
                            routine = "Weapon", war_priest_p = 0, spiritual = TRUE, sw_slot = 2))
  weapon <- 0.60 * 7.5 + 0.05 * 11
  q <- 0.05 / 0.65
  ds <- 0.65 * ((1 - q) * 4.5 + q * 9)
  sw <- 0.60 * 8.5 + 0.05 * 13
  expect_equal(dpr(b), weapon + ds + sw)
})

test_that("Cleric Sacred Flame L5 vs save +2: 5.4; Potent Spellcasting adds WIS at 8 for Light", {
  b <- build_character(list(class = "Cleric", subclass = "Light", level = 5, wis = 4, routine = "Sacred Flame", spiritual = FALSE, t_save = 2))
  expect_equal(dpr(b), 0.6 * 9)
  b8 <- build_character(list(class = "Cleric", subclass = "Light", level = 8, wis = 4, routine = "Sacred Flame", spiritual = FALSE, t_save = 2))
  expect_equal(dpr(b8), 0.6 * (9 + 4))
})

test_that("Bard L6 Swords, rapier, flourish every round (d8): 15.75 vs AC 15", {
  b <- build_character(list(class = "Bard", subclass = "Swords", level = 6, dex = 4, weapon = "Rapier", routine = "Weapon",
                            fighting_style = "None", flourish_p = 1))
  weapon <- 2 * (0.60 * 8.5 + 0.05 * 13)
  q <- 0.05 / 0.65
  fl <- (1 - 0.35^2) * ((1 - q) * 4.5 + q * 9)
  expect_equal(dpr(b), weapon + fl)
})

test_that("Moon Druid L2 brown bear: 12.275 vs AC 15, AC 11, HP override 34", {
  b <- build_character(list(class = "Druid", subclass = "Moon", level = 2, routine = "Wild Shape", beast = "Brown bear"))
  expect_equal(dpr(b), (0.55 * 8.5 + 0.05 * 13) + (0.55 * 11 + 0.05 * 18))
  expect_equal(b$ac, 11); expect_equal(b$hp_override, 34)
  expect_true("Earth elemental" %in% beasts_for_level(10)); expect_false("Earth elemental" %in% beasts_for_level(9))
})

test_that("Wizard L5 Fire Bolt 7.7; Sorcerer L5 Quickened + Elemental Affinity doubles and adds CHA", {
  w <- build_character(list(class = "Wizard", subclass = "Abjuration", level = 5, int = 4, routine = "Fire Bolt"))
  expect_equal(dpr(w), 0.60 * 11 + 0.05 * 22)
  s <- build_character(list(class = "Sorcerer", subclass = "Draconic", level = 6, cha = 4, routine = "Fire Bolt", quicken_p = 1, affinity = TRUE))
  expect_equal(dpr(s), 2 * (0.60 * 15 + 0.05 * 26))
  expect_equal(s$ac, 13 + 2)
})

test_that("Evocation: Potent Cantrip halves on save, Empowered Evocation adds INT at 10", {
  b <- build_character(list(class = "Wizard", subclass = "Evocation", level = 10, int = 5, routine = "Sacred Flame", t_save = 2))
  # Sacred Flame isn't on the wizard list -> falls back to Fire Bolt (attack)
  expect_equal(dpr(b), 0.70 * (11 + 5) + 0.05 * (22 + 5))      # to-hit 9 vs AC 15
  b2 <- build_character(list(class = "Wizard", subclass = "Evocation", level = 10, int = 5, routine = "Mind Sliver", t_save = 2))
  # DC 17 vs +2: p_fail .7; 2d6 = 7 +5 = 12 ... Mind Sliver is enchantment -> no Empowered Evocation; half on save (Potent Cantrip)
  expect_equal(dpr(b2), 7 * (0.7 + 0.3 * 0.5))
})

test_that("Artificer Battle Smith uses INT and gets Extra Attack; Artillerist cannon is a spell attack", {
  bs <- build_character(list(class = "Artificer", subclass = "Battle Smith", level = 5, int = 4, str = 0, dex = 1, weapon = "Longsword", shield = TRUE, magic = 1))
  expect_equal(bs$events[[1]]$attack_bonus, 4 + 3 + 1); expect_equal(bs$events[[1]]$attacks, 2)
  ar <- build_character(list(class = "Artificer", subclass = "Artillerist", level = 9, int = 4, routine = "Fire Bolt", cannon = TRUE))
  cannon <- keep(ar$events, ~ grepl("Cannon", .x$label))[[1]]
  expect_equal(cannon$n_dice, 3); expect_equal(cannon$attack_kind, "spell")
})
