source(file.path("..", "..", "R", "engine.R"))
source(file.path("..", "..", "R", "build_events.R"))

test_that("level 1 build reproduces the Level 1 note", {
  b <- barbarian_build(1, "Zealot", str_mod = 3, dex_mod = 2, con_mod = 2, reckless = FALSE)
  expect_equal(round_dpr(b$events, b$attack_bonus, 13, b$roll_mode)$total, 7.8)
  expect_equal(b$ac, 14)
  expect_equal(b$roll_mode, "flat")
})

test_that("level 5 Zealot build reproduces the Zealot note", {
  b <- barbarian_build(5, "Zealot", str_mod = 4, dex_mod = 2, con_mod = 3)
  expect_equal(round_dpr(b$events, b$attack_bonus, 15, b$roll_mode)$total, 29.00552, tolerance = 1e-6)
  expect_equal(b$ac, 15)
  foe <- enemy_attack("Longsword", 2, 6, die = 8, flat = 4)
  expect_equal(damage_taken(foe, b$ac, b$enemy_roll_mode, resist = b$resist)$total, 7.57875)
})

test_that("level 9 Zealot build reproduces 37.6", {
  b <- barbarian_build(9, "Zealot", str_mod = 5)
  expect_equal(round_dpr(b$events, b$attack_bonus, 15, b$roll_mode)$total, 37.55578, tolerance = 1e-6)
})

test_that("Bear totem resists fire while raging; not when not raging", {
  b <- barbarian_build(5, "Totem Warrior", option = "Bear")
  expect_true("fire" %in% b$resist); expect_false("psychic" %in% b$resist)
  expect_length(barbarian_build(5, "Totem Warrior", option = "Bear", raging = FALSE)$resist, 0)
})

test_that("Berserker Frenzy adds one bonus attack; Beast claws add one attack", {
  expect_length(barbarian_build(5, "Berserker")$events, 2)
  expect_equal(barbarian_build(5, "Beast", option = "Claws")$events[[1]]$attacks, 3)
  expect_equal(barbarian_build(3, "Beast", option = "Bite")$events[[1]]$die, 8)
})

test_that("GWM applies -5/+10 only to heavy weapons", {
  b <- barbarian_build(5, "Zealot", str_mod = 4, gwm = TRUE)
  expect_equal(b$events[[1]]$attack_bonus, 7 - 5); expect_equal(b$events[[1]]$flat, 4 + 2 + 10)
  b2 <- barbarian_build(5, "Zealot", str_mod = 4, weapon = "Longsword", gwm = TRUE)
  expect_equal(b2$events[[1]]$flat, 6); expect_equal(b2$events[[1]]$die, 10)   # versatile, no shield
  expect_equal(barbarian_build(5, "Zealot", weapon = "Longsword", shield = TRUE)$events[[1]]$die, 8)
})

test_that("every subclass/option builds and gives a finite DPR at every level", {
  for (sc in BARBARIAN_SUBCLASSES) {
    opts <- barbarian_options(sc)$choices %||% NA
    for (o in opts) for (lv in c(1, 3, 6, 10, 14, 20)) {
      b <- barbarian_build(lv, sc, option = if (is.na(o)) NULL else unname(o))
      expect_true(is.finite(round_dpr(b$events, b$attack_bonus, 15, b$roll_mode)$total),
                  label = paste(sc, o, lv))
    }
  }
})
