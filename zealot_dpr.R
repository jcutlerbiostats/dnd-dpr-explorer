# DPR calculator for a Path of the Zealot Barbarian (D&D 5e)
# Source: https://dnd5e.wikidot.com/barbarian and /barbarian:zealot
library(magrittr)

# --- Hit / crit probabilities --------------------------------------------
# p_flat = probability a single d20 roll meets/beats the target number
hit_probs <- function(attack_bonus, target_ac, advantage = FALSE) {
  need   <- target_ac - attack_bonus           # minimum d20 roll to hit
  p_hit  <- min(max((21 - need) / 20, 0.05), 0.95)  # nat 1 misses, nat 20 hits
  p_crit <- 1 / 20
  if (advantage) {
    p_hit  <- 1 - (1 - p_hit)^2
    p_crit <- 1 - (1 - p_crit)^2
  }
  c(hit = p_hit, crit = p_crit, noncrit = p_hit - p_crit)
}

# --- Offensive DPR --------------------------------------------------------
zealot_offense <- function(level, str_mod, pb, target_ac,
                           weapon_die = 12, n_weapon_dice = 1,
                           reckless = TRUE) {
  rage_dmg  <- ifelse(level >= 16, 4, ifelse(level >= 9, 3, 2))
  n_attacks <- ifelse(level >= 5, 2, 1)
  brutal    <- ifelse(level >= 17, 3, ifelse(level >= 13, 2, ifelse(level >= 9, 1, 0)))
  die_avg   <- (weapon_die + 1) / 2

  p <- hit_probs(str_mod + pb, target_ac, advantage = reckless)

  # per-attack damage
  dmg_hit  <- n_weapon_dice * die_avg + str_mod + rage_dmg
  dmg_crit <- dmg_hit + (n_weapon_dice + brutal) * die_avg   # extra dice only
  per_attack <- p["noncrit"] * dmg_hit + p["crit"] * dmg_crit

  # Divine Fury: 1d6 + floor(level/2), first hit each turn; die doubles on crit
  df_flat  <- floor(level / 2)
  p_any_hit <- 1 - (1 - p["hit"])^n_attacks
  p_first_is_crit <- p["crit"] / p["hit"]     # conditional on the first hit
  divine_fury <- p_any_hit * ((1 - p_first_is_crit) * (3.5 + df_flat) +
                              p_first_is_crit * (7 + df_flat))

  list(p = p, dmg_hit = dmg_hit, dmg_crit = dmg_crit,
       weapon_dpr = n_attacks * per_attack,
       divine_fury = unname(divine_fury),
       DPR = unname(n_attacks * per_attack + divine_fury))
}

# --- Defensive DPR (expected damage TAKEN per round) ----------------------
zealot_defense <- function(barb_ac, enemy_attack_bonus, enemy_n_attacks,
                           enemy_die = 8, enemy_n_dice = 1, enemy_mod = 4,
                           reckless = TRUE, raging = TRUE, bear_totem = FALSE,
                           dmg_type = "slashing") {
  p <- hit_probs(enemy_attack_bonus, barb_ac, advantage = reckless)
  die_avg  <- (enemy_die + 1) / 2
  dmg_hit  <- enemy_n_dice * die_avg + enemy_mod
  dmg_crit <- dmg_hit + enemy_n_dice * die_avg
  raw <- enemy_n_attacks * (p["noncrit"] * dmg_hit + p["crit"] * dmg_crit)

  physical <- dmg_type %in% c("bludgeoning", "piercing", "slashing")
  resist <- raging && (physical || (bear_totem && dmg_type != "psychic"))
  mult <- ifelse(resist, 0.5, 1)
  list(p = p, raw = unname(raw), multiplier = mult, DPR_taken = unname(raw * mult))
}

# --- Worked example: level 5 Zealot ---------------------------------------
off <- zealot_offense(level = 5, str_mod = 4, pb = 3, target_ac = 15)
def <- zealot_defense(barb_ac = 15, enemy_attack_bonus = 6, enemy_n_attacks = 2)
def_no_reckless <- zealot_defense(15, 6, 2, reckless = FALSE)
def_fire <- zealot_defense(15, 6, 2, dmg_type = "fire")
def_fire_bear <- zealot_defense(15, 6, 2, dmg_type = "fire", bear_totem = TRUE)

print(off); print(def); print(def_no_reckless); print(def_fire); print(def_fire_bear)

# Same build at level 9 (PB +4, STR 20, rage +3, Brutal Critical 1 die)
print(zealot_offense(level = 9, str_mod = 5, pb = 4, target_ac = 15)$DPR)
