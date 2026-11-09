# DPR calculator for a LEVEL 1 Barbarian (D&D 5e), any race, no subclass yet
# Source: https://dnd5e.wikidot.com/barbarian
# At level 1 the only features are Rage and Unarmored Defense:
#   - no Reckless Attack (level 2)  -> flat rolls, no advantage either way
#   - no Primal Path (level 3)      -> no subclass riders
#   - no Extra Attack (level 5)     -> one attack per round
library(magrittr)

hit_probs <- function(attack_bonus, target_ac) {
  need  <- target_ac - attack_bonus
  p_hit <- min(max((21 - need) / 20, 0.05), 0.95)
  c(hit = p_hit, crit = 0.05, noncrit = p_hit - 0.05)
}

# --- Offensive DPR: one attack, rage damage +2 ----------------------------
lvl1_offense <- function(str_mod, pb = 2, target_ac,
                         weapon_die = 12, n_weapon_dice = 1, raging = TRUE) {
  rage_dmg <- ifelse(raging, 2, 0)
  die_avg  <- (weapon_die + 1) / 2
  p <- hit_probs(str_mod + pb, target_ac)
  dmg_hit  <- n_weapon_dice * die_avg + str_mod + rage_dmg
  dmg_crit <- dmg_hit + n_weapon_dice * die_avg
  list(p = p, dmg_hit = dmg_hit, dmg_crit = dmg_crit,
       DPR = unname(p["noncrit"] * dmg_hit + p["crit"] * dmg_crit))
}

# --- Defensive DPR: expected damage taken per round -----------------------
lvl1_defense <- function(barb_ac, enemy_attack_bonus, enemy_n_attacks = 1,
                         enemy_die = 6, enemy_n_dice = 1, enemy_mod = 2,
                         raging = TRUE, dmg_type = "slashing") {
  p <- hit_probs(enemy_attack_bonus, barb_ac)
  die_avg  <- (enemy_die + 1) / 2
  dmg_hit  <- enemy_n_dice * die_avg + enemy_mod
  dmg_crit <- dmg_hit + enemy_n_dice * die_avg
  raw  <- enemy_n_attacks * (p["noncrit"] * dmg_hit + p["crit"] * dmg_crit)
  mult <- ifelse(raging && dmg_type %in% c("bludgeoning","piercing","slashing"), 0.5, 1)
  list(p = p, raw = unname(raw), multiplier = mult, DPR_taken = unname(raw * mult))
}

# --- Worked example ---------------------------------------------------------
# STR 16 (+3), CON 14 (+2), DEX 14 (+2), PB +2, greataxe, target AC 13
off_rage    <- lvl1_offense(str_mod = 3, target_ac = 13)
off_no_rage <- lvl1_offense(str_mod = 3, target_ac = 13, raging = FALSE)
# Enemy: 2 attacks at +4, 1d6+2 slashing (e.g. a pair of goblin-ish hits)
barb_ac <- 10 + 2 + 2
def_rage    <- lvl1_defense(barb_ac, enemy_attack_bonus = 4, enemy_n_attacks = 2)
def_no_rage <- lvl1_defense(barb_ac, 4, 2, raging = FALSE)
def_fire    <- lvl1_defense(barb_ac, 4, 2, dmg_type = "fire")

print(off_rage); print(off_no_rage); print(def_rage); print(def_no_rage); print(def_fire)
