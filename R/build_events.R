# 
# build_events.R — turn a character description into engine inputs -------------
#
# build_character(inp) dispatches on inp$class to one builder per class.
# Every builder returns a `build` list:
#   events        : offensive damage events (round_dpr / simulate_round)
#   attack_bonus, roll_mode, crit_min
#   ac, enemy_roll_mode, resist, immune
#   first_hit     : NULL or list(mode, amount) for damage_taken()
#   reflect       : damage dealt back to the enemy per hit taken
#   features      : tibble of active features (Build tab)
#   notes         : character vector of modelling caveats shown in the UI
# 

# Libraries --------------------------------------------------------------------
# CSVs are read with base R (readr would add ~3 MB of WebAssembly packages for phone visitors)
read_csv_base <- function(path) { d <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"), encoding = "UTF-8"); tibble::as_tibble(d) }

DATA_DIR <- if (dir.exists("data")) "data" else file.path("..", "..", "data")
CLASSES  <- read_csv_base(file.path(DATA_DIR, "classes.csv"))
WEAPONS  <- read_csv_base(file.path(DATA_DIR, "weapons.csv"))
source(file.path(dirname(DATA_DIR), "R", "defense.R"), local = TRUE)   # DATA_DIR is "data" or "../../data"

proficiency_bonus <- function(level) 2 + (level - 1) %/% 4
# spell slots for half casters (Paladin / Ranger): rows = level, cols = slot level 1..5
half_caster_slots <- function(level) {
  tbl <- rbind(c(0,0,0,0,0), c(2,0,0,0,0), c(3,0,0,0,0), c(3,0,0,0,0), c(4,2,0,0,0), c(4,2,0,0,0), c(4,3,0,0,0), c(4,3,0,0,0),
               c(4,3,2,0,0), c(4,3,2,0,0), c(4,3,3,0,0), c(4,3,3,0,0), c(4,3,3,1,0), c(4,3,3,1,0), c(4,3,3,2,0), c(4,3,3,2,0),
               c(4,3,3,3,1), c(4,3,3,3,1), c(4,3,3,3,2), c(4,3,3,3,2))
  tbl[level, ]
}
g <- function(inp, name, default = NULL) if (is.null(inp[[name]])) default else inp[[name]]

ALL_DAMAGE_TYPES <- c("bludgeoning", "piercing", "slashing", "fire", "cold", "lightning",
                      "acid", "poison", "necrotic", "radiant", "thunder", "force", "psychic")

CLASS_SUBCLASSES <- list(
  Barbarian = c("Ancestral Guardian", "Battlerager", "Beast", "Berserker", "Giant",
                "Storm Herald", "Totem Warrior", "Wild Magic", "Zealot"),
  Fighter   = c("Champion", "Battle Master", "Samurai", "Rune Knight", "Echo Knight", "Cavalier",
                "Arcane Archer", "Psi Warrior", "Eldritch Knight", "Purple Dragon Knight"),
  Rogue     = c("Thief", "Assassin", "Arcane Trickster", "Swashbuckler", "Inquisitive", "Scout",
                "Mastermind", "Soulknife", "Phantom"),
  Monk      = c("Open Hand", "Shadow", "Four Elements", "Kensei", "Mercy", "Astral Self",
                "Sun Soul", "Drunken Master", "Long Death", "Ascendant Dragon"),
  Paladin   = c("Devotion", "Ancients", "Vengeance", "Oathbreaker", "Conquest", "Redemption",
                "Crown", "Glory", "Watchers"),
  Ranger    = c("Hunter", "Gloom Stalker", "Horizon Walker", "Monster Slayer", "Fey Wanderer",
                "Swarmkeeper", "Beast Master", "Drakewarden"),
  Warlock   = c("Hexblade", "Fiend", "Great Old One", "Archfey", "Celestial", "Genie", "Undead", "Fathomless", "Undying"),
  Cleric    = c("War", "Life", "Light", "Tempest", "Trickery", "Nature", "Knowledge", "Death", "Forge", "Grave",
                "Arcana", "Order", "Peace", "Twilight"),
  Bard      = c("Swords", "Valor", "Whispers", "Lore", "Glamour", "Eloquence", "Creation", "Spirits"),
  Druid     = c("Moon", "Spores", "Stars", "Wildfire", "Land", "Dreams", "Shepherd"),
  Artificer = c("Battle Smith", "Armorer", "Artillerist", "Alchemist"),
  Wizard    = c("Evocation", "Bladesinging", "Abjuration", "Conjuration", "Divination", "Enchantment",
                "Illusion", "Necromancy", "Transmutation", "War Magic", "Chronurgy", "Graviturgy", "Order of Scribes"),
  Sorcerer  = c("Draconic", "Wild Magic", "Divine Soul", "Shadow", "Storm", "Aberrant Mind", "Clockwork Soul")
)
BARBARIAN_SUBCLASSES <- CLASS_SUBCLASSES$Barbarian

FIGHTING_STYLES <- list(
  Fighter = c("None", "Archery", "Defense", "Dueling", "Great Weapon Fighting", "Two-Weapon Fighting",
              "Unarmed Fighting", "Thrown Weapon Fighting", "Superior Technique", "Blind Fighting", "Interception", "Protection"),
  Paladin = c("None", "Defense", "Dueling", "Great Weapon Fighting", "Blind Fighting", "Interception", "Protection", "Blessed Warrior"),
  Ranger  = c("None", "Archery", "Defense", "Dueling", "Two-Weapon Fighting", "Thrown Weapon Fighting", "Blind Fighting", "Druidic Warrior"),
  Bard    = c("Dueling", "Two-Weapon Fighting")
)
NO_DPR_STYLES <- c("Blind Fighting", "Interception", "Protection", "Blessed Warrior", "Druidic Warrior")
FEATS <- c("Great Weapon Master / Sharpshooter (−5/+10)" = "feat510", "Polearm Master", "Crossbow Expert", "Dual Wielder",
           "Elven Accuracy", "Savage Attacker", "Piercer", "Lucky", "Heavy Armor Master", "Crusher")
PAM_WEAPONS <- c("Glaive", "Halberd", "Quarterstaff", "Spear", "Pike")

# E[max of two independent copies] - E[one copy] for the sum of n_dice d(die)
reroll_gain <- function(n_dice, die, gwf = FALSE) {
  if (n_dice <= 0 || die <= 0) return(0)
  pf <- rep(1 / die, die)                                   # one die's face probabilities
  if (gwf) { pf <- rep(0, die); for (f in seq_len(die)) if (f <= 2) pf <- pf + rep(1 / die^2, die) else pf[f] <- pf[f] + 1 / die }
  dist <- pf                                                # index k = sum of k pips... built up by convolution
  if (n_dice > 1) for (k in 2:n_dice) dist <- convolve(dist, rev(pf), type = "open")
  dist[dist < 0] <- 0; dist <- dist / sum(dist)
  vals <- seq(n_dice, n_dice * die)                         # dist[j] is P(sum = j + n_dice - 1)
  cdf <- cumsum(dist); cdf_prev <- c(0, head(cdf, -1))
  sum(vals * (cdf^2 - cdf_prev^2)) - sum(vals * dist)
}

# Bonus-action attacks granted by feats, in priority order; then the GWM crit trigger.
feat_bonus_attacks <- function(prof, inp, bonus_used, extra_flat = 0, crit_dice_bonus = 0) {
  feats <- g(inp, "feats", character()); ev <- list()
  if (!bonus_used && "Polearm Master" %in% feats && prof$name %in% PAM_WEAPONS && !prof$ranged) {
    ev <- c(ev, list(damage_event(paste(prof$name, "(Polearm Master butt end)"), 1, 4, prof$flat - (if (prof$gwm_melee) 10 else 0) + extra_flat + (if (prof$gwm_melee) 10 else 0),
                                  attack_bonus = prof$to_hit, crit_dice_bonus = crit_dice_bonus))); bonus_used <- TRUE }
  if (!bonus_used && "Crossbow Expert" %in% feats && prof$name == "Hand crossbow") {
    ev <- c(ev, list(damage_event("Hand crossbow (Crossbow Expert bonus attack)", 1, 6, prof$flat, attack_bonus = prof$to_hit))); bonus_used <- TRUE }
  if (!bonus_used && prof$gwm_melee)
    ev <- c(ev, list(damage_event(paste(prof$name, "(GWM bonus attack on crit)"), prof$n_dice, prof$die, prof$flat + extra_flat,
                                  attack_bonus = prof$to_hit, gwf = prof$gwf, crit_dice_bonus = crit_dice_bonus, trigger = "prior_crit")))
  ev
}
LIGHT_WEAPONS <- function() WEAPONS$name[WEAPONS$light & !WEAPONS$ranged]

# UI choices for the subclass-specific option (NULL = no option)
barbarian_options <- function(subclass) {
  switch(subclass,
    "Totem Warrior" = list(label = "Totem spirit (3rd) / Attunement (14th)",
                           choices = c("Bear", "Eagle", "Elk", "Tiger", "Wolf")),
    "Storm Herald"  = list(label = "Storm aura", choices = c("Desert", "Sea", "Tundra")),
    "Wild Magic"    = list(label = "Wild Surge result (assumed for the whole rage)",
                           choices = c("None / non-damage (1,2,4,7)" = "none",
                                       "3: spirit explodes 1d6 force (bonus action)" = "3",
                                       "5: attackers take 1d6 force" = "5",
                                       "6: +1 AC" = "6",
                                       "8: bolt 1d6 radiant (bonus action)" = "8")),
    "Beast"         = list(label = "Natural weapon", choices = c("Claws", "Bite", "Tail")),
    "Giant"         = list(label = "Elemental Cleaver type",
                           choices = c("fire", "cold", "lightning", "acid", "thunder")),
    NULL)
}
class_options <- function(class, subclass) {
  if (class == "Barbarian") return(barbarian_options(subclass))
  NULL
}

# 
# Weapon profile shared by the weapon classes ----------------------------------
# 
# Returns a list with the resolved weapon and a closure `ev()` that makes a
# weapon-attack damage_event with all the right bonuses baked in.
weapon_profile <- function(inp, class, pb, style = "None", monk_die = NULL,
                           force_ability = NULL, allow_shield = TRUE) {
  notes <- character()
  w <- WEAPONS[WEAPONS$name == g(inp, "weapon", "Longsword"), ]
  if (nrow(w) != 1) stop("unknown weapon: ", inp$weapon)
  feats <- g(inp, "feats", character())
  shield <- allow_shield && isTRUE(g(inp, "shield", FALSE))
  if (shield && w$two_handed) { shield <- FALSE; notes <- c(notes, "Two-handed weapon: shield ignored.") }
  # off-hand weapon: its own selector; light unless Dual Wielder (then any one-handed, non-heavy)
  offhand <- isTRUE(g(inp, "offhand", FALSE)) && !w$ranged && !w$two_handed
  ow <- WEAPONS[WEAPONS$name == g(inp, "offhand_weapon", "Shortsword"), ]
  if (offhand && nrow(ow) == 1 && (ow$two_handed || ow$ranged || ow$heavy)) { offhand <- FALSE; notes <- c(notes, "Off-hand weapon must be one-handed; ignored.") }
  if (offhand && !("Dual Wielder" %in% feats) && !(w$light && ow$light)) { offhand <- FALSE
    notes <- c(notes, "Two-weapon fighting needs light weapons in both hands (or the Dual Wielder feat); off-hand ignored.") }
  if (offhand && shield) { shield <- FALSE; notes <- c(notes, "Off-hand attack: shield ignored.") }
  # versatile: two-hand it unless Dueling (d8+2 beats d10) or a hand is occupied
  versatile <- !w$two_handed && !shield && !offhand && !is.na(w$versatile_die) && style != "Dueling"
  die <- if (versatile) w$versatile_die else w$die
  two_handed <- w$two_handed || versatile
  unarmed <- w$name == "Unarmed strike"
  if (unarmed) { die <- if (style == "Unarmed Fighting") (if (!shield && !offhand) 8 else 6) else 1; w$n_dice <- 1
    if (style != "Unarmed Fighting") notes <- c(notes, "Unarmed strike without Unarmed Fighting: 1 + STR per hit.") }

  str <- g(inp, "str", 3); dex <- g(inp, "dex", 2)
  ability <- if (!is.null(force_ability)) force_ability
             else if (w$ranged) "dex"
             else if (w$finesse || (class == "Monk")) (if (dex >= str) "dex" else "str")
             else "str"
  mod <- switch(ability, str = str, dex = dex, wis = g(inp, "wis", 2), cha = g(inp, "cha", 3), int = g(inp, "int", 3))
  magic <- g(inp, "magic", 0)

  # monk weapons may use the Martial Arts die instead
  if (!is.null(monk_die)) {
    is_monk_wpn <- w$monk || (class == "Monk" && g(inp, "subclass", "") == "Kensei" && !w$heavy)
    if (!is_monk_wpn) { notes <- c(notes, sprintf("%s is not a monk weapon; unarmed strikes used instead.", w$name))
      die <- monk_die; w$n_dice <- 1; w$ranged <- FALSE; w$heavy <- FALSE }
    else if (monk_die > die) { die <- monk_die; w$n_dice <- 1 }
  }

  to_hit <- mod + pb + magic + (if (style == "Archery" && w$ranged) 2 else 0)
  flat   <- mod + magic + (if (style == "Dueling" && !w$ranged && !two_handed && !offhand && !unarmed) 2 else 0) +
            (if (style == "Thrown Weapon Fighting" && isTRUE(w$thrown)) 2 else 0)
  if (unarmed) flat <- mod + if (style == "Unarmed Fighting") 0 else 0
  gwf    <- style == "Great Weapon Fighting" && two_handed && !w$ranged
  wearing_armor <- !g(inp, "armor", "None") %in% c("None", "Mage Armor")
  misc_ac <- (if (style == "Defense" && wearing_armor) 1 else 0) + (if ("Dual Wielder" %in% feats && offhand) 1 else 0)
  if (style == "Defense" && !wearing_armor) notes <- c(notes, "Defense style needs armor; no AC bonus.")
  if (style %in% NO_DPR_STYLES) notes <- c(notes, sprintf("%s style: no effect on own DPR / damage taken.", style))

  want510 <- isTRUE(g(inp, "feat510", FALSE)) || "feat510" %in% feats
  feat <- want510 && (w$heavy && !w$ranged || w$ranged) && is.null(monk_die)
  if (want510 && !feat) notes <- c(notes, "−5/+10 feat needs a heavy melee weapon (GWM) or a ranged weapon (Sharpshooter); ignored.")
  gwm_melee <- feat && !w$ranged

  prof <- list(name = w$name, n_dice = w$n_dice, die = die, ability = ability, mod = mod, light = w$light, thrown = isTRUE(w$thrown),
               to_hit = to_hit - if (feat) 5 else 0, base_to_hit = to_hit,
               flat = flat + if (feat) 10 else 0, gwf = gwf, heavy = w$heavy, ranged = w$ranged,
               light = w$light, two_handed = two_handed, shield = shield, offhand = offhand,
               gwm_melee = gwm_melee, misc_ac = misc_ac, damage_type = w$damage_type, notes = notes)
  prof$ev <- function(label = prof$name, attacks = 1, extra_flat = 0, crit_dice_bonus = 0,
                      roll_mode = NULL, trigger = "none", p_resource = 1, to_hit_bonus = 0) {
    damage_event(label, prof$n_dice, prof$die, prof$flat + extra_flat, attacks = attacks,
                 crit_dice_bonus = crit_dice_bonus, attack_bonus = prof$to_hit + to_hit_bonus,
                 roll_mode = roll_mode, gwf = prof$gwf, trigger = trigger, p_resource = p_resource)
  }
  prof$offhand_ev <- function(add_mod = FALSE, extra_flat = 0, roll_mode = NULL) {
    damage_event(paste(ow$name, "(off-hand)"), ow$n_dice, ow$die,
                 (if (add_mod) mod else 0) + extra_flat, attack_bonus = prof$base_to_hit,
                 roll_mode = roll_mode)
  }
  prof
}

features_for <- function(class, subclass, level) {
  d <- CLASSES[CLASSES$class == class & CLASSES$level <= level & (is.na(CLASSES$subclass) | CLASSES$subclass == subclass), ]
  d[order(d$level), ]
}

finish_build <- function(class, inp, events, prof = NULL, roll_mode = "flat", crit_min = 20, ac,
                         enemy_roll_mode = "flat", resist = character(), first_hit = NULL,
                         reflect = list(), notes = character(), extra = list()) {
  b <- c(list(class = class, events = events,
         attack_bonus = if (!is.null(prof)) prof$to_hit else NA, roll_mode = roll_mode, crit_min = crit_min,
         ac = ac, enemy_roll_mode = enemy_roll_mode, resist = resist, immune = character(),
         first_hit = first_hit, reflect = reflect, pb = proficiency_bonus(inp$level),
         features = features_for(class, g(inp, "subclass", ""), inp$level),
         weapon_summary = if (!is.null(prof)) list(n_dice = prof$n_dice, die = prof$die, damage_type = prof$damage_type, gwf = prof$gwf, melee = !prof$ranged),
         notes = c(if (!is.null(prof)) prof$notes, notes)), extra)
  apply_common(b, inp)
}

# Party buffs, magic-weapon dice, Haste, advantage source. Applied to every build.
apply_common <- function(b, inp) {
  notes <- character()
  b$to_hit_die <- if (isTRUE(g(inp, "bless", FALSE))) 4 else 0
  if (isTRUE(g(inp, "adv_source", FALSE)) && b$roll_mode == "flat") { b$roll_mode <- "advantage"
    notes <- c(notes, "Advantage from an ally or condition (Faerie Fire, prone target, Help, …) applied to all your attacks.") }
  if (b$roll_mode == "advantage" && "Elven Accuracy" %in% g(inp, "feats", character())) {
    wk <- keep(b$events, ~ .x$requires_hit && !.x$per_hit && !.x$once_per_turn)
    if (length(wk) && !is.na(b$attack_bonus)) { b$roll_mode <- "elven_accuracy"
      notes <- c(notes, "Elven Accuracy: advantage rolls three dice (assumes a DEX/INT/WIS/CHA attack).") } }
  wpn <- keep(b$events, ~ .x$requires_hit && !.x$per_hit && !.x$once_per_turn && .x$attack_kind == "weapon" && .x$trigger == "none")
  if (length(wpn)) {
    if (isTRUE(g(inp, "haste", FALSE))) { h <- wpn[[1]]; h$label <- paste(h$label, "(Haste)"); h$attacks <- 1
      b$events <- c(b$events, list(h)) }
    nd <- g(inp, "magic_ndice", 0); dd <- g(inp, "magic_die", 6)
    if (nd > 0) b$events <- c(b$events, list(damage_event(sprintf("Magic weapon +%dd%d", nd, dd), nd, dd, 0, per_hit = TRUE, rides = "weapon")))
  }
  feats <- g(inp, "feats", character()); ws <- b$weapon_summary
  if (length(wpn) && !is.null(ws)) {
    if ("Savage Attacker" %in% feats && ws$melee) {
      gain <- reroll_gain(ws$n_dice, ws$die, ws$gwf)
      b$events <- c(b$events, list(damage_event("Savage Attacker (reroll, once per turn)", 0, 0, gain, once_per_turn = TRUE, rides = "weapon")))
      notes <- c(notes, sprintf("Savage Attacker: +%.2f expected on one hit per turn (E[max of two rolls] − E[roll]).", gain)) }
    if ("Piercer" %in% feats && ws$damage_type == "piercing") {
      gain <- reroll_gain(1, ws$die)
      b$events <- c(b$events, list(damage_event("Piercer (reroll one die, once per turn)", 0, 0, gain, once_per_turn = TRUE, rides = "weapon"),
                                   damage_event("Piercer (extra die on crit)", 1, ws$die, 0, crit_only = TRUE, rides = "weapon"))) }
    if ("Lucky" %in% feats) { p_l <- g(inp, "lucky_p", 0.3)
      ph <- hit_probs(wpn[[1]]$attack_bonus %||% b$attack_bonus, g(inp, "target_ac", 15), b$roll_mode, b$crit_min)
      i <- which(map_lgl(b$events, ~ identical(.x$label, wpn[[1]]$label)))[1]
      b$events[[i]]$p_hit_add <- b$events[[i]]$p_hit_add + p_l * (1 - ph[["hit"]]) * ph[["hit"]]
      notes <- c(notes, "Lucky: a missed first attack is rerolled on a fraction of rounds (slider).") }
    if ("Crusher" %in% feats && ws$damage_type == "bludgeoning" && b$roll_mode == "flat" && wpn[[1]]$attacks > 1) {
      i <- which(map_lgl(b$events, ~ identical(.x$label, wpn[[1]]$label)))[1]
      main <- b$events[[i]]; A <- main$attacks
      ph <- hit_probs(main$attack_bonus %||% b$attack_bonus, g(inp, "target_ac", 15), "flat", b$crit_min)
      main$attacks <- 1; rest <- list()
      for (k in 2:A) { pk <- 1 - (1 - ph[["crit"]])^(k - 1)
        adv <- main; adv$label <- sprintf("%s (attack %d, advantage after a crit)", main$label, k); adv$roll_mode <- "advantage"; adv$p_resource <- pk
        fl  <- main; fl$label  <- sprintf("%s (attack %d)", main$label, k); fl$p_resource <- 1 - pk
        rest <- c(rest, list(adv, fl)) }
      b$events <- c(b$events[seq_len(i)], rest, b$events[-seq_len(i)]); b$events[[i]] <- main
      notes <- c(notes, "Crusher: later attacks have advantage with the probability an earlier one crit (exact mean; simulation treats the split as independent).") }
  }
  if ("Heavy Armor Master" %in% feats && ARMOR$category[ARMOR$name == g(inp, "armor", "None")] == "heavy") { b$flat_reduction <- 3
    notes <- c(notes, "Heavy Armor Master: −3 to bludgeoning/piercing/slashing damage from nonmagical attacks (applied to all physical hits here).") }
  if (g(inp, "extra_targets", 0) > 0) notes <- c(notes, sprintf("Multi-target credit: %d additional target(s) counted where a feature allows it.", g(inp, "extra_targets", 0)))
  b$flat_reduction <- b$flat_reduction %||% 0
  b$notes <- c(b$notes, notes)
  b
}

# A weapon cantrip (Booming Blade / Green-Flame Blade): one weapon attack via `prof`, plus
# the on-hit dice that ride only that attack. `extra_targets` credits GFB's second creature.
weapon_cantrip_events <- function(name, level, prof, mod, extra_targets = 0, label = name) {
  tier <- cantrip_tier(level)
  ev <- list(prof$ev(label = label, attacks = 1))
  if (tier > 1) ev <- c(ev, list(damage_event(paste(label, "extra dice"), tier - 1, 8, 0, per_hit = TRUE, rides = "weapon", rides_label = label)))
  if (name == "Green-Flame Blade" && extra_targets > 0)
    ev <- c(ev, list(damage_event("Green-Flame Blade (2nd target)", tier - 1, 8, mod, per_hit = TRUE, rides = "weapon", rides_label = label)))
  ev
}

# Reaction attacks that fire when the enemy hits you (Retaliation, Giant Killer). The
# builder stores the event; the app / plots inject it with p_resource = P(trigger) from
# the enemy's hit probabilities, since the builder does not know the enemy.
with_reaction <- function(b, p_trigger) {
  if (is.null(b$reaction) || p_trigger <= 0) return(b)
  ev <- b$reaction$event; ev$p_resource <- min(1, p_trigger)
  b$events <- c(b$events, list(ev)); b
}

# 
# Barbarian (kept with its explicit signature; tests call it directly) ---------
# 
barbarian_build <- function(level, subclass = "Zealot", option = NULL,
                            str_mod = 3, dex_mod = 2, con_mod = 2,
                            weapon = "Greataxe", shield = FALSE, armor = "None",
                            raging = TRUE, reckless = TRUE, gwm = FALSE,
                            use_bonus_attack = TRUE,
                            infectious_fury_p = 0.5,
                            magic_weapon_bonus = 0, inp = list()) {
  stopifnot(level >= 1, level <= 20, subclass %in% BARBARIAN_SUBCLASSES)
  pb <- proficiency_bonus(level)
  notes <- character()
  if (level == 20) { str_mod <- str_mod + 2; con_mod <- con_mod + 2
    notes <- c(notes, "Primal Champion: STR/CON +4 applied as +2 to each modifier.") }

  reckless <- reckless && level >= 2
  rage_dmg <- if (!raging) 0 else if (level >= 16) 4 else if (level >= 9) 3 else 2
  attacks  <- if (level >= 5) 2 else 1
  brutal   <- if (level >= 17) 3 else if (level >= 13) 2 else if (level >= 9) 1 else 0
  roll_mode <- if (reckless) "advantage" else "flat"
  attack_bonus <- str_mod + pb + magic_weapon_bonus

  w <- WEAPONS[WEAPONS$name == weapon, ]
  if (nrow(w) != 1) stop("unknown weapon: ", weapon)
  if (shield && w$two_handed) { shield <- FALSE; notes <- c(notes, "Two-handed weapon: shield ignored.") }
  die <- if (!w$two_handed && !shield && !is.na(w$versatile_die)) w$versatile_die else w$die
  n_dice <- w$n_dice
  gwm <- gwm || "feat510" %in% g(inp, "feats", character())
  gwm_ok <- gwm && w$heavy && !w$ranged
  if (gwm && !gwm_ok) notes <- c(notes, "GWM −5/+10 needs a heavy melee weapon; ignored.")
  wpn_bonus <- if (gwm_ok) attack_bonus - 5 else attack_bonus
  wpn_flat  <- str_mod + rage_dmg + magic_weapon_bonus + if (gwm_ok) 10 else 0

  natural <- subclass == "Beast" && level >= 3
  if (natural) {
    option <- option %||% "Claws"
    nd <- switch(option, Claws = c(1, 6), Bite = c(1, 8), Tail = c(1, 8))
    n_dice <- nd[1]; die <- nd[2]; wpn_bonus <- attack_bonus; wpn_flat <- str_mod + rage_dmg
    if (option == "Claws") attacks <- attacks + 1
    gwm_ok <- FALSE
    if (gwm) notes <- c(notes, "GWM does not apply to natural weapons.")
    notes <- c(notes, sprintf("Form of the Beast: %s (%dd%d). %s", option, nd[1], nd[2],
                              if (option == "Claws") "One extra claw attack per Attack action." else ""))
  }
  events <- list(damage_event(if (natural) option else weapon, n_dice, die, wpn_flat,
                              attacks = attacks, crit_dice_bonus = brutal, attack_bonus = wpn_bonus))

  if (subclass == "Battlerager" && level >= 3) {
    notes <- c(notes, "Battlerager: spiked armor modelled as medium armor (AC 14 + DEX, max 2).")
    ac <- 14 + min(dex_mod, 2) + if (shield) 2 else 0
  } else {
    ac <- armor_ac(armor, dex_mod, shield, unarmored = function(d, c, w) 10 + d + c, con_mod = con_mod)
    if (armor != "None") notes <- c(notes, sprintf("Wearing %s: Unarmored Defense not used.", armor))
  }
  resist <- if (raging) PHYSICAL_TYPES else character()
  reflect <- list(); bonus_used <- FALSE; reaction <- NULL
  save_dc <- 8 + pb + con_mod
  xt <- g(inp, "extra_targets", 0)

  if (level >= 3) switch(subclass,
    "Zealot" = {
      events <- c(events, list(damage_event("Divine Fury", 1, 6, level %/% 2, once_per_turn = TRUE)))
    },
    "Berserker" = {
      if (raging && use_bonus_attack) { bonus_used <- TRUE
        events <- c(events, list(damage_event(paste(weapon, "(Frenzy)"), n_dice, die, wpn_flat,
                                              attack_bonus = wpn_bonus, crit_dice_bonus = brutal))) }
      if (level >= 14) reaction <- list(event = damage_event(paste(weapon, "(Retaliation)"), n_dice, die, wpn_flat,
                                                             attack_bonus = wpn_bonus, crit_dice_bonus = brutal), melee_only = TRUE)
    },
    "Totem Warrior" = {
      option <- option %||% "Bear"
      if (option == "Bear" && raging) resist <- setdiff(ALL_DAMAGE_TYPES, "psychic")
      if (option == "Tiger" && level >= 14 && use_bonus_attack) { bonus_used <- TRUE
        events <- c(events, list(damage_event(paste(weapon, "(Tiger charge)"), n_dice, die, wpn_flat,
                                              attack_bonus = wpn_bonus, crit_dice_bonus = brutal))) }
      if (option %in% c("Eagle", "Elk", "Wolf"))
        notes <- c(notes, sprintf("%s totem: mobility / ally benefits only; no DPR effect.", option))
    },
    "Battlerager" = {
      if (use_bonus_attack) { bonus_used <- TRUE
        events <- c(events, list(damage_event("Armor spikes", 1, 4, str_mod + rage_dmg, attack_bonus = attack_bonus))) }
      if (level >= 6 && reckless && raging)
        notes <- c(notes, sprintf("Reckless Abandon: %d temp HP per turn (not in damage-taken figure).", max(con_mod, 1)))
      if (level >= 14 && raging) reflect <- c(reflect, list(list(label = "Spiked Retribution", n_dice = 0, die = 0, flat = 3, melee_only = TRUE)))
    },
    "Beast" = {
      if (level >= 10 && raging)
        events <- c(events, list(damage_event("Infectious Fury", 2, 12, 0, once_per_turn = TRUE, p_resource = infectious_fury_p)))
      if (level >= 10) notes <- c(notes, "Infectious Fury: 2d12 psychic on the first hit; WIS save ignored (optimistic); availability = slider.")
    },
    "Giant" = {
      if (level >= 6) {
        events <- c(events, list(damage_event(sprintf("Elemental Cleaver (%s)", option %||% "fire"),
                                              if (level >= 14) 2 else 1, 6, 0, per_hit = TRUE)))
      }
    },
    "Storm Herald" = {
      option <- option %||% "Desert"
      step <- if (level >= 20) 5 else if (level >= 15) 4 else if (level >= 10) 3 else if (level >= 5) 2 else 1
      if (raging && use_bonus_attack) { bonus_used <- TRUE
        if (option == "Desert")
          events <- c(events, list(damage_event("Storm Aura (Desert)", 0, 0, step + 1, requires_hit = FALSE, attacks = 1 + xt)))
        if (option == "Sea") {
          events <- c(events, list(damage_event("Storm Aura (Sea)", c(1, 1, 2, 3, 4)[step], 6, 0, requires_hit = FALSE,
                                                save_dc = save_dc, save_bonus = 2)))
          notes <- c(notes, "Sea aura: target DEX save bonus assumed +2.") }
        if (option == "Tundra") notes <- c(notes, "Tundra aura: temp HP to allies; no DPR effect.")
      }
      if (level >= 6) resist <- union(resist, switch(option, Desert = "fire", Sea = "lightning", Tundra = "cold"))
      if (level >= 14 && option == "Desert" && raging)
        reflect <- c(reflect, list(list(label = "Raging Storm (Desert)", n_dice = 0, die = 0, flat = level %/% 2, melee_only = TRUE)))
    },
    "Wild Magic" = {
      option <- option %||% "none"
      if (raging) switch(option,
        "3" = if (use_bonus_attack) { bonus_used <- TRUE; events <- c(events, list(damage_event("Wild Surge 3 (force)", 1, 6, 0, requires_hit = FALSE, save_dc = save_dc, save_bonus = 2))) },
        "8" = if (use_bonus_attack) { bonus_used <- TRUE; events <- c(events, list(damage_event("Wild Surge 8 (radiant)", 1, 6, 0, requires_hit = FALSE, save_dc = save_dc, save_bonus = 2))) },
        "5" = reflect <- c(reflect, list(list(label = "Wild Surge 5", n_dice = 1, die = 6, flat = 0, melee_only = FALSE))),
        "6" = ac <- ac + 1,
        NULL)
      if (option %in% c("3", "8")) notes <- c(notes, "Wild Surge save: target save bonus assumed +2.")
    },
    "Ancestral Guardian" = {
      notes <- c(notes, "Ancestral Guardian features protect allies; none change own DPR or damage taken.")
    })

  feats <- g(inp, "feats", character())
  if (!bonus_used && "Polearm Master" %in% feats && weapon %in% PAM_WEAPONS && !natural) { bonus_used <- TRUE
    events <- c(events, list(damage_event(paste(weapon, "(Polearm Master butt end)"), 1, 4, wpn_flat, attack_bonus = wpn_bonus, crit_dice_bonus = brutal))) }
  if (gwm_ok && !bonus_used)
    events <- c(events, list(damage_event(paste(weapon, "(GWM bonus attack on crit)"), n_dice, die, wpn_flat,
                                          attack_bonus = wpn_bonus, crit_dice_bonus = brutal, trigger = "prior_crit")))

  b <- list(class = "Barbarian", events = events, attack_bonus = attack_bonus, roll_mode = roll_mode, crit_min = 20,
       ac = ac, enemy_roll_mode = if (reckless) "advantage" else "flat",
       resist = resist, immune = character(), reflect = reflect, first_hit = NULL, reaction = reaction,
       pb = pb, rage_dmg = rage_dmg, save_dc = save_dc,
       weapon_summary = list(n_dice = n_dice, die = die, damage_type = if (natural) "slashing" else w$damage_type, gwf = FALSE, melee = TRUE),
       features = features_for("Barbarian", subclass, level), notes = notes)
  apply_common(b, inp)
}

barbarian_from_inputs <- function(inp) {
  barbarian_build(inp$level, g(inp, "subclass", "Zealot"), option = inp$option,
                  str_mod = g(inp, "str", 3), dex_mod = g(inp, "dex", 2), con_mod = g(inp, "con", 2),
                  weapon = g(inp, "weapon", "Greataxe"), shield = isTRUE(inp$shield), armor = g(inp, "armor", "None"),
                  raging = isTRUE(g(inp, "raging", TRUE)), reckless = isTRUE(g(inp, "reckless", TRUE)),
                  gwm = isTRUE(inp$feat510), use_bonus_attack = isTRUE(g(inp, "bonus_attack", TRUE)),
                  infectious_fury_p = g(inp, "inf_fury_p", 0.5), magic_weapon_bonus = g(inp, "magic", 0), inp = inp)
}

# 
# Fighter ----------------------------------------------------------------------
# 
fighter_build <- function(inp) {
  level <- inp$level; sc <- g(inp, "subclass", "Champion"); pb <- proficiency_bonus(level)
  style <- g(inp, "fighting_style", "None")
  prof <- weapon_profile(inp, "Fighter", pb, style)
  notes <- character(); reflect <- list()
  attacks <- if (level >= 20) 4 else if (level >= 11) 3 else if (level >= 5) 2 else 1
  crit_min <- if (sc == "Champion") (if (level >= 15) 18 else if (level >= 3) 19 else 20) else 20
  fs <- sc == "Samurai" && level >= 3 && isTRUE(g(inp, "samurai_fs", FALSE))
  nova_adv <- sc == "Samurai" && level >= 3 && !fs      # Fighting Spirit in round 1 of the Combat tab
  roll_mode <- if (fs) "advantage" else "flat"

  events <- list()
  xt <- g(inp, "extra_targets", 0)
  if (sc == "Eldritch Knight" && level >= 7 && g(inp, "routine", "Attack action") == "War Magic") {
    cant <- g(inp, "cantrip", "Booming Blade")
    if (CANTRIPS$kind[CANTRIPS$name == cant] == "weapon") events <- weapon_cantrip_events(cant, level, prof, prof$mod, xt)
    else events <- cantrip_events(cant, level, g(inp, "int", 2), pb, inp)
    events <- c(events, list(prof$ev(label = paste(prof$name, "(War Magic bonus attack)"))))
    notes <- c(notes, "War Magic: cantrip + bonus-action weapon attack instead of the Attack action.")
    bonus_used <- TRUE; attacks <- 0
  } else bonus_used <- FALSE
  if (attacks == 0) {
  } else if (fs && level >= 15) {   # Rapid Strike: one advantage roll traded for an extra flat attack
    events <- c(events, list(prof$ev(attacks = attacks - 1, roll_mode = "advantage"),
                             prof$ev(label = paste(prof$name, "(Rapid Strike, flat)"), attacks = 2, roll_mode = "flat")))
  } else events <- c(events, list(prof$ev(attacks = attacks)))

  p_surge <- g(inp, "surge_p", 0)
  pools <- list(surge = if (level >= 17) 2 else 1)
  if (level >= 2 && p_surge > 0 && attacks > 0) { se <- prof$ev(label = paste(prof$name, "(Action Surge)"), attacks = attacks, p_resource = p_surge); se$pool <- "surge"
    events <- c(events, list(se)) }
  if (prof$offhand && !bonus_used) { bonus_used <- TRUE
    events <- c(events, list(prof$offhand_ev(add_mod = style == "Two-Weapon Fighting"))) }

  if (level >= 3) switch(sc,
    NULL)
  if (style == "Superior Technique" && sc != "Battle Master") { p <- g(inp, "superior_p", 0.1)
    if (p > 0) events <- c(events, list(damage_event("Superior Technique (d6)", 1, 6, 0, per_hit = TRUE, p_resource = p, resource_scope = "hit", rides = "weapon")))
    notes <- c(notes, "Superior Technique: one d6 superiority die per short rest; slider = fraction of hits that spend it.") }
  if (level >= 3) switch(sc,
    "Battle Master" = {
      sd <- if (level >= 18) 12 else if (level >= 10) 10 else 8
      p <- g(inp, "maneuver_p", 0.3)
      pools$superiority <- if (level >= 15) 6 else if (level >= 7) 5 else 4
      if (p > 0) events <- c(events, list(damage_event(sprintf("Maneuver (d%d)", sd), 1, sd, 0, per_hit = TRUE,
                                                        p_resource = p, resource_scope = "hit", pool = "superiority")))
      notes <- c(notes, "Maneuvers modelled as damage-only (e.g. Trip / Menacing Attack); slider = fraction of hits that spend a superiority die.")
    },
    "Rune Knight" = if (isTRUE(g(inp, "rune_might", TRUE))) { bonus_used <- TRUE
      events <- c(events, list(damage_event("Giant's Might", 1, if (level >= 18) 10 else if (level >= 10) 8 else 6, 0, once_per_turn = TRUE))) },
    "Echo Knight" = { p <- g(inp, "echo_p", 0.5); pools$echo <- max(1, g(inp, "con", 2))
      if (p > 0) { ue <- prof$ev(label = "Unleash Incarnation", attacks = 1, p_resource = p); ue$pool <- "echo"; events <- c(events, list(ue)) } },
    "Cavalier" = { p <- g(inp, "cavalier_p", 0.3)
      if (p > 0 && !bonus_used && !prof$ranged) { bonus_used <- TRUE
        events <- c(events, list(prof$ev(label = "Unwavering Mark attack", extra_flat = level %/% 2, roll_mode = "advantage", p_resource = p))) }
      notes <- c(notes, "Unwavering Mark: slider = fraction of rounds the marked target hit someone else.") },
    "Arcane Archer" = { p <- g(inp, "arcane_p", 0.4)
      if (prof$ranged && p > 0) events <- c(events, list(damage_event("Arcane Shot", if (level >= 18) 4 else 2, 6, 0, once_per_turn = TRUE, p_resource = p)))
      if (!prof$ranged) notes <- c(notes, "Arcane Shot needs a ranged weapon.") },
    "Psi Warrior" = { p <- g(inp, "psi_p", 0.5)
      die <- if (level >= 17) 12 else if (level >= 11) 10 else if (level >= 5) 8 else 6
      pools$psi <- 2 * pb
      if (p > 0) events <- c(events, list(damage_event("Psionic Strike", 1, die, g(inp, "int", 1), once_per_turn = TRUE, p_resource = p, pool = "psi"))) },
    "Eldritch Knight" = if (level < 7) notes <- c(notes, "War Magic arrives at 7th level."),
    "Purple Dragon Knight" = notes <- c(notes, "Rallying Cry / Inspiring Surge help allies; no own-DPR effect."),
    "Samurai" = if (!fs) notes <- c(notes, "Fighting Spirit off: toggle it to see the 3/long-rest advantage turns."),
    NULL)

  events <- c(events, feat_bonus_attacks(prof, inp, bonus_used))

  ac <- armor_ac(g(inp, "armor", "Chain mail"), g(inp, "dex", 2), prof$shield, prof$misc_ac)
  finish_build("Fighter", inp, events, prof, roll_mode, crit_min, ac, notes = notes,
               extra = list(pools = pools, nova = if (nova_adv) list(advantage = TRUE) else NULL))
}

# 
# Rogue ------------------------------------------------------------------------
# 
rogue_build <- function(inp) {
  level <- inp$level; sc <- g(inp, "subclass", "Thief"); pb <- proficiency_bonus(level)
  notes <- character()
  soul <- sc == "Soulknife" && level >= 3
  if (soul) inp$weapon <- "Shortsword"           # stand-in: 1d6 finesse; relabelled below
  prof <- weapon_profile(inp, "Rogue", pb)
  if (soul) prof$name <- "Psychic Blade"
  sneak_ok <- prof$ranged || WEAPONS$finesse[WEAPONS$name == g(inp, "weapon", "Rapier")] || soul
  if (!sneak_ok) notes <- c(notes, "Sneak Attack needs a finesse or ranged weapon: not applied.")

  xt <- g(inp, "extra_targets", 0)
  main <- prof$ev(label = prof$name)
  if (soul && level >= 9) {                       # Homing Strikes: exact miss-conversion probability (flat rolls)
    pd <- if (level >= 17) 12 else if (level >= 11) 10 else 8
    need <- g(inp, "target_ac", 15) - prof$to_hit
    conv <- 0; for (r in 2:max(2, need - 1)) if (r < need) conv <- conv + (1 / 20) * max(0, (pd - (need - r) + 1) / pd)
    main$p_hit_add <- conv * g(inp, "homing_p", 0.5)
    notes <- c(notes, sprintf("Homing Strikes: converts %.1f%% of misses on the main blade (slider = fraction of misses a psionic die is spent on; flat rolls).", 100 * conv))
  }
  if (g(inp, "weapon_cantrip", "None") != "None" && !soul) {
    events <- weapon_cantrip_events(g(inp, "weapon_cantrip"), level, prof, prof$mod, xt, label = prof$name)
    notes <- c(notes, sprintf("%s used in place of the Attack action (Arcane Trickster or Magic Initiate).", g(inp, "weapon_cantrip")))
  } else events <- list(main)
  if (soul) events <- c(events, list(damage_event("Psychic Blade (bonus action, 1d4)", 1, 4, prof$mod, attack_bonus = prof$base_to_hit)))
  else if (prof$offhand) events <- c(events, list(prof$offhand_ev(add_mod = FALSE)))

  p_sneak <- g(inp, "sneak_p", 0.9)
  if (sc %in% c("Swashbuckler", "Inquisitive") && level >= 3) { p_sneak <- 1
    notes <- c(notes, sprintf("%s: Sneak Attack available every round without advantage (one-on-one / Insightful Fighting).", sc)) }
  if (sneak_ok) events <- c(events, list(damage_event("Sneak Attack", ceiling(level / 2), 6, 0, once_per_turn = TRUE, p_resource = p_sneak)))
  if (sc == "Inquisitive" && level >= 13 && sneak_ok)
    events <- c(events, list(damage_event("Eye for Weakness", 3, 6, 0, once_per_turn = TRUE, p_resource = p_sneak)))
  nova <- if (sc == "Assassin" && level >= 3) list(advantage = TRUE, autocrit_if_surprised = TRUE) else NULL
  if (sc == "Assassin") notes <- c(notes, "Assassinate (advantage + auto-crit on surprised targets) is first-round only: see the Combat tab.")
  if (sc == "Phantom" && level >= 3 && sneak_ok && xt > 0) { p <- g(inp, "wails_p", 0.5)
    events <- c(events, list(damage_event("Wails from the Grave (2nd target)", ceiling(level / 2) %/% 2, 6, 0, once_per_turn = TRUE, p_resource = p_sneak * p)))
    notes <- c(notes, "Wails from the Grave: half the Sneak Attack dice to a second creature (PB uses; slider).") }
  if (sc == "Phantom" && xt == 0) notes <- c(notes, "Wails from the Grave needs a second target: raise the multi-target credit.")
  if (sc == "Soulknife") notes <- c(notes, "Psychic Blades: 1d6 + mod main, 1d4 + mod bonus action. Homing Strikes not modelled.")
  if (sc %in% c("Thief", "Scout", "Mastermind", "Arcane Trickster")) notes <- c(notes, sprintf("%s: no features that change single-target DPR.", sc))

  first_hit <- if (level >= 5 && isTRUE(g(inp, "uncanny", TRUE))) list(mode = "halve") else NULL
  enemy_roll_mode <- if (level >= 18) "flat" else g(inp, "enemy_mode", "flat")
  if (level >= 18) notes <- c(notes, "Elusive: attackers never have advantage against you.")
  ac <- armor_ac(g(inp, "armor", "Studded leather"), g(inp, "dex", 3), prof$shield)
  finish_build("Rogue", inp, events, prof, "flat", 20, ac, enemy_roll_mode, first_hit = first_hit, notes = notes, extra = list(nova = nova))
}

# 
# Monk -------------------------------------------------------------------------
# 
monk_build <- function(inp) {
  level <- inp$level; sc <- g(inp, "subclass", "Open Hand"); pb <- proficiency_bonus(level)
  ma <- if (level >= 17) 10 else if (level >= 11) 8 else if (level >= 5) 6 else 4
  notes <- character(); events <- list()
  astral <- sc == "Astral Self" && level >= 3 && isTRUE(g(inp, "astral_on", TRUE))
  prof <- weapon_profile(inp, "Monk", pb, monk_die = ma, force_ability = if (astral) "wis" else NULL, allow_shield = FALSE)
  if (astral) { prof$n_dice <- 1; prof$die <- ma; prof$name <- "Astral arms"
    notes <- c(notes, "Arms of the Astral Self: attacks use WIS and deal the Martial Arts die as force damage.") }
  sharpen <- if (sc == "Kensei" && level >= 11) g(inp, "sharpen", 0) else 0
  attacks <- if (astral && level >= 17) 3 else if (level >= 5) 2 else 1
  mod <- prof$mod

  events <- c(events, list(prof$ev(attacks = attacks, extra_flat = sharpen, to_hit_bonus = sharpen)))
  unarmed <- function(label, p = 1) damage_event(label, 1, ma, mod, attack_bonus = prof$base_to_hit, p_resource = p)
  events <- c(events, list(unarmed("Unarmed strike (bonus action)")))
  p_flurry <- if (level >= 2) g(inp, "flurry_p", 0.5) else 0
  pools <- list(ki = if (level >= 2) level else 0)
  if (p_flurry > 0) { fe <- unarmed("Flurry of Blows (2nd strike)", p_flurry); fe$pool <- "ki"; events <- c(events, list(fe)) }

  ac_misc <- 0
  if (level >= 3) switch(sc,
    "Kensei" = { if (isTRUE(g(inp, "kensei_parry", TRUE))) ac_misc <- ac_misc + 2
      if (level >= 6) { p <- g(inp, "ki_rider_p", 0.5)
        if (p > 0) events <- c(events, list(damage_event("Deft Strike", 1, ma, 0, once_per_turn = TRUE, p_resource = p, pool = "ki"))) } },
    "Mercy" = { p <- g(inp, "ki_rider_p", 0.5)
      if (p > 0) events <- c(events, list(damage_event("Hand of Harm", 1, ma, g(inp, "wis", 2), once_per_turn = TRUE, p_resource = p, pool = "ki"))) },
    "Astral Self" = if (astral) {
      if (level >= 11) events <- c(events, list(damage_event("Body of the Astral Self", 1, ma, 0, once_per_turn = TRUE)))
      if (level >= 17) ac_misc <- ac_misc + 2 },
    "Sun Soul" = notes <- c(notes, "Radiant Sun Bolt has the same numbers as an unarmed strike (ranged, radiant)."),
    "Ascendant Dragon" = notes <- c(notes, "Breath of the Dragon (replaces an attack; cone) not modelled."),
    notes <- c(notes, sprintf("%s: no features that change single-target sustained DPR.", sc)))
  notes <- c(notes, "Ki sliders = fraction of rounds the ki is spent; sustained play, not nova.")

  first_hit <- if (level >= 3 && !isTRUE(g(inp, "e_melee", TRUE)))
    list(mode = "reduce", amount = 5.5 + g(inp, "dex", 3) + level) else NULL
  if (!is.null(first_hit)) notes <- c(notes, "Deflect Missiles: first ranged hit reduced by 1d10 + DEX + level (mean).")
  ac <- armor_ac("None", g(inp, "dex", 3), FALSE, ac_misc, unarmored = function(d, c, w) 10 + d + w, wis_mod = g(inp, "wis", 2))
  finish_build("Monk", inp, events, prof, "flat", 20, ac, first_hit = first_hit, notes = notes, extra = list(pools = pools))
}

# 
# Paladin ----------------------------------------------------------------------
# 
paladin_build <- function(inp) {
  level <- inp$level; sc <- g(inp, "subclass", "Devotion"); pb <- proficiency_bonus(level)
  style <- if (level >= 2) g(inp, "fighting_style", "None") else "None"
  prof <- weapon_profile(inp, "Paladin", pb, style)
  notes <- character(); reflect <- list(); cha <- g(inp, "cha", 3)
  attacks <- if (level >= 5) 2 else 1
  roll_mode <- if (sc == "Vengeance" && level >= 3 && isTRUE(g(inp, "vow", FALSE))) "advantage" else "flat"
  sacred <- if (sc == "Devotion" && level >= 3 && isTRUE(g(inp, "sacred_weapon", FALSE))) cha else 0
  hate   <- if (sc == "Oathbreaker" && level >= 7 && !prof$ranged) cha else 0

  events <- list(prof$ev(attacks = attacks, extra_flat = hate, to_hit_bonus = sacred))
  bonus_used <- FALSE
  if (prof$offhand) { bonus_used <- TRUE; events <- c(events, list(prof$offhand_ev(add_mod = FALSE, extra_flat = hate))) }

  pools <- list()
  if (level >= 2 && !prof$ranged) {
    max_slot <- if (level >= 17) 5 else if (level >= 13) 4 else if (level >= 9) 3 else if (level >= 5) 2 else 1
    slot <- min(g(inp, "smite_slot", 1), max_slot)
    dice <- min(slot + 1, 5) + if (isTRUE(g(inp, "undead", FALSE))) 1 else 0
    policy <- g(inp, "smite_policy", "crits")
    pools <- list(slots = half_caster_slots(level)[slot])
    if (policy == "every") events <- c(events, list(damage_event(sprintf("Divine Smite (%dd8, level-%d slot)", dice, slot), dice, 8, 0,
                                                                 per_hit = TRUE, p_resource = g(inp, "smite_p", 0.5), resource_scope = "hit", pool = "slots")))
    if (policy == "crits") events <- c(events, list(damage_event(sprintf("Divine Smite on crits (%dd8)", dice), dice, 8, 0, crit_only = TRUE, pool = "slots")))
    if (level >= 11) events <- c(events, list(damage_event("Improved Divine Smite", 1, 8, 0, per_hit = TRUE)))
  }
  if (prof$ranged) notes <- c(notes, "Divine Smite requires a melee weapon attack.")
  if (sc == "Conquest" && level >= 15) reflect <- list(list(label = "Scornful Rebuke", n_dice = 0, die = 0, flat = cha, melee_only = FALSE))
  if (sc %in% c("Ancients", "Redemption", "Crown", "Glory", "Watchers")) notes <- c(notes, sprintf("%s: no features that change single-target DPR.", sc))
  events <- c(events, feat_bonus_attacks(prof, inp, bonus_used, extra_flat = hate))

  ac <- armor_ac(g(inp, "armor", "Chain mail"), g(inp, "dex", 0), prof$shield, prof$misc_ac)
  finish_build("Paladin", inp, events, prof, roll_mode, 20, ac, reflect = reflect, notes = notes, extra = list(pools = pools))
}

# 
# Ranger -----------------------------------------------------------------------
# 
ranger_build <- function(inp) {
  level <- inp$level; sc <- g(inp, "subclass", "Hunter"); pb <- proficiency_bonus(level)
  style <- if (level >= 2) g(inp, "fighting_style", "None") else "None"
  prof <- weapon_profile(inp, "Ranger", pb, style)
  notes <- character()
  attacks <- if (level >= 5) 2 else 1
  events <- list(prof$ev(attacks = attacks))
  bonus_used <- FALSE
  if (prof$offhand) { bonus_used <- TRUE; events <- c(events, list(prof$offhand_ev(add_mod = style == "Two-Weapon Fighting"))) }

  mark <- g(inp, "mark", "hunters_mark")
  if (mark == "hunters_mark" && level >= 2) events <- c(events, list(damage_event("Hunter's Mark", 1, 6, 0, per_hit = TRUE, rides = "weapon", concentration = TRUE)))
  if (mark == "favored_foe") events <- c(events, list(damage_event("Favored Foe", 1, if (level >= 14) 8 else if (level >= 6) 6 else 4, 0, once_per_turn = TRUE, concentration = TRUE)))
  nova_events <- list()
  if (level >= 20) events <- c(events, list(damage_event("Foe Slayer (+WIS)", 0, 0, g(inp, "wis", 2), once_per_turn = TRUE)))

  first_hit <- NULL; reaction <- NULL; xt <- g(inp, "extra_targets", 0)
  spell_hit <- g(inp, "wis", 2) + pb
  if (level >= 3) switch(sc,
    "Hunter" = { hp3 <- g(inp, "hunter3", "Colossus Slayer")
      if (hp3 == "Colossus Slayer") events <- c(events, list(damage_event("Colossus Slayer", 1, 8, 0, once_per_turn = TRUE, p_resource = g(inp, "colossus_p", 0.9))))
      if (hp3 == "Horde Breaker" && xt > 0) events <- c(events, list(prof$ev(label = "Horde Breaker (2nd target)")))
      if (hp3 == "Horde Breaker" && xt == 0) notes <- c(notes, "Horde Breaker needs a second target within 5 ft: raise the multi-target credit.")
      if (hp3 == "Giant Killer" && isTRUE(g(inp, "target_large", FALSE))) reaction <- list(event = prof$ev(label = "Giant Killer (reaction)"), melee_only = TRUE)
      if (hp3 == "Giant Killer" && !isTRUE(g(inp, "target_large", FALSE))) notes <- c(notes, "Giant Killer needs a Large or larger target (toggle in Target panel).")
      if (level >= 11 && xt > 0 && (1 + xt) > attacks) { events[[1]]$attacks <- 1 + xt
        notes <- c(notes, sprintf("Volley / Whirlwind Attack: one attack against each of %d targets instead of Extra Attack.", 1 + xt)) }
      if (level >= 15 && isTRUE(g(inp, "uncanny", TRUE))) first_hit <- list(mode = "halve")
      notes <- c(notes, "Hunter: Colossus Slayer slider = fraction of rounds the target is below max HP.") },
    "Beast Master" = if (isTRUE(g(inp, "command", TRUE))) { bonus_used <- TRUE
      beast <- g(inp, "companion", "Beast of the Land")
      bd <- switch(beast, "Beast of the Land" = c(8, 2), "Beast of the Sea" = c(6, 2), "Beast of the Sky" = c(4, 3))
      events <- c(events, list(damage_event(beast, 1, bd[1], bd[2] + pb, attacks = if (level >= 11) 2 else 1, attack_bonus = spell_hit, attack_kind = "companion")))
      notes <- c(notes, "Primal companion (Tasha's) commanded with your bonus action; Bestial Fury at 11th gives it two attacks. Your riders (Hunter's Mark) do not ride its hits.") },
    "Drakewarden" = { if (isTRUE(g(inp, "command", TRUE))) { bonus_used <- TRUE
        events <- c(events, list(damage_event("Drake bite", 1, 6, pb, attack_bonus = 3 + pb, attack_kind = "companion")))
        if (level >= 7) events <- c(events, list(damage_event("Drake bite (essence)", if (level >= 15) 2 else 1, 6, 0, per_hit = TRUE, rides = "companion"))) }
      events <- c(events, list(damage_event("Infused Strikes (drake reaction)", 1, 6, 0, once_per_turn = TRUE, rides = "weapon")))
      notes <- c(notes, "Drake commanded with your bonus action; Infused Strikes adds 1d6 to one of your weapon hits per round.") },
    "Gloom Stalker" = { if (level >= 11) events <- c(events, list(prof$ev(label = "Stalker's Flurry", trigger = "prior_miss")))
      nova_events <- list(prof$ev(label = "Dread Ambusher attack (round 1)", extra_flat = 4.5))
      notes <- c(notes, "Dread Ambusher (first-turn extra attack +1d8) is nova: only in the Combat tab's round 1.") },
    "Horizon Walker" = { bonus_used <- TRUE
      events <- c(events, list(damage_event("Planar Warrior", if (level >= 11) 2 else 1, 8, 0, once_per_turn = TRUE))) },
    "Monster Slayer" = { bonus_used <- TRUE
      events <- c(events, list(damage_event("Slayer's Prey", 1, 6, 0, once_per_turn = TRUE))) },
    "Fey Wanderer" = events <- c(events, list(damage_event("Dreadful Strikes", 1, if (level >= 11) 6 else 4, 0, once_per_turn = TRUE))),
    "Swarmkeeper" = events <- c(events, list(damage_event("Gathered Swarm", 1, if (level >= 11) 8 else 6, 0, once_per_turn = TRUE))),
    NULL)
  events <- c(events, feat_bonus_attacks(prof, inp, bonus_used))

  ac <- armor_ac(g(inp, "armor", "Scale mail"), g(inp, "dex", 3), prof$shield, prof$misc_ac)
  finish_build("Ranger", inp, events, prof, "flat", 20, ac, first_hit = first_hit, notes = notes, extra = list(reaction = reaction, nova_events = nova_events))
}

# The generics -----------------------------------------------------------------
build_character <- function(inp) {
  switch(g(inp, "class", "Barbarian"),
    Barbarian = barbarian_from_inputs(inp),
    Fighter   = fighter_build(inp),
    Rogue     = rogue_build(inp),
    Monk      = monk_build(inp),
    Paladin   = paladin_build(inp),
    Ranger    = ranger_build(inp),
    Warlock   = warlock_build(inp),
    Cleric    = cleric_build(inp),
    Bard      = bard_build(inp),
    Druid     = druid_build(inp),
    Artificer = artificer_build(inp),
    Wizard    = wizard_build(inp),
    Sorcerer  = sorcerer_build(inp),
    stop("unknown class"))
}

# expected damage reflected back per round, given the enemy's expected hits
reflect_damage <- function(build, enemy_attacks, def_breakdown) {
  if (length(build$reflect) == 0) return(tibble())
  db <- def_breakdown[!is.na(def_breakdown$attacks), ]
  hits <- db$attacks * db$p_hit
  map_dfr(build$reflect, function(r) {
    per <- r$n_dice * die_mean(r$die) + r$flat
    tibble(label = r$label, per_hit = per, expected = sum(hits) * per)
  })
}

# 
# Cantrip helpers (Phase 4) ------------------------------------------------------
# 
CANTRIPS <- read_csv_base(file.path(DATA_DIR, "cantrips.csv"))
BEASTS   <- read_csv_base(file.path(DATA_DIR, "beasts.csv"))

cantrip_tier <- function(level) 1 + (level >= 5) + (level >= 11) + (level >= 17)
cantrips_for <- function(class) CANTRIPS$name[map_lgl(strsplit(CANTRIPS$classes, "\\|"), ~ class %in% .x)]

# One cast of a cantrip as damage events.
#   mod, pb       : casting ability modifier and proficiency bonus
#   per_cast_flat : bonus added once per cast (Potent Spellcasting, Empowered Evocation, …)
#   per_beam_flat : bonus added to every beam (Agonizing Blast)
#   p_resource    : fraction of rounds this cast happens (Quickened Spell)
cantrip_events <- function(name, level, mod, pb, inp, per_cast_flat = 0, per_beam_flat = 0,
                           p_resource = 1, half_on_save = NULL, label = name) {
  c0 <- CANTRIPS[CANTRIPS$name == name, ]
  if (nrow(c0) != 1) stop("unknown cantrip: ", name)
  tier <- cantrip_tier(level)
  if (c0$kind == "attack") {
    beams <- if (name == "Eldritch Blast") tier else 1
    dice  <- if (name == "Eldritch Blast") 1 else tier
    ev <- list(damage_event(label, dice * c0$n_dice, c0$die, per_beam_flat + if (beams == 1) per_cast_flat else 0,
                            attacks = beams, attack_bonus = mod + pb, attack_kind = "spell", p_resource = p_resource))
    if (beams > 1 && per_cast_flat != 0)
      ev <- c(ev, list(damage_event(paste(label, "(per-cast bonus)"), 0, 0, per_cast_flat, once_per_turn = TRUE,
                                    rides = "spell", p_resource = p_resource)))
    ev
  } else {
    list(damage_event(label, tier * c0$n_dice, c0$die, per_cast_flat, requires_hit = FALSE,
                      save_dc = 8 + pb + mod, save_bonus = g(inp, "t_save", 2),
                      half_on_save = half_on_save %||% c0$half_on_save, p_resource = p_resource,
                      undamaged_die = if (name == "Toll the Dead") 8 else NULL))
  }
}

caster_ac <- function(inp, default_armor, shield_ok = TRUE, misc = 0) {
  armor_ac(g(inp, "armor", default_armor), g(inp, "dex", 2), shield_ok && isTRUE(g(inp, "shield", FALSE)), misc)
}

# 
# Warlock ------------------------------------------------------------------------
# 
warlock_build <- function(inp) {
  level <- inp$level; sc <- g(inp, "subclass", "Hexblade"); pb <- proficiency_bonus(level)
  cha <- g(inp, "cha", 4); notes <- character(); events <- list(); crit_min <- 20
  routine <- g(inp, "routine", "Eldritch Blast")
  curse <- sc == "Hexblade" && isTRUE(g(inp, "curse", TRUE))
  if (curse) crit_min <- 19
  pact_slot <- min(5, (level + 1) %/% 2)
  prof <- NULL; bonus_used <- FALSE

  if (routine == "Pact weapon") {
    hexw <- sc == "Hexblade"
    prof <- weapon_profile(inp, "Warlock", pb, force_ability = if (hexw) "cha" else NULL)
    if (hexw) { prof$mod <- cha; prof$to_hit <- cha + pb + g(inp, "magic", 0) - (if (prof$gwm_melee) 5 else 0)
                prof$base_to_hit <- cha + pb + g(inp, "magic", 0); prof$flat <- cha + g(inp, "magic", 0) + (if (prof$gwm_melee) 10 else 0)
                notes <- c(notes, "Hex Warrior: CHA used for pact weapon attacks (no two-handed weapons).") }
    attacks <- if (level >= 5 && isTRUE(g(inp, "thirsting", TRUE))) 2 else 1
    if (g(inp, "weapon_cantrip", "None") != "None") { events <- c(events, weapon_cantrip_events(g(inp, "weapon_cantrip"), level, prof, prof$mod, g(inp, "extra_targets", 0), label = prof$name))
      notes <- c(notes, sprintf("%s in place of the Attack action (one attack, no Thirsting Blade).", g(inp, "weapon_cantrip"))) }
    else events <- c(events, list(prof$ev(attacks = attacks)))
    if (level >= 12) events <- c(events, list(damage_event("Lifedrinker", 0, 0, max(cha, 1), per_hit = TRUE, rides = "weapon")))
    p_smite <- g(inp, "esmite_p", 0)
    if (level >= 5 && p_smite > 0) events <- c(events, list(damage_event(sprintf("Eldritch Smite (%dd8)", 1 + pact_slot), 1 + pact_slot, 8, 0,
                                                                         once_per_turn = TRUE, rides = "weapon", p_resource = p_smite, pool = "pact")))
    events <- c(events, feat_bonus_attacks(prof, inp, FALSE))
    notes <- c(notes, "Pact of the Blade routine: Thirsting Blade (5th), Lifedrinker (12th), Eldritch Smite slider = fraction of rounds a pact slot is spent.")
  } else {
    agon <- if (level >= 2 && isTRUE(g(inp, "agonizing", TRUE)) && routine == "Eldritch Blast") cha else 0
    events <- c(events, cantrip_events(routine, level, cha, pb, inp, per_beam_flat = agon))
    if (sc == "Celestial" && level >= 6 && CANTRIPS$damage_type[CANTRIPS$name == routine] %in% c("fire", "radiant"))
      events <- c(events, list(damage_event("Radiant Soul (+CHA)", 0, 0, cha, once_per_turn = TRUE, rides = "spell")))
  }
  kind_now <- if (routine == "Pact weapon") "weapon" else "spell"
  if (isTRUE(g(inp, "hex", TRUE))) events <- c(events, list(damage_event("Hex", 1, 6, 0, per_hit = TRUE, concentration = TRUE)))
  if (curse) events <- c(events, list(damage_event("Hexblade's Curse (+PB)", 0, 0, pb, per_hit = TRUE)))
  if (sc == "Genie") events <- c(events, list(damage_event("Genie's Wrath (+PB)", 0, 0, pb, once_per_turn = TRUE)))
  if (sc == "Undead" && level >= 6 && isTRUE(g(inp, "dread", TRUE))) {
    die <- if (routine == "Pact weapon") prof$die else CANTRIPS$die[CANTRIPS$name == routine]
    events <- c(events, list(damage_event("Grave Touched (+1 die, Form of Dread)", 1, die, 0, once_per_turn = TRUE)))
    notes <- c(notes, "Form of Dread assumed active (PB uses per long rest).") }
  if (sc == "Fathomless") { p <- g(inp, "tentacle_p", 0.5)
    if (p > 0) events <- c(events, list(damage_event("Tentacle of the Deeps", if (level >= 10) 2 else 1, 8, 0, attack_bonus = cha + pb,
                                                     attack_kind = "spell", p_resource = p, pool = "tentacle"))) }
  if (sc %in% c("Fiend", "Great Old One", "Archfey", "Undying")) notes <- c(notes, sprintf("%s: no features that change single-target sustained DPR.", sc))
  notes <- c(notes, if (isTRUE(g(inp, "hex", TRUE))) "Hex assumed up (cast earlier, concentration held).")
  ac <- caster_ac(inp, if (sc == "Hexblade") "Scale mail" else "Leather", shield_ok = sc == "Hexblade")
  pools <- list(pact = if (level >= 17) 4 else if (level >= 11) 3 else if (level >= 2) 2 else 1, tentacle = pb)
  b <- finish_build("Warlock", inp, events, prof, "flat", crit_min, ac, notes = notes, extra = list(pools = pools))
  if (is.null(prof)) b$attack_bonus <- cha + pb
  b
}

# 
# Cleric -------------------------------------------------------------------------
# 
DIVINE_STRIKE_DOMAINS <- c("Death", "Forge", "Life", "Nature", "Order", "Tempest", "Trickery", "Twilight", "War")
POTENT_DOMAINS <- c("Arcana", "Grave", "Knowledge", "Light", "Peace")

cleric_build <- function(inp) {
  level <- inp$level; sc <- g(inp, "subclass", "War"); pb <- proficiency_bonus(level)
  wis <- g(inp, "wis", 4); notes <- character(); events <- list(); prof <- NULL
  routine <- g(inp, "routine", "Weapon"); blessed <- isTRUE(g(inp, "blessed", FALSE))
  bonus_used <- FALSE

  if (routine == "Weapon") {
    prof <- weapon_profile(inp, "Cleric", pb)
    events <- c(events, list(prof$ev()))
    if (sc == "War" && level >= 1) { p <- g(inp, "war_priest_p", 0.3)
      if (p > 0) { bonus_used <- TRUE; wp <- prof$ev(label = "War Priest bonus attack", p_resource = p); wp$pool <- "war_priest"; events <- c(events, list(wp)) } }
    if (level >= 8 && sc %in% DIVINE_STRIKE_DOMAINS && !blessed)
      events <- c(events, list(damage_event("Divine Strike", if (level >= 14) 2 else 1, 8, 0, once_per_turn = TRUE, rides = "weapon")))
    if (sc == "Death" && level >= 2) { p <- g(inp, "touch_death_p", 0.2)
      if (p > 0 && !prof$ranged) events <- c(events, list(damage_event("Touch of Death", 0, 0, 5 + 2 * level, once_per_turn = TRUE, rides = "weapon", p_resource = p, pool = "channel"))) }
  } else {
    potent <- if (level >= 8 && sc %in% POTENT_DOMAINS && !blessed) wis else 0
    events <- c(events, cantrip_events(routine, level, wis, pb, inp, per_cast_flat = potent))
  }
  if (level >= 8 && blessed) events <- c(events, list(damage_event("Blessed Strikes", 1, 8, 0, once_per_turn = TRUE)))
  if (isTRUE(g(inp, "spiritual", TRUE)) && level >= 3 && bonus_used)
    notes <- c(notes, "Spiritual Weapon skipped: the bonus action is spent on the War Priest attack (set its slider to 0 to use Spiritual Weapon).")
  if (isTRUE(g(inp, "spiritual", TRUE)) && level >= 3 && !bonus_used) {
    slot <- min(g(inp, "sw_slot", 2), if (level >= 9) 5 else if (level >= 7) 4 else if (level >= 5) 3 else 2)
    events <- c(events, list(damage_event(sprintf("Spiritual Weapon (slot %d)", slot), 1 + (slot - 2) %/% 2, 8, wis,
                                          attack_bonus = wis + pb, attack_kind = "spell")))
  }
  if (sc %in% DIVINE_STRIKE_DOMAINS && routine != "Weapon" && !blessed) notes <- c(notes, "Divine Strike needs a weapon attack.")
  if (sc %in% POTENT_DOMAINS && routine == "Weapon" && !blessed) notes <- c(notes, "Potent Spellcasting applies to cantrips only.")
  ac <- caster_ac(inp, "Scale mail")
  b <- finish_build("Cleric", inp, events, prof, "flat", 20, ac, notes = notes, extra = list(pools = list(war_priest = max(1, wis), channel = 1)))
  if (is.null(prof)) b$attack_bonus <- wis + pb
  b
}

# 
# Bard ---------------------------------------------------------------------------
# 
bard_build <- function(inp) {
  level <- inp$level; sc <- g(inp, "subclass", "Swords"); pb <- proficiency_bonus(level)
  cha <- g(inp, "cha", 4); notes <- character(); events <- list(); prof <- NULL
  bi_die <- if (level >= 15) 12 else if (level >= 10) 10 else if (level >= 5) 8 else 6
  routine <- g(inp, "routine", if (sc %in% c("Swords", "Valor", "Whispers")) "Weapon" else "Vicious Mockery")
  if (routine == "Weapon") {
    prof <- weapon_profile(inp, "Bard", pb, if (sc == "Swords" && level >= 3) g(inp, "fighting_style", "Dueling") else "None")
    attacks <- if (sc %in% c("Swords", "Valor") && level >= 6) 2 else 1
    events <- c(events, list(prof$ev(attacks = attacks)))
    if (prof$offhand) events <- c(events, list(prof$offhand_ev(add_mod = sc == "Swords" && g(inp, "fighting_style", "Dueling") == "Two-Weapon Fighting")))
    if (sc == "Swords" && level >= 3) { p <- g(inp, "flourish_p", 0.5)
      if (level >= 14) { p <- 1; notes <- c(notes, "Master's Flourish: free d6 flourish every turn.") }
      if (p > 0) events <- c(events, list(damage_event("Blade Flourish", 1, if (level >= 14 && g(inp, "flourish_p", 0.5) < 1) 6 else bi_die, 0,
                                                       once_per_turn = TRUE, rides = "weapon", p_resource = p, pool = if (level >= 14) NULL else "bardic"))) }
    if (sc == "Whispers" && level >= 3) { p <- g(inp, "flourish_p", 0.5)
      nd <- if (level >= 15) 8 else if (level >= 10) 5 else if (level >= 5) 3 else 2
      if (p > 0) events <- c(events, list(damage_event(sprintf("Psychic Blades (%dd6)", nd), nd, 6, 0, once_per_turn = TRUE, rides = "weapon", p_resource = p, pool = "bardic"))) }
    if (sc == "Swords" && level >= 3 && g(inp, "extra_targets", 0) > 0 && g(inp, "flourish_p", 0.5) > 0)
      events <- c(events, list(damage_event("Slashing Flourish (2nd target)", 1, bi_die, 0, once_per_turn = TRUE, rides = "weapon", p_resource = if (level >= 14) 1 else g(inp, "flourish_p", 0.5))))
  } else {
    events <- c(events, cantrip_events(routine, level, cha, pb, inp))
    if (sc == "Valor" && level >= 14) { prof <- weapon_profile(inp, "Bard", pb)
      events <- c(events, list(prof$ev(label = paste(prof$name, "(Battle Magic bonus attack)"))))
      notes <- c(notes, "Battle Magic: bonus-action weapon attack after casting the cantrip.") }
  }
  if (!sc %in% c("Swords", "Valor", "Whispers")) notes <- c(notes, sprintf("%s: no features that change single-target DPR.", sc))
  notes <- c(notes, "Bardic Inspiration slider = fraction of rounds a die is spent on damage.")
  ac <- caster_ac(inp, "Studded leather", shield_ok = sc == "Valor")
  b <- finish_build("Bard", inp, events, prof, "flat", 20, ac, notes = notes, extra = list(pools = list(bardic = max(1, cha))))
  if (is.null(prof)) b$attack_bonus <- cha + pb
  b
}

# 
# Druid --------------------------------------------------------------------------
# 
beasts_for_level <- function(level) {
  cap <- if (level < 2) 0 else if (level < 6) 1 else level %/% 3
  elem <- grepl("elemental", BEASTS$beast)
  unique(BEASTS$beast[BEASTS$min_level <= level & ((!elem & BEASTS$cr <= cap) | (elem & level >= 10))])
}

druid_build <- function(inp) {
  level <- inp$level; sc <- g(inp, "subclass", "Moon"); pb <- proficiency_bonus(level)
  wis <- g(inp, "wis", 4); notes <- character(); events <- list(); prof <- NULL; extra <- list()
  routine <- g(inp, "routine", if (sc == "Moon") "Wild Shape" else "Shillelagh")
  if (routine == "Wild Shape" && (sc != "Moon" || level < 2)) { routine <- "Shillelagh"
    notes <- c(notes, "Wild Shape combat routine needs Circle of the Moon at 2nd level; Shillelagh shown.") }
  ac <- NULL; resist <- character()

  if (routine == "Wild Shape" && sc == "Moon" && level >= 2) {
    beast <- g(inp, "beast", "Brown bear")
    if (!beast %in% beasts_for_level(level)) beast <- beasts_for_level(level)[1]
    rows <- BEASTS[BEASTS$beast == beast, ]
    for (i in seq_len(nrow(rows))) {
      r <- rows[i, ]
      if (is.na(r$save_dc)) events <- c(events, list(damage_event(paste(beast, r$attack), r$n_dice, r$die, r$flat, attacks = r$n_attacks, attack_bonus = r$to_hit)))
      else events <- c(events, list(damage_event(paste(beast, r$attack), r$n_dice, r$die, r$flat, requires_hit = FALSE, save_dc = r$save_dc,
                                                 save_bonus = g(inp, "t_save", 2), half_on_save = isTRUE(r$save_half),
                                                 p_resource = hit_probs(rows$to_hit[!is.na(rows$to_hit) & rows$attack == "Sting"][1] %||% 4, g(inp, "target_ac", 15))[["hit"]])))
    }
    ac <- rows$ac[1]; extra$hp_override <- rows$hp[1]
    notes <- c(notes, sprintf("Wild Shape: %s (CR %s, AC %d, %d HP). Beast attack bonuses used; rounds-to-drop uses the beast's HP.", beast, rows$cr[1], rows$ac[1], rows$hp[1]))
    if (any(!is.na(rows$save_dc))) notes <- c(notes, "Poison rider applied with probability equal to the sting's hit chance.")
  } else if (routine == "Shillelagh") {
    inp$weapon <- "Quarterstaff"
    prof <- weapon_profile(inp, "Druid", pb, force_ability = "wis")
    prof$n_dice <- 1; prof$die <- 8; prof$name <- "Shillelagh quarterstaff"
    events <- c(events, list(prof$ev()))
    if (sc == "Spores" && level >= 2 && isTRUE(g(inp, "symbiotic", TRUE)))
      events <- c(events, list(damage_event("Symbiotic Entity (+1d6 necrotic)", 1, 6, 0, per_hit = TRUE, rides = "weapon")))
  } else {
    fire_bonus <- if (sc == "Wildfire" && level >= 6 && CANTRIPS$damage_type[CANTRIPS$name == routine] == "fire") 4.5 else 0
    events <- c(events, cantrip_events(routine, level, wis, pb, inp, per_cast_flat = fire_bonus))
    if (fire_bonus > 0) notes <- c(notes, "Enhanced Bond: +1d8 (mean 4.5) added to the fire cantrip.")
  }
  if (sc == "Spores" && level >= 2) { p <- g(inp, "halo_p", 0.8)
    die <- if (level >= 14) 10 else if (level >= 10) 8 else if (level >= 6) 6 else 4
    nd <- if (isTRUE(g(inp, "symbiotic", TRUE))) 2 else 1
    if (p > 0) events <- c(events, list(damage_event("Halo of Spores (reaction)", nd, die, 0, requires_hit = FALSE, save_dc = 8 + pb + wis,
                                                     save_bonus = g(inp, "t_save", 2), p_resource = p)))
    notes <- c(notes, "Halo of Spores slider = fraction of rounds an enemy starts within 10 ft.") }
  if (sc == "Stars" && level >= 2 && isTRUE(g(inp, "starry", TRUE)))
    events <- c(events, list(damage_event("Starry Form: Archer", if (level >= 10) 2 else 1, 8, wis, attack_bonus = wis + pb, attack_kind = "spell")))
  if (sc == "Stars" && level >= 14) resist <- PHYSICAL_TYPES
  if (!sc %in% c("Moon", "Spores", "Stars", "Wildfire")) notes <- c(notes, sprintf("%s: no features that change single-target sustained DPR.", sc))
  if (is.null(ac)) ac <- caster_ac(inp, "Hide")
  b <- finish_build("Druid", inp, events, prof, "flat", 20, ac, resist = resist, notes = notes, extra = extra)
  if (is.null(prof)) b$attack_bonus <- if (routine == "Wild Shape" && length(events)) events[[1]]$attack_bonus else wis + pb
  b
}

# 
# Artificer ----------------------------------------------------------------------
# 
artificer_build <- function(inp) {
  level <- inp$level; sc <- g(inp, "subclass", "Battle Smith"); pb <- proficiency_bonus(level)
  int <- g(inp, "int", 4); notes <- character(); events <- list(); prof <- NULL
  attacks <- if (level >= 5 && sc %in% c("Battle Smith", "Armorer")) 2 else 1
  if (sc == "Battle Smith" && level >= 3) {
    prof <- weapon_profile(inp, "Artificer", pb, force_ability = "int")
    prof$mod <- int; prof$base_to_hit <- int + pb + g(inp, "magic", 0); prof$to_hit <- prof$base_to_hit - (if (prof$gwm_melee) 5 else 0)
    prof$flat <- int + g(inp, "magic", 0) + (if (prof$gwm_melee) 10 else 0)
    events <- c(events, list(prof$ev(attacks = attacks)))
    if (isTRUE(g(inp, "command", TRUE)))
      events <- c(events, list(damage_event("Steel Defender rend", 1, 8, pb, attack_bonus = int + pb, attack_kind = "companion")))
    if (level >= 9) { p <- g(inp, "jolt_p", 0.5)
      if (p > 0) events <- c(events, list(damage_event("Arcane Jolt", if (level >= 15) 4 else 2, 6, 0, once_per_turn = TRUE, rides = c("weapon", "companion"), p_resource = p))) }
    notes <- c(notes, "Battle Ready: INT for attacks with a magic weapon (infused). Steel Defender commanded with your bonus action; Arcane Jolt may ride its hit.")
  } else if (sc == "Armorer" && level >= 3) {
    model <- g(inp, "armor_model", "Guardian")
    if (model == "Guardian") events <- c(events, list(damage_event("Thunder Gauntlets", 1, 8, int, attacks = attacks, attack_bonus = int + pb + g(inp, "magic", 0))))
    else { events <- c(events, list(damage_event("Lightning Launcher", 1, 6, int, attacks = attacks, attack_bonus = int + pb + g(inp, "magic", 0)),
                                    damage_event("Lightning Launcher (+1d6 once per turn)", 1, 6, 0, once_per_turn = TRUE, rides = "weapon"))) }
  } else {
    cantrip <- g(inp, "routine", "Fire Bolt")
    if (!cantrip %in% cantrips_for("Artificer")) cantrip <- "Fire Bolt"
    bonus <- 0
    if (sc == "Artillerist" && level >= 5) { bonus <- bonus + 4.5; notes <- c(notes, "Arcane Firearm: +1d8 (mean 4.5) to one spell damage roll per cast.") }
    if (sc == "Alchemist" && level >= 5 && CANTRIPS$damage_type[CANTRIPS$name == cantrip] %in% c("acid", "fire", "necrotic", "poison")) bonus <- bonus + max(int, 1)
    events <- c(events, cantrip_events(cantrip, level, int, pb, inp, per_cast_flat = bonus))
    if (sc == "Artillerist" && level >= 3 && isTRUE(g(inp, "cannon", TRUE)))
      events <- c(events, list(damage_event("Eldritch Cannon (force ballista)", if (level >= 9) 3 else 2, 8, 0, attack_bonus = int + pb, attack_kind = "spell")))
    if (sc == "Battle Smith" || sc == "Armorer") notes <- c(notes, "Subclass features start at 3rd level; cantrip routine shown.")
  }
  ac <- caster_ac(inp, "Scale mail")
  b <- finish_build("Artificer", inp, events, prof, "flat", 20, ac, notes = notes)
  if (is.null(prof)) b$attack_bonus <- int + pb
  b
}

# 
# Wizard -------------------------------------------------------------------------
# 
wizard_build <- function(inp) {
  level <- inp$level; sc <- g(inp, "subclass", "Evocation"); pb <- proficiency_bonus(level)
  int <- g(inp, "int", 4); notes <- character(); events <- list(); prof <- NULL; misc_ac <- 0
  routine <- g(inp, "routine", "Fire Bolt")
  blades <- sc == "Bladesinging" && level >= 2 && isTRUE(g(inp, "bladesong", TRUE))
  if (sc == "Bladesinging" && routine %in% c("Weapon", "Weapon + cantrip")) {
    prof <- weapon_profile(inp, "Wizard", pb)
    attacks <- if (level >= 6) 2 else 1
    victory <- if (level >= 14 && blades) max(int, 1) else 0
    if (routine == "Weapon + cantrip" && level >= 6) {
      cant <- g(inp, "cantrip", "Booming Blade")
      events <- c(events, list(prof$ev(attacks = 1, extra_flat = victory)))
      if (CANTRIPS$kind[CANTRIPS$name == cant] == "weapon") { wc <- weapon_cantrip_events(cant, level, prof, prof$mod, g(inp, "extra_targets", 0)); wc[[1]]$flat <- wc[[1]]$flat + victory
        events <- c(events, wc) }
      else events <- c(events, cantrip_events(cant, level, int, pb, inp))
      notes <- c(notes, "Bladesinger Extra Attack: one weapon attack + one cantrip in place of the second attack.")
    } else events <- c(events, list(prof$ev(attacks = attacks, extra_flat = victory)))
    if (blades) misc_ac <- max(int, 1)
  } else {
    if (!routine %in% cantrips_for("Wizard")) routine <- "Fire Bolt"
    c0 <- CANTRIPS[CANTRIPS$name == routine, ]
    bonus <- if (sc == "Evocation" && level >= 10 && c0$school == "evocation") max(int, 1) else 0
    half  <- if (sc == "Evocation" && level >= 6 && c0$kind == "save") TRUE else NULL
    events <- c(events, cantrip_events(routine, level, int, pb, inp, per_cast_flat = bonus, half_on_save = half))
    if (sc == "Bladesinging" && blades) misc_ac <- max(int, 1)
    if (!sc %in% c("Evocation", "Bladesinging")) notes <- c(notes, sprintf("%s: no features that change cantrip DPR; leveled spells out of scope.", sc))
  }
  ac <- caster_ac(inp, "Mage Armor", shield_ok = FALSE, misc = misc_ac)
  b <- finish_build("Wizard", inp, events, prof, "flat", 20, ac, notes = c(notes, "Wizard: cantrip-only routine is a floor, not a characterization of the class."))
  if (is.null(prof)) b$attack_bonus <- int + pb
  b
}

# 
# Sorcerer -----------------------------------------------------------------------
# 
sorcerer_build <- function(inp) {
  level <- inp$level; sc <- g(inp, "subclass", "Draconic"); pb <- proficiency_bonus(level)
  cha <- g(inp, "cha", 4); notes <- character()
  routine <- g(inp, "routine", "Fire Bolt"); if (!routine %in% cantrips_for("Sorcerer")) routine <- "Fire Bolt"
  bonus <- if (sc == "Draconic" && level >= 6 && isTRUE(g(inp, "affinity", TRUE))) cha else 0
  events <- cantrip_events(routine, level, cha, pb, inp, per_cast_flat = bonus)
  p_q <- if (level >= 3) g(inp, "quicken_p", 0.3) else 0
  if (p_q > 0) { qe <- cantrip_events(routine, level, cha, pb, inp, per_cast_flat = bonus, p_resource = p_q, label = paste(routine, "(Quickened)"))
    qe <- map(qe, function(e) { e$pool <- "sorcery"; e$pool_cost <- 2; e }); events <- c(events, qe) }
  if (bonus > 0) notes <- c(notes, "Elemental Affinity: +CHA assumed to apply (cantrip matches the dragon type).")
  notes <- c(notes, "Quickened Spell slider = fraction of rounds 2 sorcery points buy a second cantrip.",
             "Sorcerer: cantrip-only routine is a floor, not a characterization of the class.")
  ac <- if (sc == "Draconic") 13 + g(inp, "dex", 2) else caster_ac(inp, "Mage Armor", shield_ok = FALSE)
  b <- finish_build("Sorcerer", inp, events, NULL, "flat", 20, ac, notes = notes, extra = list(pools = list(sorcery = if (level >= 2) level else 0)))
  b$attack_bonus <- cha + pb
  b
}
