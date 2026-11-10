# 
# engine.R — class-agnostic DPR engine (D&D 5e, 2014 rules) --------------------
#
# A round is a list of *damage events*. The engine knows nothing about
# classes; class features are translated into events elsewhere
# (build_events.R, Phase 2). See "Shiny DPR app — plan.md".
#
# Offense:  round_dpr(events, attack_bonus, target_ac, ...)
# Defense:  damage_taken(enemy_attacks, ac, ...)

suppressPackageStartupMessages({   # attach messages land in the browser console as "errors" under webR
  library(magrittr); library(dplyr); library(purrr); library(tibble)
})

ROLL_MODES <- c("flat", "advantage", "disadvantage", "elven_accuracy")

# 
# Dice helpers -----------------------------------------------------------------

# Mean of one die. `gwf = TRUE` applies the Great Weapon Fighting style
# (reroll a 1 or 2 once, keep the new roll).
die_mean <- function(die, gwf = FALSE) {
  if (die <= 0) return(0)
  if (!gwf) return((die + 1) / 2)
  faces <- seq_len(die)
  mean(ifelse(faces <= 2, (die + 1) / 2, faces))
}

# 
# Hit / crit probabilities for a single attack roll ----------------------------
#   attack_bonus : ability mod + proficiency (+ magic, −5 for GWM, …)
#   target_ac    : defender's AC
#   roll_mode    : "flat", "advantage", "disadvantage", "elven_accuracy"
#   crit_min     : lowest d20 face that crits (20 normally, 19 Champion, 18 at 15th)
#
# A natural 1 always misses and a natural 20 always hits, so the flat hit
# probability is clamped to [0.05, 0.95] and never below the crit chance.
hit_probs <- function(attack_bonus, target_ac, roll_mode = "flat", crit_min = 20) {
  roll_mode <- match.arg(roll_mode, ROLL_MODES)
  need   <- target_ac - attack_bonus
  p_hit  <- (21 - need) / 20
  p_hit  <- min(max(p_hit, 0.05), 0.95)
  p_crit <- (21 - crit_min) / 20
  p_hit  <- max(p_hit, p_crit)

  transform <- switch(roll_mode,
    flat           = identity,
    advantage      = function(p) 1 - (1 - p)^2,
    disadvantage   = function(p) p^2,
    elven_accuracy = function(p) 1 - (1 - p)^3
  )
  p_hit  <- transform(p_hit)
  p_crit <- transform(p_crit)
  c(hit = p_hit, crit = p_crit, noncrit = p_hit - p_crit)
}

# 
# Damage events ----------------------------------------------------------------
# One row of the offensive formula. Three kinds:
#   requires_hit = TRUE,  once_per_turn = FALSE  -> per-attack damage
#                                                  (weapon attacks, Divine Strike, …)
#   requires_hit = TRUE,  once_per_turn = TRUE   -> rider on the first hit of the turn
#                                                  (Sneak Attack, Divine Fury, Hex, Hunter's Mark)
#   requires_hit = FALSE                         -> automatic or save-based damage
#
# Fields
#   n_dice, die, flat   : damage expression, e.g. 1d12 + 6  -> n_dice=1, die=12, flat=6
#   attacks             : how many times a per-attack event occurs per round
#   crit_dice_bonus     : extra dice on a crit beyond the normal doubling (Brutal Critical)
#   p_resource          : fraction of rounds the event is available (smites, maneuvers);
#                         the Phase 1 "slider" decision
#   gwf                 : apply Great Weapon Fighting reroll to this event's dice
#   attack_bonus, roll_mode, crit_min : per-event overrides (NULL = inherit from round),
#                         e.g. a bonus-action attack without the main-hand bonus
#   save_dc, save_bonus, half_on_save : for requires_hit = FALSE events that force a save
#   per_hit             : rider added to EVERY real attack that hits (Divine Smite on each hit,
#                         Improved Divine Smite, Hunter's Mark, Elemental Cleaver). Not an attack
#                         itself: it rides the weapon attack's roll; `attacks` is ignored.
#   crit_only           : per-hit rider only used on a crit (smite-on-crit policy); implies per_hit
#   trigger             : "prior_crit" = happens only if an earlier attack this round crit
#                         (GWM bonus attack); "prior_miss" = only if one missed (Stalker's Flurry)
#   resource_scope      : "round" = p_resource is drawn once per round (Action Surge);
#                         "hit"   = drawn per attack (smite on each hit). Same mean, different spread.
#   attack_kind         : for real attacks: "weapon" or "spell" (Eldritch Blast, Spiritual Weapon)
#   rides               : for per_hit / once_per_turn riders: which attack kinds they can ride.
#                         Divine Strike rides "weapon" only; Hex rides both.
#   rides_label         : restrict a rider to attacks with these labels (Booming Blade's
#                         extra dice ride only the Booming Blade attack).
#   p_hit_add           : extra non-crit hit probability from miss-conversion effects
#                         (Homing Strikes, Lucky); clamped so p_hit <= 1.
#   pool, pool_cost     : (combat simulation only) named resource pool this event spends
#                         from, and how much per use; round_dpr() ignores these and uses
#                         p_resource. simulate_combat() uses the pool greedily instead.
#   concentration       : (combat simulation only) the event ends when concentration breaks
#   undamaged_die       : (combat simulation only) die used while the target is at full HP
#                         (Toll the Dead d8 -> d12)
damage_event <- function(label, n_dice = 0, die = 0, flat = 0,
                         attacks = 1,
                         requires_hit = TRUE,
                         once_per_turn = FALSE,
                         crit_dice_bonus = 0,
                         p_resource = 1,
                         gwf = FALSE,
                         attack_bonus = NULL, roll_mode = NULL, crit_min = NULL,
                         save_dc = NULL, save_bonus = 0, half_on_save = FALSE,
                         per_hit = FALSE, crit_only = FALSE,
                         trigger = c("none", "prior_crit", "prior_miss"),
                         resource_scope = c("round", "hit"),
                         attack_kind = c("weapon", "spell", "companion"),
                         rides = c("weapon", "spell"),
                         rides_label = NULL, p_hit_add = 0,
                         pool = NULL, pool_cost = 1, concentration = FALSE, undamaged_die = NULL) {
  stopifnot(p_resource >= 0, p_resource <= 1)
  trigger <- match.arg(trigger); resource_scope <- match.arg(resource_scope)
  attack_kind <- match.arg(attack_kind); rides <- match.arg(rides, c("weapon", "spell", "companion"), several.ok = TRUE)
  if (crit_only) per_hit <- TRUE
  structure(list(label = label, n_dice = n_dice, die = die, flat = flat,
                 attacks = attacks, requires_hit = requires_hit,
                 once_per_turn = once_per_turn, crit_dice_bonus = crit_dice_bonus,
                 p_resource = p_resource, gwf = gwf,
                 attack_bonus = attack_bonus, roll_mode = roll_mode, crit_min = crit_min,
                 save_dc = save_dc, save_bonus = save_bonus, half_on_save = half_on_save,
                 per_hit = per_hit, crit_only = crit_only, trigger = trigger,
                 resource_scope = resource_scope, attack_kind = attack_kind, rides = rides,
                 rides_label = rides_label, p_hit_add = p_hit_add,
                 pool = pool, pool_cost = pool_cost, concentration = concentration, undamaged_die = undamaged_die),
            class = "damage_event")
}

# probabilities for one event, honouring per-event overrides
.event_probs <- function(ev, attack_bonus, target_ac, roll_mode, crit_min, to_hit_die = 0) {
  one <- function(k) hit_probs(attack_bonus = (ev$attack_bonus %||% attack_bonus) + k,
                               target_ac    = target_ac,
                               roll_mode    = ev$roll_mode %||% roll_mode,
                               crit_min     = ev$crit_min %||% crit_min)
  # Bless-style bonus die on the attack roll: average the probabilities over its faces
  p <- if (to_hit_die > 0) Reduce(`+`, lapply(seq_len(to_hit_die), one)) / to_hit_die else one(0)
  if (isTRUE(ev$p_hit_add > 0)) {
    add <- min(ev$p_hit_add, 1 - p[["hit"]])
    p[["hit"]] <- p[["hit"]] + add; p[["noncrit"]] <- p[["noncrit"]] + add
  }
  p
}

# 
# Offensive DPR for a whole round ----------------------------------------------
# Returns list(total, breakdown) where breakdown is one tibble row per event.
#
# Per-attack events:
#   A * p_res * [ (p_hit - p_crit) * D_hit + p_crit * D_crit ],
#   D_crit = D_hit + (n_dice + crit_dice_bonus) * die_mean
#
# Once-per-turn riders ride the first hit among the attacks they may ride. The
# first-hit probability and the chance that the first hit was a crit are computed
# EXACTLY by walking the attacks in order (see first_hit_walk), including the
# conditional occurrence of triggered attacks.
#   P(any hit) = 1 - prod_i (1 - p_hit_i)^(A_i)
#   q = P(first hit is a crit) ~= sum_i A_i p_crit_i / sum_i A_i p_hit_i
#   p_res * P(any hit) * [ (1 - q) * D_hit + q * (D_hit + n_dice * die_mean) ]
#
# Save-based events (requires_hit = FALSE, save_dc given):
#   p_fail = clamp((save_dc - save_bonus - 1) / 20, 0, 1)
#   A * p_res * D * [ p_fail + (1 - p_fail) * (0.5 if half_on_save else 0) ]
#   to_hit_die : 0, or 4 for Bless (d4 added to every attack roll)
round_dpr <- function(events, attack_bonus, target_ac,
                      roll_mode = "flat", crit_min = 20, to_hit_die = 0) {
  if (inherits(events, "damage_event")) events <- list(events)
  roll_mode <- match.arg(roll_mode, ROLL_MODES)

  per_attack <- keep(events, ~ .x$requires_hit && !.x$once_per_turn && !.x$per_hit)
  per_hit    <- keep(events, ~ .x$requires_hit && .x$per_hit)
  riders     <- keep(events, ~ .x$requires_hit &&  .x$once_per_turn && !.x$per_hit)
  auto       <- keep(events, ~ !.x$requires_hit)

  # --- real attacks: untriggered first, so triggered ones can condition on them
  base_pa <- keep(per_attack, ~ .x$trigger == "none")
  trig_pa <- keep(per_attack, ~ .x$trigger != "none")

  pa_row <- function(ev, mult = 1) {
    p  <- .event_probs(ev, attack_bonus, target_ac, roll_mode, crit_min, to_hit_die)
    dm <- die_mean(ev$die, ev$gwf)
    d_hit  <- ev$n_dice * dm + ev$flat
    d_crit <- d_hit + (ev$n_dice + ev$crit_dice_bonus) * dm
    tibble(label = ev$label,
           type = paste0("attack", if (ev$trigger != "none") paste0(" [", ev$trigger, "]") else ""),
           kind = ev$attack_kind,
           attacks = ev$attacks * mult,
           p_hit = p[["hit"]], p_crit = p[["crit"]],
           d_hit = d_hit, d_crit = d_crit, p_resource = ev$p_resource,
           expected = ev$attacks * mult * ev$p_resource *
             (p[["noncrit"]] * d_hit + p[["crit"]] * d_crit))
  }
  empty_rows <- tibble(label = character(), type = character(), kind = character(), attacks = numeric(),
                       p_hit = numeric(), p_crit = numeric(), d_hit = numeric(), d_crit = numeric(),
                       p_resource = numeric(), expected = numeric())
  pa_rows <- bind_rows(empty_rows, map_dfr(base_pa, pa_row))
  # an attack that is not made (p_resource) can neither crit nor miss
  if (nrow(pa_rows) > 0) {
    p_any_crit <- 1 - prod((1 - pa_rows$p_resource * pa_rows$p_crit)^pa_rows$attacks)
    p_any_miss <- 1 - prod((1 - pa_rows$p_resource * (1 - pa_rows$p_hit))^pa_rows$attacks)
  } else { p_any_crit <- 0; p_any_miss <- 0 }
  pa_rows <- bind_rows(pa_rows, map_dfr(trig_pa, function(ev)
    pa_row(ev, mult = if (ev$trigger == "prior_crit") p_any_crit else p_any_miss)))

  # effective number of attack rolls per round (triggered ones weighted by trigger prob.)
  # and their (resource-weighted) hit / crit probabilities
  n_eff <- if (nrow(pa_rows)) pa_rows$attacks * pa_rows$p_resource else numeric()

  # --- per-hit riders: sum over every real attack of P(hit) * rider damage ------
  ph_rows <- map_dfr(per_hit, function(ev) {
    dm <- die_mean(ev$die, ev$gwf)
    d_hit  <- ev$n_dice * dm + ev$flat
    d_crit <- d_hit + ev$n_dice * dm
    rows <- pa_rows[pa_rows$kind %in% ev$rides, , drop = FALSE]
    if (!is.null(ev$rides_label)) rows <- rows[rows$label %in% ev$rides_label, , drop = FALSE]
    ne <- rows$attacks * rows$p_resource
    per_attack_exp <- if (ev$crit_only) rows$p_crit * d_crit
                      else (rows$p_hit - rows$p_crit) * d_hit + rows$p_crit * d_crit
    tibble(label = ev$label, type = if (ev$crit_only) "per-hit rider (crit only)" else "per-hit rider",
           kind = paste(ev$rides, collapse = "+"),
           attacks = sum(ne),
           p_hit = if (sum(ne)) sum(ne * rows$p_hit) / sum(ne) else 0,
           p_crit = if (sum(ne)) sum(ne * rows$p_crit) / sum(ne) else 0,
           d_hit = d_hit, d_crit = d_crit, p_resource = ev$p_resource,
           expected = ev$p_resource * sum(ne * per_attack_exp))
  })

  # --- once-per-turn riders: ride the first hit among the real attacks ---------
  # Exact first-hit walk. Attacks happen in list order; an attack that is not made
  # (p_resource) cannot be the first hit. Triggered attacks come after all base attacks:
  #   prior_crit : only happens when a base attack crit, i.e. a hit already landed, so it
  #                can never carry the first hit;
  #   prior_miss : can carry the first hit only if no base attack hit AND at least one
  #                base attack was made and missed: P = P(no hit) - P(no attack made).
  first_hit_walk <- function(rows_base, rows_trig) {
    p_none <- 1; p_first <- 0; p_first_crit <- 0; p_no_attack <- 1
    for (i in seq_len(nrow(rows_base))) for (j in seq_len(rows_base$attacks[i])) {
      r <- rows_base[i, ]
      p_first      <- p_first      + p_none * r$p_resource * r$p_hit
      p_first_crit <- p_first_crit + p_none * r$p_resource * r$p_crit
      p_none       <- p_none * (1 - r$p_resource * r$p_hit)
      p_no_attack  <- p_no_attack * (1 - r$p_resource)
    }
    for (i in seq_len(nrow(rows_trig))) {
      r <- rows_trig[i, ]
      if (grepl("prior_miss", r$type)) {
        p_gate <- max(p_none - p_no_attack, 0)
        n_tr <- r$attacks / max(p_any_miss, 1e-12)   # raw number of attacks (type weight undone)
        for (j in seq_len(round(n_tr))) {
          p_first      <- p_first      + p_gate * r$p_resource * r$p_hit
          p_first_crit <- p_first_crit + p_gate * r$p_resource * r$p_crit
          p_gate       <- p_gate * (1 - r$p_resource * r$p_hit)
        }
      }
    }
    list(p_any_hit = p_first, q_crit = if (p_first > 0) p_first_crit / p_first else 0)
  }
  rider_rows <- map_dfr(riders, function(ev) {
    rows <- pa_rows[pa_rows$kind %in% ev$rides, , drop = FALSE]
    if (!is.null(ev$rides_label)) rows <- rows[rows$label %in% ev$rides_label, , drop = FALSE]
    fw <- first_hit_walk(rows[rows$type == "attack", , drop = FALSE], rows[rows$type != "attack", , drop = FALSE])
    p_any_hit <- fw$p_any_hit; q_crit <- fw$q_crit
    dm <- die_mean(ev$die, ev$gwf)
    d_hit  <- ev$n_dice * dm + ev$flat
    d_crit <- d_hit + ev$n_dice * dm
    tibble(label = ev$label, type = "once-per-turn", kind = paste(ev$rides, collapse = "+"), attacks = NA_real_,
           p_hit = p_any_hit, p_crit = p_any_hit * q_crit,
           d_hit = d_hit, d_crit = d_crit, p_resource = ev$p_resource,
           expected = ev$p_resource * p_any_hit *
             ((1 - q_crit) * d_hit + q_crit * d_crit))
  })
  pa_rows <- bind_rows(pa_rows, ph_rows)

  # --- automatic / save-based events ---------------------------------------
  auto_rows <- map_dfr(auto, function(ev) {
    d <- ev$n_dice * die_mean(ev$die, ev$gwf) + ev$flat
    if (is.null(ev$save_dc)) {
      p_apply <- 1
    } else {
      p_fail  <- min(max((ev$save_dc - ev$save_bonus - 1) / 20, 0), 1)
      p_apply <- p_fail + (1 - p_fail) * (if (ev$half_on_save) 0.5 else 0)
    }
    tibble(label = ev$label, type = if (is.null(ev$save_dc)) "automatic" else "save", kind = NA_character_,
           attacks = ev$attacks, p_hit = p_apply, p_crit = 0,
           d_hit = d, d_crit = NA_real_, p_resource = ev$p_resource,
           expected = ev$attacks * ev$p_resource * p_apply * d)
  })

  breakdown <- bind_rows(pa_rows, rider_rows, auto_rows)
  list(total = sum(breakdown$expected), breakdown = breakdown)
}

# 
# Defensive side: expected damage taken per round ------------------------------
enemy_attack <- function(label, n_attacks = 1, attack_bonus, n_dice = 1, die = 6,
                         flat = 0, damage_type = "slashing") {
  structure(list(label = label, n_attacks = n_attacks, attack_bonus = attack_bonus,
                 n_dice = n_dice, die = die, flat = flat, damage_type = damage_type),
            class = "enemy_attack")
}

PHYSICAL_TYPES <- c("bludgeoning", "piercing", "slashing")

# damage_taken()
#   enemy_attacks : list of enemy_attack()
#   ac            : character's AC
#   roll_mode     : the ENEMY's roll mode against the character
#                   ("advantage" after Reckless Attack, "disadvantage" vs Bear Totemic Attunement)
#   resist / immune / vulnerable : character vectors of damage types
#   flat_reduction, flat_reduction_types : e.g. Heavy Armor Master (3, PHYSICAL_TYPES),
#                   applied per hit before resistance
#
#   Raw_i = n_i * [ (p_hit - p_crit) * max(D_hit - r, 0) + p_crit * max(D_crit - r, 0) ]
#   taken = sum_i m_i * Raw_i,  m = 0.5 resist, 0 immune, 2 vulnerable, 1 otherwise
#   first_hit     : NULL, or list(mode = "halve") (Uncanny Dodge) or
#                   list(mode = "reduce", amount = x) (Deflect Missiles); applied to the
#                   FIRST hit of the round, i.e. the reaction is spent as soon as it can be.
#                   Exact: sum_k P(first hit is attack k) * reduction_k.
damage_taken <- function(enemy_attacks, ac, roll_mode = "flat",
                         resist = character(), immune = character(),
                         vulnerable = character(),
                         flat_reduction = 0, flat_reduction_types = PHYSICAL_TYPES,
                         first_hit = NULL) {
  if (inherits(enemy_attacks, "enemy_attack")) enemy_attacks <- list(enemy_attacks)
  roll_mode <- match.arg(roll_mode, ROLL_MODES)

  breakdown <- map_dfr(enemy_attacks, function(ea) {
    p  <- hit_probs(ea$attack_bonus, ac, roll_mode)
    dm <- die_mean(ea$die)
    r  <- if (ea$damage_type %in% flat_reduction_types) flat_reduction else 0
    d_hit  <- max(ea$n_dice * dm + ea$flat - r, 0)
    d_crit <- max(ea$n_dice * dm * 2 + ea$flat - r, 0)
    mult <- case_when(ea$damage_type %in% immune     ~ 0,
                      ea$damage_type %in% vulnerable ~ 2,
                      ea$damage_type %in% resist     ~ 0.5,
                      TRUE                           ~ 1)
    raw <- ea$n_attacks * (p[["noncrit"]] * d_hit + p[["crit"]] * d_crit)
    tibble(label = ea$label, damage_type = ea$damage_type, attacks = ea$n_attacks,
           p_hit = p[["hit"]], p_crit = p[["crit"]], d_hit = d_hit, d_crit = d_crit,
           raw = raw, multiplier = mult, taken = raw * mult)
  })
  total <- sum(breakdown$taken)

  if (!is.null(first_hit) && nrow(breakdown) > 0) {
    # expand to one row per individual attack, in order
    per <- breakdown[rep(seq_len(nrow(breakdown)), breakdown$attacks), ]
    p_no_hit_before <- cumprod(c(1, 1 - per$p_hit))[seq_len(nrow(per))]
    e_dmg_given_hit <- ((per$p_hit - per$p_crit) * per$d_hit + per$p_crit * per$d_crit) /
                       per$p_hit * per$multiplier
    red <- if (first_hit$mode == "halve") 0.5 * e_dmg_given_hit
           else pmin(first_hit$amount * per$multiplier, e_dmg_given_hit)
    reduction <- sum(p_no_hit_before * per$p_hit * red)
    breakdown <- bind_rows(breakdown,
      tibble(label = paste0("First-hit reduction (", first_hit$mode, ")"), damage_type = NA,
             attacks = NA, p_hit = 1 - prod((1 - per$p_hit)), p_crit = NA, d_hit = NA, d_crit = NA,
             raw = -reduction, multiplier = 1, taken = -reduction))
    total <- total - reduction
  }
  list(total = total, breakdown = breakdown)
}

# 
# Small AC helpers (full AC builder arrives with defense.R) --------------------
ac_unarmored_defense <- function(dex_mod, con_mod = 0, wis_mod = 0, shield = FALSE) {
  10 + dex_mod + con_mod + wis_mod + if (shield) 2 else 0
}
