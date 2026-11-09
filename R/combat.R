# 
# combat.R — multi-round combat simulation with state ------------------------------
# 
# simulate_combat() runs n combats in parallel (vectorised over combats) for up to
# max_rounds rounds. Unlike simulate_round() it tracks:
#   - target HP (damage is truncated at the kill; the combat ends)
#   - character HP against the enemy's attacks (the combat ends when it drops)
#   - resource pools (events with `pool` are used greedily while the pool lasts,
#     instead of the per-round sliders)
#   - concentration (Hex / Hunter's Mark / Favored Foe end on a failed CON save)
#   - a first-round nova: Action Surge, Fighting Spirit, Assassinate, Dread Ambusher
#   - target-HP-dependent effects: Colossus Slayer needs a damaged target; Toll the
#     Dead uses d8 on an undamaged target
# Policy note: pools are spent as early as possible ("use it while you have it").
# 

simulate_combat <- function(build, enemy_attacks, target_ac, target_hp, char_hp,
                            enemy_roll_mode = "flat", n = 5000, max_rounds = 20, seed = NULL,
                            nova = TRUE, surprised = FALSE, con_save = 2, war_caster = FALSE,
                            e_melee = TRUE) {
  if (!is.null(seed)) set.seed(seed)
  if (inherits(enemy_attacks, "enemy_attack")) enemy_attacks <- list(enemy_attacks)
  events <- build$events
  attack_bonus <- build$attack_bonus; roll_mode0 <- build$roll_mode; crit_min0 <- build$crit_min
  to_hit_die <- build$to_hit_die %||% 0

  # ---- state --------------------------------------------------------------------------
  t_hp <- rep(target_hp, n); c_hp <- rep(char_hp, n)
  alive_t <- rep(TRUE, n); alive_c <- rep(TRUE, n)
  kill_round <- rep(NA_integer_, n); drop_round <- rep(NA_integer_, n)
  pools <- lapply(build$pools %||% list(), function(v) rep(v, n))
  conc_up <- rep(TRUE, n)
  dealt_total <- numeric(n)

  per_attack <- keep(events, ~ .x$requires_hit && !.x$once_per_turn && !.x$per_hit)
  per_hit    <- keep(events, ~ .x$requires_hit && .x$per_hit)
  riders     <- keep(events, ~ .x$requires_hit &&  .x$once_per_turn && !.x$per_hit)
  auto       <- keep(events, ~ !.x$requires_hit)
  scope_key <- function(ev) if (!is.null(ev$rides_label)) paste0("label:", ev$rides_label[1]) else paste(sort(ev$rides), collapse = "+")
  scope_defs <- list(); for (rv in riders) scope_defs[[scope_key(rv)]] <- list(kinds = rv$rides, label = rv$rides_label)
  scopes_for_attack <- function(ev) names(scope_defs)[map_lgl(scope_defs, function(d)
    if (!is.null(d$label)) ev$label %in% d$label else ev$attack_kind %in% d$kinds)]

  # availability of an event this round: pool-limited (greedy) or slider
  avail_of <- function(ev, n_uses = 1) {
    if (!is.null(ev$pool) && !is.null(pools[[ev$pool]])) {
      ok <- pools[[ev$pool]] >= ev$pool_cost
      pools[[ev$pool]][ok] <<- pools[[ev$pool]][ok] - ev$pool_cost
      ok
    } else runif(n) < ev$p_resource
  }
  is_conc <- function(ev) isTRUE(ev$concentration)

  for (round in seq_len(max_rounds)) {
    active <- alive_t & alive_c
    if (!any(active)) break
    # ---- nova adjustments for round 1 -------------------------------------------------
    roll_mode <- roll_mode0; crit_min <- crit_min0
    if (nova && round == 1 && !is.null(build$nova)) {
      if (isTRUE(build$nova$advantage) && roll_mode == "flat") roll_mode <- "advantage"
      if (isTRUE(build$nova$autocrit_if_surprised) && surprised) crit_min <- 1
    }
    round_events <- per_attack
    if (nova && round == 1 && length(build$nova_events)) round_events <- c(round_events, build$nova_events)

    dmg <- numeric(n)
    any_hit <- lapply(scope_defs, function(d) logical(n)); first_crit <- any_hit
    any_crit <- logical(n); any_miss <- logical(n)
    base_crit <- NULL; base_miss <- NULL
    ph_avail <- lapply(per_hit, function(r) if (r$resource_scope == "hit") NULL else avail_of(r))
    ordered <- c(keep(round_events, ~ .x$trigger == "none"), keep(round_events, ~ .x$trigger != "none"))
    for (ev in ordered) {
      if (is_conc(ev) && !any(conc_up)) next
      if (ev$trigger != "none" && is.null(base_crit)) { base_crit <- any_crit; base_miss <- any_miss }
      gate <- switch(ev$trigger, none = rep(TRUE, n), prior_crit = base_crit, prior_miss = base_miss)
      # nova: Action Surge is used in round 1 if the pool allows
      avail_round <- if (nova && round == 1 && identical(ev$pool, "surge")) avail_of(ev)
                     else if (ev$resource_scope == "round") avail_of(ev) else NULL
      for (j in seq_len(ev$attacks)) {
        avail <- if (is.null(avail_round)) avail_of(ev) else avail_round
        made <- avail & gate & active & alive_t
        bless <- if (to_hit_die > 0) sample.int(to_hit_die, n, replace = TRUE) else 0
        a <- resolve_attack(n, (ev$attack_bonus %||% attack_bonus) + bless, target_ac,
                            ev$roll_mode %||% roll_mode, ev$crit_min %||% crit_min)
        if (isTRUE(ev$p_hit_add > 0)) { p0 <- .event_probs(list(attack_bonus = ev$attack_bonus, roll_mode = ev$roll_mode, crit_min = ev$crit_min, p_hit_add = 0),
                                                          attack_bonus, target_ac, roll_mode, crit_min, to_hit_die)
          conv <- min(ev$p_hit_add, 1 - p0[["hit"]]) / max(1 - p0[["hit"]], 1e-12); a$hit <- a$hit | (!a$hit & runif(n) < conv) }
        hit <- a$hit & made; crit <- a$crit & made
        dice <- roll_dice(n, ev$n_dice, ev$die, ev$gwf) + ifelse(crit, roll_dice(n, ev$n_dice + ev$crit_dice_bonus, ev$die, ev$gwf), 0)
        d <- ifelse(hit, dice + ev$flat, 0)
        for (k in seq_along(per_hit)) {
          r <- per_hit[[k]]
          if (!(ev$attack_kind %in% r$rides)) next
          if (!is.null(r$rides_label) && !(ev$label %in% r$rides_label)) next
          if (is_conc(r)) ra <- conc_up else ra <- if (is.null(ph_avail[[k]])) (if (r$crit_only) crit else hit) & avail_of(r) else ph_avail[[k]]
          on <- ra & (if (r$crit_only) crit else hit)
          rd <- roll_dice(n, r$n_dice, r$die, r$gwf) + ifelse(crit, roll_dice(n, r$n_dice, r$die, r$gwf), 0)
          d <- d + ifelse(on, rd + r$flat, 0)
        }
        # apply to target, truncating at the kill
        d <- pmin(d, t_hp); t_hp <- t_hp - d; dmg <- dmg + d
        for (sc in scopes_for_attack(ev)) { newly <- hit & !any_hit[[sc]]; first_crit[[sc]][newly] <- crit[newly]; any_hit[[sc]] <- any_hit[[sc]] | hit }
        any_crit <- any_crit | crit; any_miss <- any_miss | (made & !a$hit)
        newly_dead <- alive_t & t_hp <= 0; kill_round[newly_dead] <- round; alive_t <- alive_t & t_hp > 0
      }
    }
    for (ev in riders) {
      sc <- scope_key(ev)
      if (grepl("Colossus Slayer", ev$label) && round == 1) next          # target still at full HP
      avail <- if (is_conc(ev)) conc_up else avail_of(ev)
      dice <- roll_dice(n, ev$n_dice, ev$die, ev$gwf) + ifelse(first_crit[[sc]], roll_dice(n, ev$n_dice, ev$die, ev$gwf), 0)
      d <- ifelse(any_hit[[sc]] & avail & active, dice + ev$flat, 0)
      d <- pmin(d, t_hp); t_hp <- t_hp - d; dmg <- dmg + d
    }
    for (ev in auto) {
      avail <- avail_of(ev)
      for (j in seq_len(ev$attacks)) {
        die <- if (!is.null(ev$undamaged_die) && round == 1) ev$undamaged_die else ev$die
        d <- roll_dice(n, ev$n_dice, die, ev$gwf) + ev$flat
        if (!is.null(ev$save_dc)) { saved <- (sample.int(20, n, replace = TRUE) + ev$save_bonus) >= ev$save_dc
          d <- ifelse(saved, if (ev$half_on_save) floor(d / 2) else 0, d) }
        d <- ifelse(avail & active & alive_t, d, 0)
        d <- pmin(d, t_hp); t_hp <- t_hp - d; dmg <- dmg + d
      }
    }
    newly_dead <- alive_t & t_hp <= 0; kill_round[newly_dead] <- round; alive_t <- alive_t & t_hp > 0
    dealt_total <- dealt_total + dmg

    # ---- enemy's turn (only if still alive) ---------------------------------------------
    taken <- numeric(n); reaction_used <- logical(n); conc_fail <- logical(n)
    for (ea in enemy_attacks) {
      mult <- if (ea$damage_type %in% build$immune) 0 else if (ea$damage_type %in% build$resist) 0.5 else 1
      rflat <- if (ea$damage_type %in% PHYSICAL_TYPES) (build$flat_reduction %||% 0) else 0
      for (j in seq_len(ea$n_attacks)) {
        a <- resolve_attack(n, ea$attack_bonus, build$ac, enemy_roll_mode)
        dice <- roll_dice(n, ea$n_dice, ea$die) + ifelse(a$crit, roll_dice(n, ea$n_dice, ea$die), 0)
        d <- pmax(dice + ea$flat - rflat, 0); d <- if (mult == 0.5) floor(d / 2) else d * mult
        if (!is.null(build$first_hit) && (e_melee || build$first_hit$mode == "reduce")) {
          use <- a$hit & !reaction_used
          d[use] <- if (build$first_hit$mode == "halve") floor(d[use] / 2) else pmax(d[use] - build$first_hit$amount * mult, 0)
          reaction_used <- reaction_used | use }
        d <- ifelse(a$hit & active & alive_t & alive_c, d, 0)
        # concentration check on each damaging hit
        if (any(conc_up) && length(keep(events, is_conc))) {
          dc <- pmax(10, floor(d / 2))
          r1 <- sample.int(20, n, replace = TRUE); r2 <- sample.int(20, n, replace = TRUE)
          roll <- if (war_caster) pmax(r1, r2) else r1
          conc_fail <- conc_fail | (d > 0 & (roll + con_save) < dc)
        }
        taken <- taken + d
      }
    }
    conc_up <- conc_up & !conc_fail
    c_hp <- c_hp - taken
    newly_down <- alive_c & c_hp <= 0; drop_round[newly_down] <- round; alive_c <- alive_c & c_hp > 0
  }
  win <- !is.na(kill_round) & (is.na(drop_round) | kill_round <= drop_round)
  list(kill_round = kill_round, drop_round = drop_round, win = win, dealt_total = dealt_total,
       target_hp_left = pmax(t_hp, 0), char_hp_left = pmax(c_hp, 0),
       summary = tibble(p_win = mean(win), p_kill_within_max = mean(!is.na(kill_round)),
                        kill_round_median = median(kill_round, na.rm = TRUE), kill_round_mean = mean(kill_round, na.rm = TRUE),
                        p_drop_within_max = mean(!is.na(drop_round)), drop_round_median = median(drop_round, na.rm = TRUE),
                        char_hp_left_mean = mean(pmax(c_hp, 0)[win]), max_rounds = max_rounds))
}
