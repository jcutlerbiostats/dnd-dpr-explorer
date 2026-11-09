# 
# simulate.R — Monte Carlo companion to engine.R -------------------------------
#
# simulate_round()        : n simulated rounds of offensive damage (vector)
# simulate_damage_taken() : n simulated rounds of damage taken (vector)
# summarize_sim()         : mean / sd / quantiles / P(X >= x) of a sim vector
# rounds_to_threshold()   : distribution of rounds until cumulative damage >= hp
#
# Both simulators consume the SAME event lists as round_dpr() / damage_taken(),
# so the closed form and the simulation can always be cross-checked.
# Policy-dependent mechanics (smite-on-crit, GWM bonus attack on crit, …) will be
# added here first, because they are easier to state as a policy than as algebra.

# 
# Dice -------------------------------------------------------------------------
roll_d20 <- function(n, roll_mode = "flat") {
  roll_mode <- match.arg(roll_mode, ROLL_MODES)
  k <- switch(roll_mode, flat = 1L, advantage = 2L, disadvantage = 2L, elven_accuracy = 3L)
  m <- matrix(sample.int(20, n * k, replace = TRUE), nrow = n)
  if (roll_mode == "disadvantage") apply(m, 1, min) else apply(m, 1, max)
}

# n rows, each the sum of `n_dice` dice of size `die`; gwf rerolls 1s and 2s once
roll_dice <- function(n, n_dice, die, gwf = FALSE) {
  if (n_dice <= 0 || die <= 0) return(rep(0, n))
  m <- matrix(sample.int(die, n * n_dice, replace = TRUE), nrow = n)
  if (gwf) {
    low <- m <= 2
    m[low] <- sample.int(die, sum(low), replace = TRUE)
  }
  rowSums(m)
}

# attack resolution for n parallel attacks against one AC
# returns list(hit = logical, crit = logical)
resolve_attack <- function(n, attack_bonus, target_ac, roll_mode = "flat", crit_min = 20) {
  r    <- roll_d20(n, roll_mode)
  crit <- r >= crit_min
  hit  <- crit | (r != 1 & (r + attack_bonus) >= target_ac)
  list(hit = hit, crit = crit)
}

# 
# Offensive simulation ---------------------------------------------------------
simulate_round <- function(events, attack_bonus, target_ac,
                           roll_mode = "flat", crit_min = 20, to_hit_die = 0,
                           n = 10000, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (inherits(events, "damage_event")) events <- list(events)
  roll_mode <- match.arg(roll_mode, ROLL_MODES)

  total <- numeric(n)
  # first-hit tracking per rider scope. A scope is the set of attack kinds a rider may
  # ride, or a specific attack label; state is kept for every scope some rider uses.
  scope_key <- function(ev) if (!is.null(ev$rides_label)) paste0("label:", ev$rides_label[1]) else paste(sort(ev$rides), collapse = "+")
  rider_list <- keep(events, ~ .x$requires_hit && .x$once_per_turn && !.x$per_hit)
  scope_defs <- list(); for (rv in rider_list) scope_defs[[scope_key(rv)]] <- list(kinds = rv$rides, label = rv$rides_label)
  any_hit <- lapply(scope_defs, function(d) logical(n)); first_hit_crit <- any_hit
  scopes_for_attack <- function(ev) names(scope_defs)[map_lgl(scope_defs, function(d)
    if (!is.null(d$label)) ev$label %in% d$label else ev$attack_kind %in% d$kinds)]

  per_attack <- keep(events, ~ .x$requires_hit && !.x$once_per_turn && !.x$per_hit)
  per_hit    <- keep(events, ~ .x$requires_hit && .x$per_hit)
  riders     <- keep(events, ~ .x$requires_hit &&  .x$once_per_turn && !.x$per_hit)
  auto       <- keep(events, ~ !.x$requires_hit)

  any_crit <- logical(n); any_miss <- logical(n)
  ph_avail_round <- map(per_hit, ~ runif(n) < .x$p_resource)   # round-scope availability
  # triggered attacks condition on the outcome of the untriggered (Attack-action) attacks
  # only; the trigger state is frozen after those, matching round_dpr().
  ordered  <- c(keep(per_attack, ~ .x$trigger == "none"), keep(per_attack, ~ .x$trigger != "none"))
  base_crit <- NULL; base_miss <- NULL
  for (ev in ordered) {
    if (ev$trigger != "none" && is.null(base_crit)) { base_crit <- any_crit; base_miss <- any_miss }
    avail_round <- runif(n) < ev$p_resource
    gate <- switch(ev$trigger, none = rep(TRUE, n), prior_crit = base_crit, prior_miss = base_miss)
    for (j in seq_len(ev$attacks)) {
      avail <- if (ev$resource_scope == "hit") runif(n) < ev$p_resource else avail_round
      bless <- if (to_hit_die > 0) sample.int(to_hit_die, n, replace = TRUE) else 0
      a <- resolve_attack(n,
                          (ev$attack_bonus %||% attack_bonus) + bless, target_ac,
                          ev$roll_mode %||% roll_mode, ev$crit_min %||% crit_min)
      if (isTRUE(ev$p_hit_add > 0)) {              # miss conversion (Homing Strikes, Lucky)
        p0 <- .event_probs(list(attack_bonus = ev$attack_bonus, roll_mode = ev$roll_mode, crit_min = ev$crit_min, p_hit_add = 0),
                           attack_bonus, target_ac, roll_mode, crit_min, to_hit_die)
        conv <- min(ev$p_hit_add, 1 - p0[["hit"]]) / max(1 - p0[["hit"]], 1e-12)
        a$hit <- a$hit | (!a$hit & runif(n) < conv)
      }
      made <- avail & gate
      hit  <- a$hit & made
      crit <- a$crit & made
      dice <- roll_dice(n, ev$n_dice, ev$die, ev$gwf) +
              ifelse(crit, roll_dice(n, ev$n_dice + ev$crit_dice_bonus, ev$die, ev$gwf), 0)
      total <- total + ifelse(hit, dice + ev$flat, 0)
      # per-hit riders attach to this attack's outcome (if they ride this attack kind)
      for (k in seq_along(per_hit)) {
        r  <- per_hit[[k]]
        if (!(ev$attack_kind %in% r$rides)) next
        if (!is.null(r$rides_label) && !(ev$label %in% r$rides_label)) next
        ra <- if (r$resource_scope == "hit") runif(n) < r$p_resource else ph_avail_round[[k]]
        on <- ra & (if (r$crit_only) crit else hit)
        rd <- roll_dice(n, r$n_dice, r$die, r$gwf) + ifelse(crit, roll_dice(n, r$n_dice, r$die, r$gwf), 0)
        total <- total + ifelse(on, rd + r$flat, 0)
      }
      for (sc in scopes_for_attack(ev)) {
        newly <- hit & !any_hit[[sc]]
        first_hit_crit[[sc]][newly] <- crit[newly]
        any_hit[[sc]] <- any_hit[[sc]] | hit
      }
      any_crit <- any_crit | crit
      any_miss <- any_miss | (made & !a$hit)
    }
  }

  for (ev in riders) {
    sc    <- scope_key(ev)
    avail <- runif(n) < ev$p_resource
    dice  <- roll_dice(n, ev$n_dice, ev$die, ev$gwf) +
             ifelse(first_hit_crit[[sc]], roll_dice(n, ev$n_dice, ev$die, ev$gwf), 0)
    total <- total + ifelse(any_hit[[sc]] & avail, dice + ev$flat, 0)
  }

  for (ev in auto) {
    avail <- runif(n) < ev$p_resource
    for (j in seq_len(ev$attacks)) {
      dmg <- roll_dice(n, ev$n_dice, ev$die, ev$gwf) + ev$flat
      if (!is.null(ev$save_dc)) {
        saved <- (sample.int(20, n, replace = TRUE) + ev$save_bonus) >= ev$save_dc
        dmg   <- ifelse(saved, if (ev$half_on_save) floor(dmg / 2) else 0, dmg)
      }
      total <- total + ifelse(avail, dmg, 0)
    }
  }
  total
}

# 
# Defensive simulation ---------------------------------------------------------
simulate_damage_taken <- function(enemy_attacks, ac, roll_mode = "flat",
                                  resist = character(), immune = character(),
                                  vulnerable = character(),
                                  flat_reduction = 0, flat_reduction_types = PHYSICAL_TYPES,
                                  first_hit = NULL,
                                  n = 10000, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (inherits(enemy_attacks, "enemy_attack")) enemy_attacks <- list(enemy_attacks)
  roll_mode <- match.arg(roll_mode, ROLL_MODES)
  total <- numeric(n)
  reaction_used <- logical(n)
  largest_policy <- !is.null(first_hit) && identical(first_hit$policy, "largest")
  hits_mat <- NULL                                  # per-attack damage, for the "largest" policy

  for (ea in enemy_attacks) {
    mult <- if (ea$damage_type %in% immune) 0 else if (ea$damage_type %in% vulnerable) 2 else
            if (ea$damage_type %in% resist) 0.5 else 1
    r <- if (ea$damage_type %in% flat_reduction_types) flat_reduction else 0
    for (j in seq_len(ea$n_attacks)) {
      a    <- resolve_attack(n, ea$attack_bonus, ac, roll_mode)
      dice <- roll_dice(n, ea$n_dice, ea$die) + ifelse(a$crit, roll_dice(n, ea$n_dice, ea$die), 0)
      dmg  <- pmax(dice + ea$flat - r, 0)
      # resistance halves and rounds down (PHB: round down when dividing damage)
      dmg  <- if (mult == 0.5) floor(dmg / 2) else dmg * mult
      if (!is.null(first_hit) && !largest_policy) {
        use <- a$hit & !reaction_used
        dmg[use] <- if (first_hit$mode == "halve") floor(dmg[use] / 2)
                    else pmax(dmg[use] - first_hit$amount * mult, 0)
        reaction_used <- reaction_used | use
      }
      if (largest_policy) hits_mat <- cbind(hits_mat, ifelse(a$hit, dmg, 0))
      total <- total + ifelse(a$hit, dmg, 0)
    }
  }
  if (largest_policy && !is.null(hits_mat)) {       # hindsight policy: react to the biggest hit of the round
    big <- apply(hits_mat, 1, max)
    red <- if (first_hit$mode == "halve") big - floor(big / 2) else pmin(first_hit$amount, big)
    total <- total - red
  }
  total
}

# 
# Summaries --------------------------------------------------------------------
summarize_sim <- function(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), at_least = NULL) {
  out <- tibble(
    mean = mean(x), sd = sd(x), se_mean = sd(x) / sqrt(length(x)),
    p_zero = mean(x == 0)
  )
  q <- quantile(x, probs, names = FALSE)
  for (i in seq_along(probs)) out[[paste0("q", probs[i] * 100)]] <- q[i]
  if (!is.null(at_least)) out[[paste0("p_ge_", at_least)]] <- mean(x >= at_least)
  out
}

# Rounds until cumulative damage reaches `hp`, resampling rounds from `x`.
# Works for both offense (rounds to kill a target) and defense (rounds until the
# character drops). `max_rounds` caps runaway sequences (e.g. a damage vector of
# mostly zeros) and is reported as a censored value.
rounds_to_threshold <- function(x, hp, n = 10000, max_rounds = 50, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  m   <- matrix(sample(x, n * max_rounds, replace = TRUE), nrow = n)
  cum <- t(apply(m, 1, cumsum))
  reached <- cum >= hp
  rounds  <- ifelse(rowSums(reached) == 0, NA_integer_, apply(reached, 1, function(z) which(z)[1]))
  list(rounds = rounds,
       censored = mean(is.na(rounds)),
       summary = tibble(mean = mean(rounds, na.rm = TRUE),
                        median = median(rounds, na.rm = TRUE),
                        q25 = quantile(rounds, 0.25, na.rm = TRUE, names = FALSE),
                        q75 = quantile(rounds, 0.75, na.rm = TRUE, names = FALSE),
                        p_within_3 = mean(rounds <= 3, na.rm = TRUE)))
}
