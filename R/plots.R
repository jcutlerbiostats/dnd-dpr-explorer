# 
# plots.R — Trade-off and Progression helpers --------------------------------------
# 
# These rebuild the character under varied inputs and evaluate the closed form,
# so they are cheap enough to recompute on every input change.

ENEMIES_BY_CR <- read_csv_base(file.path(DATA_DIR, "enemies_by_cr.csv"))

# DMG "Monster Statistics by Challenge Rating" row -> app inputs.
# The table's damage/round assumes every attack hits, so it is split into
# n_attacks attacks of (die + flat) whose mean matches the midpoint.
cr_preset <- function(cr, n_attacks = 2, die = 8) {
  r <- ENEMIES_BY_CR[ENEMIES_BY_CR$cr == cr, ]
  per_attack <- (r$dmg_lo + r$dmg_hi) / 2 / n_attacks
  list(target_ac = r$ac, target_hp = round((r$hp_lo + r$hp_hi) / 2), t_save = r$save_dc - 10,
       e_n = n_attacks, e_bonus = r$attack_bonus, e_ndice = 1, e_die = die,
       e_flat = max(0, round(per_attack - (die + 1) / 2)))
}

build_dpr <- function(inp, target_ac) {
  b <- build_character(modifyList(inp, list(target_ac = target_ac)))
  round_dpr(b$events, b$attack_bonus, target_ac, b$roll_mode, b$crit_min, b$to_hit_die %||% 0)$total
}

# DPR vs target AC for the current build and a few one-toggle variants
tradeoff_curves <- function(inp, acs = 8:25) {
  variants <- list("Current build" = inp)
  cl <- g(inp, "class", "Barbarian")
  if (cl == "Barbarian") variants[[if (isTRUE(inp$reckless)) "Reckless off" else "Reckless on"]] <- modifyList(inp, list(reckless = !isTRUE(inp$reckless)))
  if (cl %in% c("Barbarian", "Fighter", "Paladin", "Ranger", "Rogue", "Warlock", "Bard", "Artificer", "Wizard"))
    variants[[if (isTRUE(inp$feat510)) "−5/+10 feat off" else "−5/+10 feat on"]] <- modifyList(inp, list(feat510 = !isTRUE(inp$feat510)))
  map_dfr(names(variants), function(nm) {
    v <- variants[[nm]]
    tibble(variant = nm, target_ac = acs, dpr = map_dbl(acs, ~ tryCatch(build_dpr(v, .x), error = function(e) NA_real_)))
  })
}

# damage taken per round vs enemy attack bonus, for the current AC
defense_curve <- function(build, enemy, enemy_mode, bonuses = 2:14) {
  tibble(enemy_bonus = bonuses,
         taken = map_dbl(bonuses, function(bn) { e <- enemy; e$attack_bonus <- bn
           damage_taken(e, build$ac, enemy_mode, resist = build$resist, first_hit = build$first_hit, flat_reduction = build$flat_reduction %||% 0)$total }))
}

# DPR and damage taken by character level, holding every other input fixed
progression <- function(inp, target_ac, enemy, enemy_adv = FALSE) {
  map_dfr(1:20, function(L) {
    v <- modifyList(inp, list(level = L))
    b <- tryCatch(build_character(v), error = function(e) NULL)
    if (is.null(b)) return(tibble(level = L, dpr = NA_real_, taken = NA_real_, ac = NA_real_))
    em <- if (b$enemy_roll_mode == "advantage" || enemy_adv) "advantage" else "flat"
    tibble(level = L,
           dpr   = round_dpr(b$events, b$attack_bonus, target_ac, b$roll_mode, b$crit_min, b$to_hit_die %||% 0)$total,
           taken = damage_taken(enemy, b$ac, em, resist = b$resist, first_hit = b$first_hit, flat_reduction = b$flat_reduction %||% 0)$total,
           ac    = b$ac)
  })
}

# 
# Formula rendering ------------------------------------------------------------------
# 
fnum <- function(x, d = 2) formatC(x, format = "f", digits = d)

formula_offense <- function(breakdown, total) {
  rows <- map_chr(seq_len(nrow(breakdown)), function(i) {
    r <- breakdown[i, ]
    body <- switch(sub(" .*", "", r$type),
      "attack" = sprintf("%s \\times %s \\times [(%s - %s)\\,%s + %s \\times %s]",
                         fnum(r$attacks, 3), fnum(r$p_resource), fnum(r$p_hit, 3), fnum(r$p_crit, 3), fnum(r$d_hit), fnum(r$p_crit, 3), fnum(r$d_crit)),
      "per-hit" = sprintf("%s \\times \\sum_{\\text{attacks}} [(p_h - p_c)\\,%s + p_c \\times %s]\\;(\\bar{p}_h = %s)",
                          fnum(r$p_resource), fnum(r$d_hit), fnum(r$d_crit), fnum(r$p_hit, 3)),
      "once-per-turn" = sprintf("%s \\times P(\\text{any hit}) = %s \\times [(1-q)\\,%s + q \\times %s],\\; q = %s",
                                fnum(r$p_resource), fnum(r$p_hit, 3), fnum(r$d_hit), fnum(r$d_crit), fnum(r$p_crit / max(r$p_hit, 1e-9), 3)),
      "automatic" = sprintf("%s \\times %s \\times %s", fnum(r$attacks, 3), fnum(r$p_resource), fnum(r$d_hit)),
      "save" = sprintf("%s \\times %s \\times P(\\text{applies}) = %s \\times %s", fnum(r$attacks, 3), fnum(r$p_resource), fnum(r$p_hit, 3), fnum(r$d_hit)),
      "")
    sprintf("\\text{%s:}\\quad %s = \\mathbf{%s}", gsub("([&%$#_{}])", "\\\\\\1", r$label), body, fnum(r$expected))
  })
  paste0("$$", c(rows, sprintf("\\text{DPR} = \\mathbf{%s}", fnum(total))), "$$", collapse = "\n")
}

formula_defense <- function(breakdown, total) {
  rows <- map_chr(seq_len(nrow(breakdown)), function(i) {
    r <- breakdown[i, ]
    if (is.na(r$attacks)) return(sprintf("\\text{%s:}\\quad \\mathbf{%s}", gsub("([&%$#_{}])", "\\\\\\1", r$label), fnum(r$taken)))
    sprintf("\\text{%s:}\\quad m \\times n\\,[(p_h - p_c)\\,D_h + p_c\\,D_c] = %s \\times %s\\,[(%s - %s)\\,%s + %s \\times %s] = \\mathbf{%s}",
            gsub("([&%$#_{}])", "\\\\\\1", r$label), fnum(r$multiplier, 2), fnum(r$attacks, 0), fnum(r$p_hit, 3), fnum(r$p_crit, 3),
            fnum(r$d_hit), fnum(r$p_crit, 3), fnum(r$d_crit), fnum(r$taken))
  })
  paste0("$$", c(rows, sprintf("\\text{Damage taken per round} = \\mathbf{%s}", fnum(total))), "$$", collapse = "\n")
}
