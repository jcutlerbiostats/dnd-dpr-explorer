---
tags:
  - dnd
  - dpr
  - r
  - shiny
  - project
created: 2026-11-09
status: phases 1–6 complete
---
# DPR app — project overview

A Shiny app that computes **offensive DPR** (expected damage dealt per round) and **expected damage taken per round** for D&D 5e (2014 rules) characters, for all 13 classes and every published subclass, with an exact closed form, a Monte Carlo simulator that cross-checks it, and a stateful whole-combat simulator. Built 2026-11-07 to 2026-11-09 from the two worked barbarian notes.

Run from the project folder: `shiny::runApp()`. Tests: `testthat::test_dir("tests/testthat")`.

---
## Summary of what is there

| Piece | What it does |
|---|---|
| `R/engine.R` (367 lines) | Class-agnostic closed form. A round is a list of *damage events*; `round_dpr()` sums them exactly. `damage_taken()` is the mirror image for defense. |
| `R/simulate.R` (211) | Monte Carlo twin of the engine on the same event lists: `simulate_round()`, `simulate_damage_taken()`, summaries, rounds-to-threshold. |
| `R/combat.R` (164) | `simulate_combat()`: whole fights with state (HP, resource pools, concentration, round-1 nova). |
| `R/build_events.R` (1067) | Turns a character description into events: one builder per class, a shared `weapon_profile()`, feats, buffs, companions. |
| `R/defense.R`, `R/plots.R` | AC from the armor table; Trade-off / Progression curves; MathJax formula rendering. |
| `app.R` (434) | bslib UI with 9 tabs. |
| `data/` | `classes.csv` (145 feature rows), `weapons.csv` (24), `armor.csv` (14), `cantrips.csv` (15), `beasts.csv` (16 Moon Druid forms), `enemies_by_cr.csv` (27 DMG rows). |
| `tests/testthat/` | 76 test blocks, 1,735 expectations, all passing. Hand-computed values for every class plus simulator-vs-formula agreement within Monte Carlo error. |

**Scope rule.** The app models *attack routines*, not classes. A class is in scope to the extent its typical round is a fixed list of damage events. Leveled-spell selection, area effects (beyond a credit slider), and nova optimisation across an adventuring day are deliberately out.

**Tabs.** Offense · Defense · Rounds to… · Combat · Trade-off · Progression · Formula · Build. Every figure shows the exact value next to the simulated distribution (mean, SD, quantiles, P(zero), histogram).

**Sources.** [dnd5e.wikidot.com](https://dnd5e.wikidot.com/) for class text; aidedd.org for beast stat blocks; the DMG "Monster Statistics by Challenge Rating" table for enemy presets.

---
## The two formulas

Per attack, with hit probability $p_h$, crit probability $p_c$, damage on hit $D_h$ and on crit $D_c$:

$$\text{DPR}_{\text{attack}} = A \cdot p_{\text{res}} \left[(p_h - p_c)\,D_h + p_c\,D_c\right]$$

Once-per-turn riders (Sneak Attack, Divine Fury, Colossus Slayer…) ride the first hit among the attacks they may ride; $P(\text{first hit})$ and $q = P(\text{first hit is a crit})$ come from an exact ordered walk over the attacks. Per-hit riders (Hex, Hunter's Mark, smite on every hit) are summed over every attack they ride. Save-based and automatic damage add $P(\text{applies}) \cdot D$.

Defense: $\text{taken} = \sum_i m_i \cdot n_i\,[(p_h - p_c)\,E_h + p_c\,E_c]$ with the enemy's hit probabilities against your AC, $m$ the resistance multiplier, minus an exact first-hit reduction for Uncanny Dodge / Deflect Missiles.

With advantage $p \to 1-(1-p)^2$; Elven Accuracy $1-(1-p)^3$; Bless averages $p$ over the d4.

---
## Phase by phase

### Phase 1 — Engine and tests (2026-11-07)
- `hit_probs()`, `damage_event()`, `round_dpr()`, `damage_taken()`.
- Tests lock in the numbers from [[Barbarian (Level 1) — Offensive & Defensive DPR]] (7.8 / 6.5 / 3.2) and [[Barbarian (Path of the Zealot) — Offensive & Defensive DPR]] (29.0 / 7.58 / 37.6).

### Phase 1b — Monte Carlo companion
- Same event lists as the closed form; tests assert agreement within 4 standard errors.
- Decision: the app shows **distributions, not just means** — a single-attack Fire Bolt round is a spike at zero plus a hump near 11, and the mean sits in the gap between them.
- `rounds_to_threshold()` resamples rounds to give rounds-to-kill and rounds-until-you-drop.

### Phase 2 — Barbarian, all nine paths
- `data/classes.csv` as the feature table; `barbarian_build()`; first Shiny UI (Offense, Defense, Rounds to…, Build).
- Simplifications surfaced as Build-tab notes rather than hidden.

### Phase 3 — Fighter, Rogue, Monk, Paladin, Ranger (46 subclasses)
- Shared `weapon_profile()`: ability choice, versatile, fighting styles, −5/+10 feat, off-hand.
- Engine gained policy mechanics, each exact and matched by the simulator: `per_hit` riders that ride real attack rolls; `crit_only` (smite on crits); `trigger = prior_crit / prior_miss` (GWM bonus attack, Stalker's Flurry); `first_hit` reduction on defense.
- Two bugs caught by the simulator: a per-hit rider rolling its own attacks; Action Surge attacks counted at full weight in trigger probabilities.

### Phase 4 — Casters as fixed routines (2026-11-08)
- Warlock (Eldritch Blast / pact weapon / cantrip), Cleric (weapon or cantrip + Spiritual Weapon, Divine Strike / Potent Spellcasting / Blessed Strikes), Bard, Druid (Moon Wild Shape from `beasts.csv`, beast AC and HP used on defense), Artificer, Wizard and Sorcerer in cantrip-only mode labelled as a floor.
- `data/cantrips.csv` drives attack vs save cantrips with tier scaling; target save bonus input.
- Engine: `attack_kind` / `rides` so riders are scoped (Divine Strike ignores Spiritual Weapon; Hex rides both).

### Phase 5 — Presets and analysis tabs
- DMG enemy presets by CR with a Load button; Trade-off tab (DPR vs target AC with Reckless / feat variants; damage taken vs enemy attack bonus); Progression tab (levels 1–20); Formula tab (MathJax with numbers substituted); per-class starting ability modifiers.

### Phase 6 — Modelling completeness (2026-11-09)
- **Fighting-style fixes:** Dueling keeps versatile weapons one-handed; Defense requires armor; off-hand selector; Tasha's styles.
- **Exact math:** `first_hit_walk()` removed the last two approximations.
- **Buffs & items:** Bless, Haste, advantage source, extra magic-weapon dice (`apply_common()`).
- **Previously unmodelled, now in:** Tasha's companions (primal companion, Steel Defender, drake) as `attack_kind = "companion"`; Retaliation / Giant Killer reaction attacks (`with_reaction()`); War Magic, Battle Magic, Bladesinger weapon + cantrip; Booming Blade / Green-Flame Blade via `rides_label`; Homing Strikes exact conversion (`p_hit_add`); multi-target credit slider.
- **Feats:** −5/+10, Polearm Master, Crossbow Expert, Dual Wielder, Elven Accuracy, Savage Attacker, Piercer (`reroll_gain()`), Lucky, Heavy Armor Master, Crusher.
- **Combat simulator** (`R/combat.R`, Combat tab): damage truncated at the kill; your HP vs the enemy; resource pools spent greedily (slots, surge, superiority, ki, bardic, sorcery, pact, arcane shot, psi, echo, war priest, channel, tentacle); concentration saves with War Caster; round-1 nova (Action Surge, Fighting Spirit, Assassinate auto-crit, Dread Ambusher); Colossus Slayer waits for a damaged target; Toll the Dead starts on a d8. Outputs P(win), kill-round and drop-round distributions, total damage per combat.
- Uncanny Dodge "largest hit" policy for the Defense simulation.

---
## Modelling conventions worth remembering
- **"Fraction of rounds" sliders** are the resource-smoothing assumption for sustained play; the Combat tab replaces them with actual pools.
- Resistance is halved exactly in the closed form and rounded down in the simulation (PHB rule), so simulated damage taken sits a few tenths below the exact value.
- Companions' hits are not ridden by your own riders (Hunter's Mark), except Arcane Jolt which may ride the Steel Defender.
- Crusher's advantage split is exact in mean but independent in the simulation.
- Still out of scope: GWM's on-kill bonus attack, leveled spells, AoE beyond the credit slider, Song of Defense, Relentless Rage, 2024 rules (decided against).

## Related
- [[Shiny DPR app — plan]] — the running plan with per-phase detail and decisions
- [[Barbarian (Level 1) — Offensive & Defensive DPR]] · [[Barbarian (Path of the Zealot) — Offensive & Defensive DPR]]
