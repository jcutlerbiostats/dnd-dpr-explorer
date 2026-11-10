---
tags:
  - dnd
  - dpr
  - r
  - shiny
  - plan
created: 2026-11-07
status: deploy-ready
---
# Shiny DPR app — plan

Goal: a Shiny app that computes **offensive DPR** and **expected damage taken per round** ("defensive DPR") for any D&D 5e (2014 rules, per [dnd5e.wikidot.com](https://dnd5e.wikidot.com/)) character whose round can be described as a fixed attack routine. Builds on [[Barbarian (Level 1) — Offensive & Defensive DPR]] and [[Barbarian (Path of the Zealot) — Offensive & Defensive DPR]].

---
## 1. Reframe the feasibility question

The hard part is not the *class*, it is the *size of the decision space per round*. A barbarian's round is one fixed routine (Attack ×2, Reckless, rage). A wizard's round is a choice among dozens of spells with different targets, saves, areas, and slot costs, and the "right" choice depends on the whole adventuring day.

So the scoping rule is: **the app models attack routines, not classes.** A class is in scope to the extent its typical round can be written as a fixed list of damage events. Under that rule:

| Tier | Classes / builds | Why |
|---|---|---|
| **1 — fully in scope** | Barbarian (all paths), Fighter (Champion, Battle Master with a maneuver-per-round slider, Samurai, Eldritch Knight weapon side), Rogue (Sneak Attack once/turn), Monk (Martial Arts + Flurry), Paladin (Improved Divine Smite; smites as a per-round resource slider), Ranger (Hunter's Mark, Horizon Walker, Gloom Stalker) | Weapon attacks, fixed riders, once-per-turn riders, crit modifiers. Same engine as the barbarian notes. |
| **2 — in scope with a fixed routine** | **Warlock** (Eldritch Blast + Agonizing Blast + Hex: this is the community's standard DPR benchmark and is *more* tractable than most martials), Cleric (weapon + Spiritual Weapon; Divine Strike), Bard (Swords/Valor), Druid (Moon: wild shape is a lookup table of beast attacks), Artificer (Battle Smith) | One or two spells define the round; model them as damage events with a "resource available this round" toggle. |
| **3 — cantrip-only mode** | Wizard, Sorcerer (and any caster) | Offer a single "cantrip spam" routine (Fire Bolt / Toll the Dead etc. scaled by level). Explicitly labelled as a floor, not a characterization of the class. |
| **Excluded** | Leveled-spell nova/AoE optimization, multi-target damage, summons, companion-based builds (Beast Master animal, Drakewarden) in v1 | Requires modeling spell selection, slot allocation across a day, and target geometry. Out of scope by design, not infeasible in principle. |

Recommendation: do **not** exclude warlock. Exclude *leveled spell selection* and keep every class's attack-roll / cantrip routine.

---
## 2. Core engine (class-agnostic)

One generic data structure handles every class. A round is a list of **damage events**:

```r
damage_event <- function(label, n_dice = 0, die = 0, flat = 0,
                         attacks = 1,           # how many times this event occurs
                         requires_hit = TRUE,   # FALSE for auto / save-based damage
                         once_per_turn = FALSE, # Sneak Attack, Divine Fury, Hex... ride the first hit
                         crit_dice_bonus = 0,   # Brutal Critical, Superior Critical, etc.
                         p_resource = 1)        # fraction of rounds the resource is available
```

Engine functions (already prototyped in `zealot_dpr.R` / `level1_barbarian_dpr.R`):

- `hit_probs(attack_bonus, target_ac, roll_mode = c("flat","advantage","disadvantage","elven_accuracy"), crit_min = 20)`
- `event_dpr(event, probs)` — per-attack events: $A[(p_{hit}-p_{crit})D_{hit} + p_{crit}D_{crit}]$; once-per-turn events: $[1-(1-p_{hit})^A]\,[\ldots]$ exactly as in the Zealot note
- `round_dpr(events, probs)` — sum
- `damage_taken(enemy, barb_ac, roll_mode, resistances)` — mirror image for the defensive side
- Feat/option toggles that alter inputs rather than adding code paths: Great Weapon Master / Sharpshooter (−5 / +10), Elven Accuracy, Crossbow Expert, Polearm Master bonus attack, fighting styles (Dueling +2, Archery +2 to hit, GWF reroll ≈ +0.83 per d12 / +1.33 per 2d6)

**Class definitions are data, not code.** Each class/subclass is a table of `(level, feature, effect_type, value)` rows; `build_events(class, subclass, level, inputs)` turns them into a list of damage events. Effect types needed so far: `extra_attack`, `rider_per_hit`, `rider_once_per_turn`, `crit_range`, `crit_extra_dice`, `roll_mode_self`, `roll_mode_enemy`, `resistance`, `ac_formula`, `bonus_attack`. Adding a class means adding rows, not functions.

---
## 3. Defensive side

- Barbarian notes define it as $m \cdot \text{Raw}$ with the enemy's hit probabilities against the character's AC. Keep that.
- **Enemy presets:** the DMG "Monster Statistics by Challenge Rating" table gives attack bonus and damage/round per CR. Load as a lookup so the user picks a CR instead of typing an attack bonus. Also allow a custom enemy (n attacks, bonus, dice, damage type).
- Character AC input: armor + shield + DEX cap, or Unarmored Defense (Barbarian: DEX+CON; Monk: DEX+WIS), or mage armor.
- Resistances as a vector keyed by damage type; Rage, Bear Totem, Heavy Armor Master (−3 flat, applied before halving), Uncanny Dodge (halve one hit, approximated as halving the largest expected single hit).

---
## 4. Shiny layout

Sidebar (inputs) · main area (tabs). Use `bslib` for layout and a sidebar that shows only the controls relevant to the selected class.

| Tab | Content |
|---|---|
| **Offense** | DPR total, plus a **term-by-term breakdown table** (each event, its probability, damage, contribution). This is the feature that distinguishes the app from a calculator: it shows the formula the way the Obsidian notes do. |
| **Defense** | Damage taken/round, breakdown by enemy attack, resistance multiplier shown explicitly |
| **Trade-off** | Line chart of offense and defense vs. target AC (AC 10–25), with toggles overlaid (Reckless on/off, GWM on/off) |
| **Progression** | DPR by character level 1–20 for the current build vs. a fixed target AC |
| **Formula** | Rendered LaTeX of the exact expression being computed, with current numbers substituted |

Plots are `ggplot2`, rendered in-app (no `ggsave`). Pipes are `%>%`.

---
## 5. Project structure

```
dun_and_dragons_DPR/
  app.R                      # ui + server, sources R/
  R/
    engine.R                 # hit_probs, event_dpr, round_dpr, damage_taken
    build_events.R           # class table -> damage events
    defense.R                # AC formulas, resistances, enemy presets
    plots.R
  data/
    classes.csv              # level, class, subclass, feature, effect_type, value
    enemies_by_cr.csv        # DMG table
    weapons.csv
  tests/testthat/
    test-engine.R            # must reproduce 7.8 / 6.5 / 3.2 (lvl 1) and 29.0 / 7.58 / 37.6 (Zealot)
  zealot_dpr.R, level1_barbarian_dpr.R   # keep as worked derivations
```

Packages: `shiny`, `bslib`, `dplyr`, `purrr`, `tidyr`, `ggplot2`, `readr`, `testthat`. Plain `R/` folder sourced from `app.R`; `golem` is overkill at this size.

---
## 6. Phases

1. ~~**Engine + tests.**~~ **Done 2026-11-07.** `R/engine.R` + `tests/testthat/test-engine.R` (30 tests reproduce every number in both barbarian notes). Run with `Rscript -e 'testthat::test_dir("tests/testthat")'`. Demo: `demo_engine.R`.
1b. ~~**Monte Carlo companion.**~~ **Done 2026-11-07.** `R/simulate.R`: `simulate_round()`, `simulate_damage_taken()`, `summarize_sim()`, `rounds_to_threshold()`. Same event lists as the closed forms; tests assert agreement within MC error. Mixed-bonus rider approximation measured at ~0.35% error. Decision: the app shows **distributions, not just means** (histograms, quantiles, P(0), rounds-to-kill / rounds-to-drop).
2. ~~**Barbarian complete.**~~ **Done 2026-11-07.** `data/classes.csv` (feature table, all nine paths), `data/weapons.csv`, `R/build_events.R` (`barbarian_build()`, `barbarian_options()`), `app.R` with Offense / Defense / Rounds-to… / Build tabs. 7 build tests incl. every subclass × option × level smoke test. Known v1 simplifications are surfaced in the app's Build tab (Retaliation not modelled; Infectious Fury save ignored; aura/surge save targets assumed +2; spiked armor = medium armor; Reckless Abandon temp HP not in damage-taken).
3. ~~**Martials.**~~ **Done 2026-11-07.** Fighter (10 subclasses), Rogue (9), Monk (10), Paladin (9), Ranger (8) in `R/build_events.R` via a shared `weapon_profile()` (ability choice, versatile, fighting styles, −5/+10 feat, off-hand) and `build_character()` dispatcher; `data/armor.csv` + `R/defense.R` for AC. Engine gained three policy mechanics, each exact in closed form and matched by the simulator: `per_hit` riders (smite every hit, Improved Divine Smite, Hunter's Mark, Elemental Cleaver, maneuvers) that ride the real attack rolls; `crit_only` (smite-on-crit policy); `trigger = prior_crit / prior_miss` (GWM bonus attack, Stalker's Flurry); and `first_hit` reduction on defense (Uncanny Dodge halve, Deflect Missiles reduce). 10 new hand-computed build tests + a smoke test over every class × subclass × 6 levels. Documented approximations: once-per-turn rider with mixed attack bonuses (~0.35%); triggered attacks condition on the Attack action only. Not modelled (surfaced as notes in-app): Berserker Retaliation, Soulknife Homing Strikes, Assassinate / Dread Ambusher (nova), Breath of the Dragon, War Magic, companions, multi-target features.
4. ~~**Fixed-routine casters.**~~ **Done 2026-11-08.** Warlock (9 patrons; Eldritch Blast / Pact weapon / other cantrip routines; Agonizing Blast, Hex, Hexblade's Curse, Thirsting Blade, Lifedrinker, Eldritch Smite slider, Genie's Wrath, Grave Touched, Tentacle of the Deeps, Radiant Soul), Cleric (14 domains; weapon or cantrip routine; Spiritual Weapon with slot level; Divine Strike / Potent Spellcasting / Blessed Strikes; War Priest, Touch of Death), Bard (8 colleges; Swords/Valor Extra Attack, Blade Flourish, Whispers Psychic Blades, Vicious Mockery), Druid (7 circles; Moon Wild Shape from `data/beasts.csv` with beast AC/HP used on defense; Spores Symbiotic Entity + Halo; Stars Archer; Wildfire Enhanced Bond; Shillelagh), Artificer (4; Battle Smith INT weapons + Arcane Jolt, Armorer gauntlets/launcher, Artillerist cannon + Arcane Firearm, Alchemist Savant), Wizard (Evocation Potent Cantrip / Empowered Evocation, Bladesinger weapon routine + Bladesong AC; others cantrip-only), Sorcerer (Quickened slider, Draconic affinity + AC; cantrip-only). `data/cantrips.csv` drives attack vs save cantrips with tier scaling; new `t_save` input = target save bonus. Engine gained `attack_kind` / `rides` so riders are scoped to weapon or spell attacks (Divine Strike ignores Spiritual Weapon; Hex rides both). 9 hand-computed caster tests; smoke test over all 13 classes. Not modelled: leveled-spell selection, AoE, companions (Steel Defender), Battle Magic, Bladesinger cantrip-in-place-of-attack.
5. ~~**Defense presets + Trade-off / Progression tabs + Formula tab.**~~ **Done 2026-11-08.** `data/enemies_by_cr.csv` (DMG Monster Statistics by CR, 1/8–24) + "Load" button fills target AC/HP, save bonus, and a 2-attack enemy whose mean matches the table's damage/round. `R/plots.R`: Trade-off tab (DPR vs target AC with Reckless / −5/+10 feat toggled variants; damage taken vs enemy attack bonus), Progression tab (levels 1–20, inputs otherwise fixed), Formula tab (MathJax, every breakdown row with numbers substituted). Per-class starting ability modifiers now load when the class changes (fixes the INT-0 wizard default).
6. ~~**Remaining modelling work**~~ **Done 2026-11-09** (A–E; 2024 rules dropped by decision).
   - **B. Fighting styles:** Dueling keeps versatile weapons one-handed (d8+2 > d10); Defense requires armor; off-hand weapon has its own selector (light unless Dual Wielder); Tasha's styles added (Unarmed Fighting d6/d8, Thrown Weapon Fighting +2, Superior Technique d6 with slider; Blind Fighting / Interception / Protection / Blessed Warrior / Druidic Warrior listed as no-DPR).
   - **E1. Exact math:** `first_hit_walk()` replaces both approximations; every once-per-turn rider is now exact under mixed bonuses and triggered attacks. All simulator cross-checks are within MC error.
   - **C. Buffs & items:** Bless (`to_hit_die` averages p_hit over the d4; exact), Haste extra attack, advantage-source toggle, extra magic-weapon dice. `apply_common()` applies these to every build.
   - **D. Formerly "not modelled":** Tasha's companions (primal companion, Steel Defender, drake) as `attack_kind = "companion"` events (own riders don't ride them; Arcane Jolt may); Retaliation / Giant Killer reaction attacks injected with P(enemy hits) (`with_reaction()`); War Magic, Battle Magic, Bladesinger weapon + cantrip routines; Booming Blade / Green-Flame Blade via `rides_label`; Homing Strikes exact miss-conversion (`p_hit_add`); multi-target credit slider (Horde Breaker, Volley/Whirlwind, Wails from the Grave, desert aura, Slashing Flourish, GFB second target).
   - **A. Feats:** −5/+10 (now in the feats list), Polearm Master (bonus-action priority over GWM), Crossbow Expert, Dual Wielder (+1 AC, non-light off-hand), Elven Accuracy, Savage Attacker and Piercer (`reroll_gain()` = E[max of two] − E), Lucky (miss reroll on the first attack via `p_hit_add`), Heavy Armor Master (needs heavy armor), Crusher (later attacks get advantage with P(prior crit); exact mean, independent split in the simulator).
   - **E2. Combat simulator** `R/combat.R` + Combat tab: whole combats with state — damage truncated at the kill, character HP vs the enemy, resource pools spent greedily (`pool` / `pool_cost` on events; `b$pools` per class: slots, surge, superiority, ki, bardic, sorcery, pact, arcane shot, psi, echo, war priest, channel, tentacle), concentration saves (Hex / Hunter's Mark / Favored Foe; War Caster), round-1 nova (Action Surge, Fighting Spirit, Assassinate auto-crit if surprised, Dread Ambusher), Colossus Slayer waits for a damaged target, Toll the Dead d8 in round 1. Outputs: P(win), kill-round and drop-round distributions, total damage per combat. Uncanny Dodge "largest hit" policy available in the Defense simulation.
   - **Still out:** GWM on-kill bonus attack (needs a second target), leveled-spell selection, AoE beyond the credit slider, Bladesinger Song of Defense, Relentless Rage, Deflect Missiles catch-and-throw.

## Deployment (2026-11-09)
- **Target:** Shinylive on GitHub Pages (free, no server, runs in the browser via webR). shinyapps.io rejected as slow.
- **Done:** all plots are native plotly (pre-aggregated data; `ggplotly` was far too slow in webR); mobile-first layout (collapsible sidebar, 2-up value boxes on phones, scrollable tab strip and tables, non-fillable page); `scripts/export_shinylive.R`; `.github/workflows/deploy.yml`; local git repo initialised with the first commit. Verified in Chrome against the exported bundle: start-up, class switching, Combat and Formula tabs.
- **Bugs found only in the browser:** (1) webR's ggplot2 needs `munsell` (hint in app.R); (2) `Plotly is not defined` because Shinylive injects widget scripts asynchronously → plotly.js now shipped in `www/` and loaded synchronously; (3) an **infinite re-render loop**: `reactiveValuesToList(input)` made `build()` depend on `plotly_afterplot` events → fixed with an explicit `BUILD_INPUTS` list; (4) `renderUI` removed entirely (static inputs + `conditionalPanel` + `update*`), safer under Shinylive's async UI rendering; (5) bslib's `header=` arg dropped in webR → CSS as body child.
- **Live (2026-11-10):** https://jcutlerbiostats.github.io/dnd-dpr-explorer/ — repo https://github.com/jcutlerbiostats/dnd-dpr-explorer (public). Pages source = GitHub Actions; every push to `main` redeploys. First load downloads ~30 MB of webR packages; later loads are cached. Test on a real phone.

## Related
- [[Barbarian (Level 1) — Offensive & Defensive DPR]]
- [[Barbarian (Path of the Zealot) — Offensive & Defensive DPR]]
