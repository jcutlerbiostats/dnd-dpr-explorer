# 
# app.R — D&D 5e (2014) DPR explorer. ------------------------------------------
# Phase 4: all 13 classes (casters via fixed routines / cantrip mode).
# Run:  shiny::runApp()   from the project root
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(dplyr); library(purrr)
})
# Charts: plotly.js is shipped in www/ and driven directly through a custom message
# (Plotly.react). The plotly R package is deliberately NOT used: it pulls ggplot2, htmlwidgets,
# knitr, rmarkdown, stringi, data.table, httr and openssl into the WebAssembly bundle (~35 MB),
# which is what blew past the iOS WebKit memory limit and gave a blank page on phones.
source("R/engine.R"); source("R/simulate.R"); source("R/build_events.R"); source("R/plots.R"); source("R/combat.R")

DEFAULT_ARMOR <- c(Barbarian = "None", Fighter = "Chain mail", Rogue = "Studded leather",
                   Monk = "None", Paladin = "Chain mail", Ranger = "Scale mail",
                   Warlock = "Leather", Cleric = "Scale mail", Bard = "Studded leather", Druid = "Hide",
                   Artificer = "Scale mail", Wizard = "Mage Armor", Sorcerer = "Mage Armor")
DEFAULT_WEAPON <- c(Barbarian = "Greataxe", Fighter = "Longsword", Rogue = "Rapier",
                    Monk = "Shortsword", Paladin = "Longsword", Ranger = "Longbow",
                    Warlock = "Longsword", Cleric = "Mace", Bard = "Rapier", Druid = "Quarterstaff",
                    Artificer = "Longsword", Wizard = "Rapier", Sorcerer = "Dagger")
CASTERS <- c("Warlock", "Cleric", "Bard", "Druid", "Artificer", "Wizard", "Sorcerer")
# sensible starting ability modifiers per class (a 16 primary, 14/12 secondaries); the user can edit them
ABILITY_DEFAULTS <- list(
  Barbarian = c(str = 3, dex = 2, con = 3, int = 0, wis = 1, cha = 0),
  Fighter   = c(str = 3, dex = 2, con = 2, int = 0, wis = 1, cha = 0),
  Rogue     = c(str = 0, dex = 3, con = 2, int = 1, wis = 1, cha = 1),
  Monk      = c(str = 0, dex = 3, con = 2, int = 0, wis = 3, cha = 0),
  Paladin   = c(str = 3, dex = 0, con = 2, int = 0, wis = 0, cha = 3),
  Ranger    = c(str = 0, dex = 3, con = 2, int = 0, wis = 2, cha = 0),
  Warlock   = c(str = 0, dex = 2, con = 2, int = 0, wis = 1, cha = 3),
  Cleric    = c(str = 2, dex = 0, con = 2, int = 0, wis = 3, cha = 0),
  Bard      = c(str = 0, dex = 2, con = 2, int = 0, wis = 0, cha = 3),
  Druid     = c(str = 0, dex = 2, con = 2, int = 0, wis = 3, cha = 0),
  Artificer = c(str = 0, dex = 2, con = 2, int = 3, wis = 1, cha = 0),
  Wizard    = c(str = 0, dex = 2, con = 2, int = 3, wis = 1, cha = 0),
  Sorcerer  = c(str = 0, dex = 2, con = 2, int = 0, wis = 0, cha = 3))

# mobile-first: the sidebar collapses to a toggle on phones, value boxes wrap,
# paired cards stack below the lg breakpoint, tables and the tab strip scroll sideways
MOBILE_CSS <- "
  /* Flatly-like palette without a Sass compile */
  .bslib-page-title.navbar, .navbar { background: #2c3e50 !important; color: #fff; }
  .navbar .navbar-brand, .bslib-page-title { color: #fff !important; }
  .bg-primary { background-color: #2c3e50 !important; }
  .bg-danger { background-color: #e74c3c !important; }
  .nav-tabs .nav-link { color: #18bc9c; }
  .nav-tabs .nav-link.active { color: #2c3e50; }
  .btn-outline-primary { color: #2c3e50; border-color: #2c3e50; }
  .btn-outline-primary:hover { background: #2c3e50; color: #fff; }
  .accordion-button:not(.collapsed) { background: #ecf0f1; color: #2c3e50; }
  .irs--shiny .irs-bar, .irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to { background: #2c3e50; border-color: #2c3e50; }
  /* tab strip: one scrollable row, never wraps */
  .bslib-card .card-header .nav, .nav-tabs, .nav-underline, .card-header-tabs {
    flex-wrap: nowrap !important; overflow-x: auto; overflow-y: hidden; white-space: nowrap; scrollbar-width: thin; }
  .nav-tabs .nav-item, .card-header-tabs .nav-item { flex: 0 0 auto; }
  .nav-tabs .nav-link { padding: .5rem .7rem; }
  /* value boxes: compact, two-up on phones, four-up from 992px */
  .vb-row { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: .5rem; margin-bottom: .75rem; }
  @media (min-width: 992px) { .vb-row { grid-template-columns: repeat(4, minmax(0, 1fr)); } }
  .vb-row .bslib-value-box { margin-bottom: 0; height: 100%; }
  .bslib-value-box { min-height: 0 !important; }
  .bslib-value-box .value-box-area { padding: .6rem .9rem !important; }
  .bslib-value-box .value-box-title { font-size: .78rem; margin-bottom: .15rem; }
  .bslib-value-box .value-box-value { font-size: 1.25rem; line-height: 1.2; }
  .table-responsive { -webkit-overflow-scrolling: touch; }
  .table { font-size: .85rem; margin-bottom: 0; }
  .form-group, .shiny-input-container { margin-bottom: .6rem; }
  .accordion-button { padding: .6rem .9rem; }
  @media (max-width: 575.98px) {
    .card-header { font-size: .95rem; }
    h1, .navbar-brand { font-size: 1.1rem; }
    .bslib-value-box .value-box-value { font-size: 1.1rem; }
    .js-plotly-plot .plotly .modebar { display: none; }
  }
"
vb_row <- function(...) div(class = "vb-row", ...)   # own CSS grid: 2-up on phones, 4-up from lg
pair   <- function(...) layout_columns(col_widths = breakpoints(sm = 12, lg = 6), fill = FALSE, ...)
tbl    <- function(id) div(class = "table-responsive", tableOutput(id))
# fixed-height, non-fill plot box: inside bslib fill containers a plot div would otherwise
# shrink to the row height (the Combat tab's charts collapsed to ~40px)
plt    <- function(id, height = "300px") div(id = id, class = "dpr-plot", style = paste0("height:", height, "; width:100%;"))
PLOT_JS <- "
(function () {
  var pending = {};
  function visible(el) { return !!(el && el.offsetParent !== null && el.offsetWidth > 0); }
  function draw(id, spec) {
    var el = document.getElementById(id); if (!el) return;
    if (!visible(el)) { pending[id] = spec; return; }
    delete pending[id];
    Plotly.react(el, spec.data, spec.layout, spec.config);
  }
  Shiny.addCustomMessageHandler('dpr-plot', function (m) { draw(m.id, m); });
  function flush() { Object.keys(pending).forEach(function (id) { draw(id, pending[id]); });
    document.querySelectorAll('.dpr-plot').forEach(function (el) { if (visible(el) && el.data) Plotly.Plots.resize(el); }); }
  document.addEventListener('shown.bs.tab', function () { setTimeout(flush, 50); });
  document.addEventListener('shown.bs.collapse', function () { setTimeout(flush, 50); });
  window.addEventListener('resize', function () { setTimeout(flush, 100); });
})();
"

ui <- page_sidebar(
  title = "D&D 5e DPR explorer",
  fillable = FALSE,      # natural heights; the page scrolls (better on phones than viewport-filling cards)
  # Plain bs_theme(): bslib ships it precompiled. Any customisation (bootswatch, colours) makes
  # bslib compile Sass with libsass inside WebAssembly at start-up, which trapped the R process on
  # iOS Safari (blank page). The Flatly-like look is reproduced with the CSS overrides below.
  theme = bs_theme(version = 5),
  sidebar = sidebar(width = 340, open = list(desktop = "open", mobile = "closed"),
    accordion(open = c("Character", "Class features"),
      accordion_panel("Character",
        selectInput("class", "Class", names(CLASS_SUBCLASSES), "Barbarian", selectize = FALSE),
        selectInput("subclass", "Subclass", CLASS_SUBCLASSES$Barbarian, selectize = FALSE),
        conditionalPanel("input.class == 'Barbarian' && ['Totem Warrior','Storm Herald','Wild Magic','Beast','Giant'].indexOf(input.subclass) > -1",
          selectInput("option", "Subclass option", c("Bear"), selectize = FALSE)),
        sliderInput("level", "Level", 1, 20, 5, step = 1),
        layout_columns(col_widths = c(4, 4, 4),
          numericInput("str", "STR", 3, -1, 7), numericInput("dex", "DEX", 2, -1, 7),
          numericInput("con", "CON", 3, -1, 7)),
        layout_columns(col_widths = c(4, 4, 4),
          numericInput("int", "INT", 0, -1, 7), numericInput("wis", "WIS", 1, -1, 7),
          numericInput("cha", "CHA", 0, -1, 7)),
        helpText("Ability modifiers, not scores."),
        selectInput("weapon", "Weapon", WEAPONS$name, "Greataxe", selectize = FALSE),
        checkboxInput("shield", "Shield", FALSE),
        checkboxInput("offhand", "Off-hand attack", FALSE),
        conditionalPanel("input.offhand", selectInput("offhand_weapon", "Off-hand weapon", WEAPONS$name[!WEAPONS$two_handed & !WEAPONS$ranged & !WEAPONS$heavy], "Shortsword", selectize = FALSE)),
        conditionalPanel("input.class != 'Monk' && !(input.class == 'Sorcerer' && input.subclass == 'Draconic')",
          selectInput("armor", "Armor (None = Unarmored Defense for Barbarian)", ARMOR$name, "None", selectize = FALSE)),
        conditionalPanel("input.class == 'Monk'", helpText("Monk: Unarmored Defense (10 + DEX + WIS), no armor or shield.")),
        conditionalPanel("input.class == 'Sorcerer' && input.subclass == 'Draconic'", helpText("Draconic Resilience: AC 13 + DEX.")),
        layout_columns(col_widths = c(4, 4, 4), numericInput("magic", "Magic +X", 0, 0, 3), numericInput("magic_ndice", "Extra dice", 0, 0, 4), numericInput("magic_die", "die", 6, 4, 12)),
        checkboxGroupInput("feats", "Feats", FEATS),
        conditionalPanel("input.feats && input.feats.indexOf('Lucky') > -1", sliderInput("lucky_p", "Lucky: fraction of rounds a point rerolls a missed first attack", 0, 1, 0.3, 0.05)),
        numericInput("hp", "Character HP (for rounds-to-drop)", 55, 1, 400)
      ),
      # Class features: every control is static and shown with conditionalPanel. Shinylive's
      # async renderUI can stall the client under the PostMessage channel, so no renderUI here.
      accordion_panel("Class features",
        conditionalPanel("['Fighter','Paladin','Ranger','Bard'].indexOf(input.class) > -1 && !(input.class == 'Bard' && input.subclass != 'Swords')",
          selectInput("fighting_style", "Fighting Style", FIGHTING_STYLES$Fighter, "None", selectize = FALSE)),
        conditionalPanel("input.fighting_style == 'Superior Technique' && input.class == 'Fighter'",
          sliderInput("superior_p", "Superior Technique: fraction of hits spending the d6", 0, 1, 0.1, 0.05)),
        conditionalPanel("['Warlock','Cleric','Bard','Druid','Wizard','Sorcerer'].indexOf(input.class) > -1 || (input.class == 'Artificer' && (['Artillerist','Alchemist'].indexOf(input.subclass) > -1 || input.level < 3)) || (input.class == 'Fighter' && input.subclass == 'Eldritch Knight' && input.level >= 7)",
          radioButtons("routine", "Round routine", c("Attack action"), "Attack action")),
        conditionalPanel("(input.class == 'Fighter' && input.subclass == 'Eldritch Knight' && input.level >= 7) || (input.class == 'Wizard' && input.subclass == 'Bladesinging')",
          selectInput("cantrip", "Weapon cantrip / cantrip", c("Booming Blade", "Green-Flame Blade", "Fire Bolt"), selectize = FALSE)),
        # Barbarian
        conditionalPanel("input.class == 'Barbarian'",
          checkboxInput("raging", "Raging", TRUE),
          checkboxInput("reckless", "Reckless Attack (lvl 2+)", TRUE),
          checkboxInput("bonus_attack", "Use subclass bonus action each turn", TRUE),
          conditionalPanel("input.subclass == 'Beast' && input.level >= 10", sliderInput("inf_fury_p", "Infectious Fury: fraction of rounds available", 0, 1, 0.5, 0.1))),
        # Fighter
        conditionalPanel("input.class == 'Fighter'",
          sliderInput("surge_p", "Action Surge: fraction of rounds", 0, 1, 0.2, 0.05),
          conditionalPanel("input.subclass == 'Battle Master'", sliderInput("maneuver_p", "Fraction of hits spending a superiority die", 0, 1, 0.3, 0.05)),
          conditionalPanel("input.subclass == 'Samurai'", checkboxInput("samurai_fs", "Fighting Spirit active this turn (advantage)", FALSE)),
          conditionalPanel("input.subclass == 'Rune Knight'", checkboxInput("rune_might", "Giant's Might active", TRUE)),
          conditionalPanel("input.subclass == 'Echo Knight'", sliderInput("echo_p", "Unleash Incarnation: fraction of rounds", 0, 1, 0.5, 0.05)),
          conditionalPanel("input.subclass == 'Cavalier'", sliderInput("cavalier_p", "Unwavering Mark attack: fraction of rounds", 0, 1, 0.3, 0.05)),
          conditionalPanel("input.subclass == 'Arcane Archer'", sliderInput("arcane_p", "Arcane Shot: fraction of rounds", 0, 1, 0.4, 0.05)),
          conditionalPanel("input.subclass == 'Psi Warrior'", sliderInput("psi_p", "Psionic Strike: fraction of rounds", 0, 1, 0.5, 0.05))),
        # Rogue
        conditionalPanel("input.class == 'Rogue'",
          sliderInput("sneak_p", "Sneak Attack available: fraction of rounds", 0, 1, 0.9, 0.05),
          conditionalPanel("input.subclass == 'Soulknife' && input.level >= 9", sliderInput("homing_p", "Homing Strikes: fraction of misses a psionic die is spent on", 0, 1, 0.5, 0.05)),
          conditionalPanel("input.subclass == 'Phantom'", sliderInput("wails_p", "Wails from the Grave: fraction of rounds (needs multi-target credit)", 0, 1, 0.5, 0.05))),
        conditionalPanel("(input.class == 'Rogue' && input.subclass != 'Soulknife') || (input.class == 'Warlock' && input.routine == 'Pact weapon')",
          selectInput("weapon_cantrip", "Weapon cantrip instead of the Attack action", c("None", "Booming Blade", "Green-Flame Blade"), selectize = FALSE)),
        conditionalPanel("(input.class == 'Rogue' && input.level >= 5) || (input.class == 'Ranger' && input.subclass == 'Hunter' && input.level >= 15)",
          checkboxInput("uncanny", "Uncanny Dodge (halve first hit each round)", TRUE)),
        # Monk
        conditionalPanel("input.class == 'Monk'",
          sliderInput("flurry_p", "Flurry of Blows: fraction of rounds (1 ki)", 0, 1, 0.5, 0.05),
          conditionalPanel("input.subclass == 'Kensei'", checkboxInput("kensei_parry", "Agile Parry (+2 AC)", TRUE)),
          conditionalPanel("(input.subclass == 'Kensei' && input.level >= 6) || input.subclass == 'Mercy'", sliderInput("ki_rider_p", "Deft Strike / Hand of Harm: fraction of rounds (1 ki)", 0, 1, 0.5, 0.05)),
          conditionalPanel("input.subclass == 'Kensei' && input.level >= 11", sliderInput("sharpen", "Sharpen the Blade: ki spent (+hit/+dmg)", 0, 3, 0, 1)),
          conditionalPanel("input.subclass == 'Astral Self'", checkboxInput("astral_on", "Astral arms active", TRUE))),
        # Paladin
        conditionalPanel("input.class == 'Paladin' && input.level >= 2",
          radioButtons("smite_policy", "Divine Smite policy", c("Never" = "none", "On every hit (fraction below)" = "every", "On crits only" = "crits"), "crits"),
          sliderInput("smite_slot", "Smite slot level", 1, 5, 1, 1),
          sliderInput("smite_p", "Fraction of hits smited (policy = every hit)", 0, 1, 0.5, 0.05),
          checkboxInput("undead", "Target is undead or fiend (+1d8)", FALSE)),
        conditionalPanel("input.class == 'Paladin' && input.subclass == 'Devotion'", checkboxInput("sacred_weapon", "Sacred Weapon active (+CHA to hit)", FALSE)),
        conditionalPanel("input.class == 'Paladin' && input.subclass == 'Vengeance'", checkboxInput("vow", "Vow of Enmity active (advantage)", FALSE)),
        # Ranger
        conditionalPanel("input.class == 'Ranger'",
          radioButtons("mark", "Mark", c("Hunter's Mark (1d6 per hit)" = "hunters_mark", "Favored Foe (1d4/1d6/1d8 once per turn)" = "favored_foe", "None" = "none"), "hunters_mark"),
          conditionalPanel("input.subclass == 'Hunter'",
            radioButtons("hunter3", "Hunter's Prey (3rd)", c("Colossus Slayer", "Giant Killer", "Horde Breaker")),
            sliderInput("colossus_p", "Colossus Slayer: fraction of rounds target is below max HP", 0, 1, 0.9, 0.05)),
          conditionalPanel("input.subclass == 'Beast Master'", selectInput("companion", "Primal companion (Tasha's)", c("Beast of the Land", "Beast of the Sea", "Beast of the Sky"), selectize = FALSE))),
        conditionalPanel("(input.class == 'Ranger' && ['Beast Master','Drakewarden'].indexOf(input.subclass) > -1) || (input.class == 'Artificer' && input.subclass == 'Battle Smith')",
          checkboxInput("command", "Command the companion each turn (bonus action)", TRUE)),
        # Warlock
        conditionalPanel("input.class == 'Warlock'",
          checkboxInput("agonizing", "Agonizing Blast (lvl 2+)", TRUE),
          checkboxInput("hex", "Hex up (1d6 per hit)", TRUE),
          conditionalPanel("input.routine == 'Pact weapon'",
            checkboxInput("thirsting", "Thirsting Blade (lvl 5+)", TRUE),
            sliderInput("esmite_p", "Eldritch Smite: fraction of rounds (lvl 5+)", 0, 1, 0, 0.05)),
          conditionalPanel("input.subclass == 'Hexblade'", checkboxInput("curse", "Hexblade's Curse on target (+PB, crit 19)", TRUE)),
          conditionalPanel("input.subclass == 'Undead'", checkboxInput("dread", "Form of Dread active (lvl 6+: +1 die once/turn)", TRUE)),
          conditionalPanel("input.subclass == 'Fathomless'", sliderInput("tentacle_p", "Tentacle of the Deeps: fraction of rounds", 0, 1, 0.5, 0.05))),
        # Cleric
        conditionalPanel("input.class == 'Cleric'",
          checkboxInput("spiritual", "Spiritual Weapon up (lvl 3+, bonus action)", TRUE),
          sliderInput("sw_slot", "Spiritual Weapon slot level", 2, 5, 2, 1),
          checkboxInput("blessed", "Blessed Strikes (Tasha's) instead of Divine Strike / Potent Spellcasting", FALSE),
          conditionalPanel("input.subclass == 'War'", sliderInput("war_priest_p", "War Priest bonus attack: fraction of rounds", 0, 1, 0.3, 0.05)),
          conditionalPanel("input.subclass == 'Death'", sliderInput("touch_death_p", "Touch of Death: fraction of rounds", 0, 1, 0.2, 0.05))),
        # Bard
        conditionalPanel("input.class == 'Bard' && ['Swords','Whispers'].indexOf(input.subclass) > -1",
          sliderInput("flourish_p", "Bardic Inspiration spent on damage: fraction of rounds", 0, 1, 0.5, 0.05)),
        # Druid
        conditionalPanel("input.class == 'Druid'",
          conditionalPanel("input.subclass == 'Moon'", selectInput("beast", "Wild Shape form (filtered by level)", beasts_for_level(5), selectize = FALSE)),
          conditionalPanel("input.subclass == 'Spores'",
            checkboxInput("symbiotic", "Symbiotic Entity active", TRUE),
            sliderInput("halo_p", "Halo of Spores: fraction of rounds an enemy is within 10 ft", 0, 1, 0.8, 0.05)),
          conditionalPanel("input.subclass == 'Stars'", checkboxInput("starry", "Starry Form (Archer) active", TRUE))),
        # Artificer
        conditionalPanel("input.class == 'Artificer'",
          conditionalPanel("input.subclass == 'Battle Smith' && input.level >= 9", sliderInput("jolt_p", "Arcane Jolt: fraction of rounds", 0, 1, 0.5, 0.05)),
          conditionalPanel("input.subclass == 'Armorer'", radioButtons("armor_model", "Armor model", c("Guardian", "Infiltrator"))),
          conditionalPanel("input.subclass == 'Artillerist'", checkboxInput("cannon", "Eldritch Cannon (force ballista) active", TRUE))),
        # Wizard / Sorcerer
        conditionalPanel("input.class == 'Wizard' && input.subclass == 'Bladesinging'", checkboxInput("bladesong", "Bladesong active (+INT AC; Song of Victory at 14)", TRUE)),
        conditionalPanel("input.class == 'Sorcerer'",
          conditionalPanel("input.level >= 3", sliderInput("quicken_p", "Quickened Spell (2nd cantrip): fraction of rounds", 0, 1, 0.3, 0.05)),
          conditionalPanel("input.subclass == 'Draconic'", checkboxInput("affinity", "Elemental Affinity applies (+CHA, lvl 6+)", TRUE)))
      ),
      accordion_panel("Party buffs",
        checkboxInput("bless", "Bless (+1d4 to attack rolls)", FALSE),
        checkboxInput("haste", "Haste (one extra weapon attack)", FALSE),
        checkboxInput("adv_source", "Advantage from an ally or condition", FALSE)
      ),
      accordion_panel("Target & enemy",
        layout_columns(col_widths = c(7, 5),
          selectInput("cr", "DMG preset by CR", ENEMIES_BY_CR$cr, 5),
          actionButton("load_cr", "Load", class = "btn-sm btn-outline-primary", style = "margin-top: 32px")),
        helpText("Loads AC, HP, attack bonus, damage split over 2 attacks, and save DC from the DMG table."),
        sliderInput("target_ac", "Target AC", 8, 25, 15, 1),
        numericInput("target_hp", "Target HP (for rounds-to-kill)", 90, 1, 1000),
        numericInput("t_save", "Target saving-throw bonus (for save-based damage)", 2, -2, 12),
        sliderInput("extra_targets", "Multi-target credit: additional targets in reach", 0, 3, 0, 1),
        checkboxInput("target_large", "Target is Large or larger (Giant Killer)", FALSE),
        tags$hr(),
        numericInput("e_n", "Enemy attacks / round", 2, 1, 8),
        numericInput("e_bonus", "Enemy attack bonus", 6, 0, 20),
        layout_columns(col_widths = c(6, 6), numericInput("e_ndice", "Dice", 1, 0, 20), numericInput("e_die", "Die", 8, 0, 20)),
        numericInput("e_flat", "Flat damage", 4, 0, 50),
        selectInput("e_type", "Damage type", ALL_DAMAGE_TYPES, "slashing"),
        checkboxInput("e_melee", "Enemy attacks are melee", TRUE),
        checkboxInput("e_adv", "Enemy has advantage against you (other than from Reckless Attack)", FALSE)
      ),
      accordion_panel("Simulation",
        sliderInput("nsim", "Simulated rounds", 1000, 50000, 5000, 1000),
        numericInput("seed", "Seed", 42, 1, 1e6),
        radioButtons("dodge_policy", "Uncanny Dodge policy (simulation only)", c("React to the first hit" = "first", "React to the largest hit (hindsight)" = "largest")),
        tags$hr(), tags$b("Combat tab"),
        sliderInput("max_rounds", "Maximum rounds per combat", 3, 30, 12, 1),
        checkboxInput("nova", "Round-1 nova (Action Surge, Fighting Spirit, Assassinate, Dread Ambusher)", TRUE),
        checkboxInput("surprised", "Target is surprised in round 1 (Assassinate auto-crit)", FALSE),
        numericInput("con_save", "Your CON saving-throw bonus (concentration)", 2, -2, 15),
        checkboxInput("war_caster", "War Caster (advantage on concentration saves)", FALSE)
      )
    )
  ),
  tags$style(HTML(MOBILE_CSS)),
  # Load plotly.js synchronously before Shiny connects. Shinylive injects htmlwidget scripts
  # dynamically, and the first widget render can run before the 3.5 MB bundle has arrived
  # ("Plotly is not defined"), which aborts Shiny's message handling and leaves outputs grey.
  tags$script(src = "plotly.min.js"),
  tags$script(HTML(PLOT_JS)),
  navset_card_tab(id = "main_tab",
    nav_panel("Offense",
      vb_row(
        value_box("Exact DPR", textOutput("off_exact"), theme = "primary"),
        value_box("Simulated mean ± SE", textOutput("off_sim")),
        value_box("Median · 5–95%", textOutput("off_q")),
        value_box("P(zero damage)", textOutput("off_p0"))),
      card(card_header("Distribution of damage per round"), plt("off_hist")),
      card(card_header("Term-by-term breakdown (closed form)"), tbl("off_table"))
    ),
    nav_panel("Defense",
      vb_row(
        value_box("Exact damage taken / round", textOutput("def_exact"), theme = "danger"),
        value_box("Simulated mean ± SE", textOutput("def_sim")),
        value_box("Median · 5–95%", textOutput("def_q")),
        value_box("Armor Class", textOutput("def_ac"))),
      card(card_header("Distribution of damage taken per round"), plt("def_hist")),
      card(card_header("Breakdown"), tbl("def_table")),
      card(card_header("Damage reflected to attacker"), tbl("reflect_table"))
    ),
    nav_panel("Rounds to…",
      pair(
        card(card_header("Rounds to kill the target"), plt("rtk_hist", "260px"), tbl("rtk_table")),
        card(card_header(textOutput("rtd_header")), plt("rtd_hist", "260px"), tbl("rtd_table")))
    ),
    nav_panel("Combat",
      helpText("Whole combats, simulated with state: damage stops at the kill, resource pools are spent greedily instead of the sliders, ",
               "concentration can break, round-1 nova features fire, Colossus Slayer waits for a damaged target, Toll the Dead starts on a d8. ",
               "Uses the Target HP / Character HP inputs. Reaction attacks are not included here."),
      vb_row(
        value_box("P(you win)", textOutput("cb_pwin"), theme = "primary"),
        value_box("Rounds to kill (median)", textOutput("cb_kill")),
        value_box("P(you drop first or time out)", textOutput("cb_pdrop"), theme = "danger"),
        value_box("Your HP left when you win (mean)", textOutput("cb_hpleft"))),
      pair(
        card(card_header("Round the target dies"), plt("cb_kill_hist", "240px")),
        card(card_header("Round you drop (if you do)"), plt("cb_drop_hist", "240px"))),
      card(card_header("Total damage dealt per combat"), plt("cb_dealt_hist", "240px")),
      card(card_header("Summary"), tbl("cb_table"))
    ),
    nav_panel("Trade-off",
      card(card_header("Offensive DPR vs. target AC (closed form)"), plt("tradeoff_plot", "320px")),
      card(card_header("Damage taken per round vs. enemy attack bonus, at your current AC"), plt("defense_curve_plot", "260px"))
    ),
    nav_panel("Progression",
      helpText("Levels 1–20 with every other input held fixed (ability modifiers are not bumped for ASIs; raise them by hand to compare)."),
      card(card_header("DPR and damage taken by level"), plt("progression_plot", "340px")),
      card(card_header("Table"), tbl("progression_table"))
    ),
    nav_panel("Formula",
      withMathJax(),
      card(card_header("Offense — each term with the current numbers substituted"), uiOutput("formula_off")),
      card(card_header("Defense"), uiOutput("formula_def")),
      card(card_header("Symbols"), helpText(
        "A = attacks per round, p_h / p_c = hit / crit probability, D_h / D_c = damage on a hit / crit, ",
        "q = P(first hit is a crit), m = resistance multiplier, n = enemy attacks. ",
        "Once-per-turn riders ride the first hit among the attacks they are allowed to ride; per-hit riders are summed over every attack."))
    ),
    nav_panel("Build",
      card(card_header("Active features"), tbl("features")),
      card(card_header("Modelling notes"), uiOutput("notes"))
    )
  )
)

server <- function(input, output, session) {
  # ---- choice lists that depend on class / subclass / level (no renderUI) ------------
  routine_choices <- function(cl, sc, lv) {
    nowc <- function(x) setdiff(x, c("Booming Blade", "Green-Flame Blade"))
    switch(cl,
      Warlock   = list(ch = c("Eldritch Blast", "Pact weapon", nowc(setdiff(cantrips_for("Warlock"), "Eldritch Blast"))), sel = "Eldritch Blast"),
      Cleric    = list(ch = c("Weapon", nowc(cantrips_for("Cleric"))), sel = "Weapon"),
      Bard      = list(ch = c("Weapon", nowc(cantrips_for("Bard"))), sel = if (sc %in% c("Swords", "Valor", "Whispers")) "Weapon" else "Vicious Mockery"),
      Druid     = list(ch = c(if (sc == "Moon") "Wild Shape", "Shillelagh", nowc(cantrips_for("Druid"))), sel = if (sc == "Moon") "Wild Shape" else "Shillelagh"),
      Artificer = list(ch = nowc(cantrips_for("Artificer")), sel = "Fire Bolt"),
      Wizard    = list(ch = c(if (sc == "Bladesinging") c("Weapon", if (lv >= 6) "Weapon + cantrip"), nowc(cantrips_for("Wizard"))), sel = if (sc == "Bladesinging") "Weapon" else "Fire Bolt"),
      Sorcerer  = list(ch = nowc(cantrips_for("Sorcerer")), sel = "Fire Bolt"),
      Fighter   = list(ch = c("Attack action", "War Magic"), sel = "Attack action"),
      list(ch = "Attack action", sel = "Attack action"))
  }
  style_choices <- function(cl) FIGHTING_STYLES[[cl]] %||% c("None")

  observeEvent(input$class, {
    cl <- input$class
    updateSelectInput(session, "subclass", choices = CLASS_SUBCLASSES[[cl]])
    updateSelectInput(session, "weapon", selected = DEFAULT_WEAPON[[cl]])
    updateSelectInput(session, "armor", selected = DEFAULT_ARMOR[[cl]])
    updateSelectInput(session, "fighting_style", choices = style_choices(cl), selected = style_choices(cl)[1])
  }, ignoreInit = TRUE)
  observeEvent(list(input$class, input$subclass, input$level), {
    req(input$subclass %in% CLASS_SUBCLASSES[[input$class]])
    rc <- routine_choices(input$class, input$subclass, input$level)
    if (!identical(rc$ch, isolate(routine_state$ch)) ) {
      updateRadioButtons(session, "routine", choices = rc$ch, selected = if (isolate(input$routine) %in% rc$ch) isolate(input$routine) else rc$sel)
      routine_state$ch <- rc$ch }
    o <- class_options(input$class, input$subclass)
    if (!is.null(o)) updateSelectInput(session, "option", label = o$label, choices = o$choices)
    if (input$class == "Druid" && input$subclass == "Moon") {
      bl <- beasts_for_level(max(input$level, 2)); updateSelectInput(session, "beast", choices = bl, selected = if (isolate(input$beast) %in% bl) isolate(input$beast) else bl[1]) }
  })
  routine_state <- reactiveValues(ch = "Attack action")

  # Only these inputs feed the builders. Never use reactiveValuesToList(input) here: it would
  # also depend on htmlwidget event inputs (plotly_afterplot, …), so every plot render would
  # trigger a rebuild and re-render — an infinite loop that showed up as a permanently busy app.
  BUILD_INPUTS <- c("class", "subclass", "option", "level", "str", "dex", "con", "int", "wis", "cha",
                    "weapon", "shield", "offhand", "offhand_weapon", "armor", "magic", "magic_ndice", "magic_die",
                    "feats", "lucky_p", "bless", "haste", "adv_source", "target_ac", "t_save", "extra_targets", "target_large",
                    "e_melee", "fighting_style", "superior_p", "routine", "cantrip", "raging", "reckless", "bonus_attack", "inf_fury_p",
                    "surge_p", "maneuver_p", "samurai_fs", "rune_might", "echo_p", "cavalier_p", "arcane_p", "psi_p",
                    "sneak_p", "homing_p", "wails_p", "weapon_cantrip", "uncanny", "flurry_p", "kensei_parry", "ki_rider_p", "sharpen", "astral_on",
                    "smite_policy", "smite_slot", "smite_p", "undead", "sacred_weapon", "vow", "mark", "hunter3", "colossus_p", "companion", "command",
                    "agonizing", "hex", "thirsting", "esmite_p", "curse", "dread", "tentacle_p", "spiritual", "sw_slot", "blessed", "war_priest_p", "touch_death_p",
                    "flourish_p", "beast", "symbiotic", "halo_p", "starry", "jolt_p", "armor_model", "cannon", "bladesong", "quicken_p", "affinity")
  collect_inputs <- function() { out <- lapply(BUILD_INPUTS, function(id) input[[id]]); names(out) <- BUILD_INPUTS; out[!vapply(out, is.null, logical(1))] }

  # Guards: during a class change the dependent inputs lag by one flush; coerce them to valid values
  sanitize <- function(inp) {
    cl <- inp$class
    if (!isTRUE(inp$subclass %in% CLASS_SUBCLASSES[[cl]])) inp$subclass <- CLASS_SUBCLASSES[[cl]][1]
    rc <- routine_choices(cl, inp$subclass, inp$level)
    if (!isTRUE(inp$routine %in% rc$ch)) inp$routine <- rc$sel
    if (!isTRUE(inp$fighting_style %in% style_choices(cl))) inp$fighting_style <- style_choices(cl)[1]
    o <- class_options(cl, inp$subclass)
    if (is.null(o)) inp$option <- NULL else if (!isTRUE(inp$option %in% unname(o$choices))) inp$option <- unname(o$choices)[1]
    if (cl == "Druid" && !isTRUE(inp$beast %in% beasts_for_level(max(inp$level, 2)))) inp$beast <- beasts_for_level(max(inp$level, 2))[1]
    inp
  }

  # UI defaults already match Barbarian, so only a real class change pushes new modifiers
  observeEvent(input$class, {
    d <- ABILITY_DEFAULTS[[input$class]]
    for (ab in names(d)) updateNumericInput(session, ab, value = unname(d[[ab]]))
  }, ignoreInit = TRUE)

  observeEvent(input$load_cr, {
    p <- cr_preset(as.numeric(input$cr))
    updateSliderInput(session, "target_ac", value = p$target_ac)
    for (nm in c("target_hp", "t_save", "e_n", "e_bonus", "e_ndice", "e_die", "e_flat")) updateNumericInput(session, nm, value = p[[nm]])
  })

  # ---- build -------------------------------------------------------------------
  build <- reactive({
    inp <- sanitize(collect_inputs())
    inp$enemy_mode <- if (isTRUE(input$e_adv)) "advantage" else "flat"
    inp$armor <- inp$armor %||% DEFAULT_ARMOR[[input$class]]
    build_character(inp)
  })
  enemy_mode <- reactive({ b <- build()
    if (b$enemy_roll_mode == "advantage" || (isTRUE(input$e_adv) && input$class != "Rogue")) "advantage" else b$enemy_roll_mode })
  enemy <- reactive(enemy_attack("Enemy", input$e_n, input$e_bonus, input$e_ndice, input$e_die, input$e_flat, input$e_type))
  first_hit <- reactive({ b <- build(); b$first_hit })

  def_exact <- reactive({ b <- build(); damage_taken(enemy(), b$ac, enemy_mode(), resist = b$resist, first_hit = first_hit(), flat_reduction = b$flat_reduction %||% 0) })
  first_hit_sim <- reactive({ fh <- first_hit(); if (!is.null(fh) && fh$mode == "halve") fh$policy <- input$dodge_policy; fh })
  def_sim   <- reactive({ b <- build(); simulate_damage_taken(enemy(), b$ac, enemy_mode(), resist = b$resist, first_hit = first_hit_sim(), flat_reduction = b$flat_reduction %||% 0, n = input$nsim, seed = input$seed + 1) })
  # reaction attacks (Retaliation, Giant Killer) fire when the enemy hits you
  p_hit_taken <- reactive({ d <- def_exact()$breakdown; d <- d[!is.na(d$attacks), ]
    if (!input$e_melee && !is.null(build()$reaction) && isTRUE(build()$reaction$melee_only)) 0 else 1 - prod((1 - d$p_hit)^d$attacks) })
  build_off <- reactive(with_reaction(build(), p_hit_taken()))

  off_exact <- reactive({ b <- build_off(); round_dpr(b$events, b$attack_bonus, input$target_ac, b$roll_mode, b$crit_min, b$to_hit_die %||% 0) })
  off_sim   <- reactive({ b <- build_off(); simulate_round(b$events, b$attack_bonus, input$target_ac, b$roll_mode, b$crit_min, b$to_hit_die %||% 0, n = input$nsim, seed = input$seed) })

  # Chart specs: plain lists sent to plotly.js (Plotly.react) via a custom message. Data are
  # pre-aggregated in R so each message is a few KB. I() keeps length-1 vectors as JSON arrays.
  base_layout <- function(xlab, ylab, legend = FALSE, extra = list()) {
    modifyList(list(
      xaxis = list(title = list(text = xlab), zeroline = FALSE), yaxis = list(title = list(text = ylab), zeroline = FALSE),
      margin = list(l = 50, r = 10, t = 20, b = 50), hovermode = "x", font = list(size = 12),
      showlegend = legend, legend = list(orientation = "h", x = 0, y = 1.12), bargap = 0.05,
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", autosize = TRUE), extra)
  }
  PLOT_CONFIG <- list(displayModeBar = FALSE, responsive = TRUE, displaylogo = FALSE)
  spec <- function(data, layout) list(data = data, layout = layout, config = PLOT_CONFIG)
  hist_spec <- function(x, exact, fill, xlab, ylab = "Simulated rounds") {
    bw <- max(1, round(diff(range(x)) / 40)); br <- seq(floor(min(x)), max(x) + bw, by = bw)
    h <- hist(x, breaks = br, plot = FALSE)
    spec(list(
      list(x = I(h$mids), y = I(h$counts), type = "bar", width = I(rep(bw, length(h$mids))), marker = list(color = fill),
           hovertemplate = "%{x:.0f}: %{y} rounds<extra></extra>"),
      list(x = I(c(exact, exact)), y = I(c(0, max(h$counts))), type = "scatter", mode = "lines",
           line = list(color = if (fill == "#2c3e50") "#e74c3c" else "#2c3e50", width = 2),
           hovertemplate = paste0("exact mean ", fmt(exact, 2), "<extra></extra>"))),
      base_layout(xlab, ylab))
  }
  bar_spec <- function(x, fill, xlab, ylab) {
    x <- x[!is.na(x)]
    if (!length(x)) return(spec(list(list(x = I(0), y = I(0), type = "bar")), base_layout(xlab, ylab)))
    t <- table(factor(x, levels = seq(min(x), max(x))))
    spec(list(list(x = I(as.numeric(names(t))), y = I(as.numeric(t)), type = "bar", marker = list(color = fill),
                   hovertemplate = "round %{x}: %{y}<extra></extra>")),
         base_layout(xlab, ylab, extra = list(xaxis = list(title = list(text = xlab), dtick = 1, zeroline = FALSE))))
  }
  line_spec <- function(df, x, y, group = NULL, colours, vline = NULL, xlab, ylab, dtick = NULL) {
    groups <- if (is.null(group)) list(NULL) else as.list(unique(df[[group]]))
    traces <- lapply(seq_along(groups), function(i) { g <- groups[[i]]; d <- if (is.null(g)) df else df[df[[group]] == g, ]
      col <- colours[(i - 1) %% length(colours) + 1]
      list(x = I(d[[x]]), y = I(round(d[[y]], 2)), type = "scatter", mode = "lines+markers", name = g %||% "",
           line = list(color = col, width = 2), marker = list(color = col),
           hovertemplate = paste0(if (is.null(g)) "" else paste0(g, ": "), "%{y}<extra></extra>")) })
    lay <- base_layout(xlab, ylab, legend = !is.null(group))
    if (!is.null(dtick)) lay$xaxis$dtick <- dtick
    if (!is.null(vline)) lay$shapes <- list(list(type = "line", x0 = vline, x1 = vline, y0 = 0, y1 = 1, yref = "paper", line = list(dash = "dot", color = "grey")))
    spec(traces, lay)
  }
  # send a chart only while its tab is showing (keeps the old suspend-when-hidden laziness)
  plot_when <- function(tab, id, expr) observe({ req(identical(input$main_tab, tab)); session$sendCustomMessage("dpr-plot", c(list(id = id), expr())) })
  fmt <- function(x, d = 1) formatC(x, format = "f", digits = d)
  qtxt <- function(s) sprintf("%s · %s–%s", fmt(s$q50), fmt(s$q5), fmt(s$q95))
  rnd <- function(df) df %>% mutate(across(where(is.numeric), ~ round(.x, 3)))

  output$off_exact <- renderText(fmt(off_exact()$total, 2))
  output$off_sim   <- renderText({ s <- summarize_sim(off_sim()); sprintf("%s ± %s", fmt(s$mean, 2), fmt(s$se_mean, 2)) })
  output$off_q     <- renderText(qtxt(summarize_sim(off_sim())))
  output$off_p0    <- renderText(sprintf("%.1f%%", 100 * mean(off_sim() == 0)))
  output$off_table <- renderTable(rnd(off_exact()$breakdown), striped = TRUE)
  plot_when("Offense", "off_hist", function() hist_spec(off_sim(), off_exact()$total, "#2c3e50", "Damage dealt in a round (line = exact mean)"))

  output$def_exact <- renderText(fmt(def_exact()$total, 2))
  output$def_sim   <- renderText({ s <- summarize_sim(def_sim()); sprintf("%s ± %s", fmt(s$mean, 2), fmt(s$se_mean, 2)) })
  output$def_q     <- renderText(qtxt(summarize_sim(def_sim())))
  output$def_ac    <- renderText(build()$ac)
  output$def_table <- renderTable(rnd(def_exact()$breakdown), striped = TRUE)
  output$reflect_table <- renderTable({
    b <- build(); r <- b$reflect
    if (!input$e_melee) r <- keep(r, ~ !.x$melee_only)
    if (length(r) == 0) return(data.frame(note = "None for this build / enemy."))
    b$reflect <- r
    reflect_damage(b, enemy(), def_exact()$breakdown) %>% mutate(across(where(is.numeric), ~ round(.x, 2)))
  })
  plot_when("Defense", "def_hist", function() hist_spec(def_sim(), def_exact()$total, "#c0392b", "Damage taken in a round (line = exact mean)"))

  rtk <- reactive(rounds_to_threshold(off_sim(), input$target_hp, n = min(input$nsim, 5000), seed = input$seed + 2))
  char_hp <- reactive({ b <- build(); b$hp_override %||% input$hp })
  rtd <- reactive(rounds_to_threshold(def_sim(), char_hp(),       n = min(input$nsim, 5000), seed = input$seed + 3))
  rt_table <- function(r) { s <- r$summary %>% mutate(across(everything(), ~ round(.x, 2))); s$censored_at_50 <- sprintf("%.1f%%", 100 * r$censored); s }
  output$rtd_header <- renderText(sprintf("Rounds until the character drops (%d HP%s)", char_hp(), if (!is.null(build()$hp_override)) ", beast form" else ""))
  output$rtk_table <- renderTable(rt_table(rtk()))
  output$rtd_table <- renderTable(rt_table(rtd()))
  plot_when("Rounds to…", "rtk_hist", function() bar_spec(rtk()$rounds, "#2c3e50", "Rounds to reduce target to 0 HP", "Simulated combats"))
  plot_when("Rounds to…", "rtd_hist", function() bar_spec(rtd()$rounds, "#c0392b", "Rounds until character reaches 0 HP", "Simulated combats"))

  inp_list <- reactive({ inp <- sanitize(collect_inputs()); inp$enemy_mode <- if (isTRUE(input$e_adv)) "advantage" else "flat"
    inp$armor <- inp$armor %||% DEFAULT_ARMOR[[input$class]]; inp })

  plot_when("Trade-off", "tradeoff_plot", function()
    line_spec(tradeoff_curves(inp_list()), "target_ac", "dpr", "variant", c("#2c3e50", "#e67e22", "#27ae60"), input$target_ac, "Target AC", "Expected damage per round"))
  plot_when("Trade-off", "defense_curve_plot", function()
    line_spec(defense_curve(build(), enemy(), enemy_mode()), "enemy_bonus", "taken", NULL, "#c0392b", input$e_bonus, "Enemy attack bonus", "Expected damage taken per round"))
  prog <- reactive(progression(inp_list(), input$target_ac, enemy(), isTRUE(input$e_adv)))
  plot_when("Progression", "progression_plot", function() {
    pr <- prog()
    df <- rbind(data.frame(level = pr$level, measure = "Damage dealt / round", value = pr$dpr),
                data.frame(level = pr$level, measure = "Damage taken / round", value = pr$taken))
    line_spec(df, "level", "value", "measure", c("#2c3e50", "#c0392b"), input$level, "Character level", "Expected damage per round", dtick = 1) })
  output$progression_table <- renderTable(prog() %>% mutate(across(c(dpr, taken), ~ round(.x, 2))))

  combat <- reactive({ b <- build()
    simulate_combat(b, enemy(), input$target_ac, input$target_hp, char_hp(), enemy_mode(), n = min(input$nsim, 5000),
                    max_rounds = input$max_rounds, seed = input$seed + 4, nova = isTRUE(input$nova), surprised = isTRUE(input$surprised),
                    con_save = input$con_save, war_caster = isTRUE(input$war_caster), e_melee = isTRUE(input$e_melee)) })
  output$cb_pwin   <- renderText(sprintf("%.1f%%", 100 * combat()$summary$p_win))
  output$cb_kill   <- renderText({ s <- combat()$summary; if (is.na(s$kill_round_median)) "never" else sprintf("%g (mean %.1f)", s$kill_round_median, s$kill_round_mean) })
  output$cb_pdrop  <- renderText(sprintf("%.1f%%", 100 * (1 - combat()$summary$p_win)))
  output$cb_hpleft <- renderText({ s <- combat()$summary; if (is.nan(s$char_hp_left_mean)) "—" else fmt(s$char_hp_left_mean) })
  output$cb_table  <- renderTable(combat()$summary %>% mutate(across(everything(), ~ round(.x, 3))))
  plot_when("Combat", "cb_kill_hist", function() bar_spec(combat()$kill_round, "#2c3e50", "Round the target dies", "Combats"))
  plot_when("Combat", "cb_drop_hist", function() bar_spec(combat()$drop_round, "#c0392b", "Round you drop", "Combats"))
  plot_when("Combat", "cb_dealt_hist", function() hist_spec(combat()$dealt_total, mean(combat()$dealt_total), "#2c3e50", "Damage dealt before the combat ended (line = mean)", "Combats"))

  # plain HTML + an inline typeset call: no MathJax dependency object inside renderUI (async
  # dependency rendering is what stalls Shinylive), MathJax itself is loaded once by withMathJax() in the UI
  typeset <- function(html) tagList(HTML(html), tags$script(HTML("if (window.MathJax && MathJax.Hub) MathJax.Hub.Queue(['Typeset', MathJax.Hub]);")))
  output$formula_off <- renderUI(typeset(formula_offense(off_exact()$breakdown, off_exact()$total)))
  output$formula_def <- renderUI(typeset(formula_defense(def_exact()$breakdown, def_exact()$total)))

  output$features <- renderTable(build()$features %>% select(level, subclass, feature, effect_type, value, notes))
  output$notes <- renderUI({
    b <- build()
    items <- c(sprintf("Attack bonus %+d; roll mode: %s; crit on %d+; AC %d; enemy rolls with %s.",
                       b$attack_bonus, b$roll_mode, b$crit_min, b$ac, enemy_mode()),
               sprintf("Resistances: %s.", if (length(b$resist)) paste(b$resist, collapse = ", ") else "none"),
               "Resistance and halving are exact in the closed form and rounded down in the simulation (PHB rule), so the simulated mean sits slightly below the exact value.",
               "Sliders labelled 'fraction of rounds' are the resource-smoothing assumption: sustained play, averaged over a day, not a nova round.",
               b$notes)
    tags$ul(lapply(items, tags$li))
  })
}

shinyApp(ui, server)
