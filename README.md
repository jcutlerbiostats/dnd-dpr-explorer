# D&D 5e DPR explorer

A Shiny app that computes offensive damage per round and expected damage taken per round for
D&D 5e (2014 rules) characters — all 13 classes and their published subclasses — with an exact
closed form, a Monte Carlo simulator that cross-checks it, and a stateful whole-combat simulator.

- **Run locally:** open the project in R and run `shiny::runApp()`.
- **Tests:** `testthat::test_dir("tests/testthat")`.
- **Deploy:** pushes to `main` build a [Shinylive](https://posit-dev.github.io/r-shinylive/) site
  (R compiled to WebAssembly, runs entirely in the browser) and publish it to GitHub Pages.
  Local preview: `Rscript scripts/export_shinylive.R` then `Rscript -e 'httpuv::runStaticServer("_site")'`.

Sources: class text from [dnd5e.wikidot.com](https://dnd5e.wikidot.com/), beast stat blocks from aidedd.org,
enemy presets from the DMG "Monster Statistics by Challenge Rating" table.

See `DPR app — project overview.md` for the full description of the model and its phases.
