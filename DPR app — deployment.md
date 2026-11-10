---
tags:
  - dnd
  - dpr
  - r
  - shiny
  - deployment
created: 2026-11-10
status: live
---
# DPR app — deployment

**Live app:** https://jcutlerbiostats.github.io/dnd-dpr-explorer/
**Repository:** https://github.com/jcutlerbiostats/dnd-dpr-explorer (public, `jcutlerbiostats` GitHub account)
**Hosting:** [Shinylive](https://posit-dev.github.io/r-shinylive/) on GitHub Pages — R compiled to WebAssembly (webR) runs entirely in the visitor's browser. No server, nothing to pay for, nothing sleeps.

Deployed 2026-11-10. See [[DPR app — project overview]] for what the app does and [[Shiny DPR app — plan]] for the build history.

---
## Why this option
- **shinyapps.io** was rejected as slow (shared server, cold starts).
- **Shinylive** is free, never idles, and after the first load everything runs locally in the browser, so it is fast on any device — including a phone with nothing installed.
- Fallbacks if it ever becomes a problem: Posit Connect Cloud (free tier, GitHub-deployed), Hugging Face Spaces (Docker, sleeps when idle), a local install of R + RStudio.

## What a visitor should expect
- **First visit:** the interface appears after roughly 50 seconds and the numbers and chart after about 70, while webR downloads ~30 MB of packages. Wi-Fi is better than cellular for that first visit. If it looks stuck, wait rather than reload.
- **Later visits:** cached by the browser's service worker; loads in a few seconds.
- The simulations run on the visitor's device. 5,000 simulated rounds (the default) is instant; the Combat tab's whole-fight simulation takes a second or two.

---
## How updates work
1. Edit the project (`app.R`, `R/`, `data/`, `www/`) and run the tests: `testthat::test_dir("tests/testthat")`.
2. Optional local preview of the *exported* bundle (this is what catches webR-only problems):
   ```r
   Rscript scripts/export_shinylive.R            # stages app.R, R/, data/, www/ -> _site/
   Rscript -e 'httpuv::runStaticServer("_site")' # open the printed URL
   ```
   `_site/` is git-ignored.
3. Commit and push to `main`. The GitHub Actions workflow (`.github/workflows/deploy.yml`) installs R and the packages, runs the export script, and publishes `_site/` to Pages. About 3 minutes. Watch it with `gh run watch`.

Pages is configured with **build type = GitHub Actions** (set via `gh api repos/.../pages -f build_type=workflow`), so nothing is served from the branch itself.

---
## Files that exist only for deployment
| File | Purpose |
|---|---|
| `scripts/export_shinylive.R` | Copies only the app files to a staging dir and calls `shinylive::export()`; keeps notes, tests and the `.Rproj` out of the bundle. |
| `.github/workflows/deploy.yml` | Build + deploy on push to `main` (and manual `workflow_dispatch`). |
| `www/plotly.min.js` | plotly.js (3.7 MB) shipped with the app and loaded with a plain `tags$script` before Shiny connects — see gotcha 2. |
| `.gitignore` | ignores `_site/`, `.Rproj.user/`, `.Rhistory`, `.DS_Store`. |
| `README.md` | short public-facing description. |

---
## Mobile-first changes made for this deployment
- All ten charts are **native plotly** with the data pre-aggregated in R (`hist()`, `table()`), 3–7 KB per chart; tooltips and pinch/zoom work on touch. `ggplotly()` was dropped — far too slow in webR.
- `page_sidebar(fillable = FALSE)`: the page scrolls naturally instead of squeezing cards into the viewport.
- Sidebar collapses to a toggle on phones (`sidebar(open = list(desktop = "open", mobile = "closed"))`).
- Value boxes in an explicit CSS grid: two-up below 992 px, four-up above.
- Tab strip and tables scroll sideways instead of wrapping; smaller fonts under 576 px; plotly mode bar hidden on phones.
- Per-class sensible ability modifiers load when the class changes.
- **Not yet verified on a real phone** — Chrome on the Mac won't shrink below ~500 px and Shinylive's service worker refuses to run inside an iframe wrapper. James will report anything that looks off.

---
## webR / Shinylive gotchas found (all fixed, keep in mind when editing)
1. **`munsell` missing.** Shinylive's bundled webR is R 4.4 with ggplot2 3.5.2 / scales 1.3.0, older than the local library, and the exporter resolves dependencies from *local* metadata. Result: "there is no package called 'munsell'" and a blank page. Fix: `requireNamespace("munsell", quietly = TRUE)` near the top of `app.R` so it gets bundled (plotly loads ggplot2, which needs it).
2. **"Plotly is not defined".** Shinylive injects htmlwidget scripts asynchronously, so the first chart render could run before the 3.5 MB plotly.js arrived; the exception aborted Shiny's message handling and left every output greyed. Fix: plotly.js in `www/`, loaded synchronously as a body `<script>`.
3. **Infinite re-render loop (the big one).** `build()` used `reactiveValuesToList(input)`, which also depends on the `plotly_afterplot` event input every chart sends back → each render rebuilt the character → re-rendered the chart → forever. The app looked permanently busy while R sat idle. Fix: an explicit `BUILD_INPUTS` vector and `collect_inputs()`; a testServer check asserts plotly events no longer invalidate `build()`. **Never depend on all inputs in a reactive that feeds an htmlwidget.**
4. **No `renderUI`.** Shiny's dynamic UI rendering is asynchronous (made so for Shinylive) and fragile under the PostMessage channel. All class controls are now static, shown with `conditionalPanel`, with choice lists swapped by `updateSelectInput()` / `updateRadioButtons()`; `sanitize()` coerces inputs that lag one flush behind during a class switch. The Formula tab's MathJax output is plain HTML plus an inline typeset call, not `withMathJax()` inside `renderUI`.
5. **`page_sidebar(header = tags$head(...))`** is silently dropped by the webR bslib version. The CSS block is a plain child of the main area instead.
6. **Service-worker caching.** After a re-export, test on a **new port** (or hard-reload with the service worker bypassed) or you will see the previous bundle.
7. **Package version warnings** during export ("Installed version X, WebAssembly version Y") are expected; webR tracks an older CRAN snapshot.
8. **Diagnostics that worked:** the exported `edit/` page gives a live R console sharing the app's session (`geterrmessage()`, timing functions directly in webR); patching `Shiny.shinyapp.dispatchMessage` in the browser console to log server messages is what exposed the render loop.

---
## Useful commands
```bash
gh run list --limit 3                      # recent deploys
gh run watch <id>                          # follow one
gh repo view jcutlerbiostats/dnd-dpr-explorer --web
```
```r
shiny::runApp()                                          # local dev (native R)
testthat::test_dir("tests/testthat")                     # 76 test blocks, ~1,700 expectations
Rscript scripts/export_shinylive.R                       # build the browser bundle locally
```

## Related
- [[DPR app — project overview]] · [[Shiny DPR app — plan]]
- [[Barbarian (Level 1) — Offensive & Defensive DPR]] · [[Barbarian (Path of the Zealot) — Offensive & Defensive DPR]]
