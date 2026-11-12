# Export the app to a static Shinylive site ------------------------------------
#
# Stages only what the app needs (app.R, R/, data/, www/) so the Obsidian notes,
# tests and .Rproj are not shipped, then writes the site to `_site/`.
# Local preview:  Rscript scripts/export_shinylive.R && Rscript -e 'httpuv::runStaticServer("_site")'
stage <- file.path(tempdir(), "dpr_stage"); unlink(stage, recursive = TRUE); dir.create(stage)
ok <- file.copy(c("app.R", "R", "data", "www"), stage, recursive = TRUE)
stopifnot(all(ok))
shinylive::export(stage, "_site", quiet = TRUE)

# iOS smoke-test apps (tests/ios/*) exported as sub-sites: _site/test-basic/, _site/test-bslib/ ----
for (t in c("basic", "bslib")) {
  shinylive::export(file.path("tests/ios", t), file.path("_site", paste0("test-", t)), quiet = TRUE)
}

# Inject the diagnostic banner script into the host page (and the editor page) ----
diag <- paste(readLines("scripts/diag.js", warn = FALSE), collapse = "\n")
for (f in c("_site/index.html", "_site/edit/index.html", "_site/test-basic/index.html", "_site/test-bslib/index.html")) if (file.exists(f)) {
  html <- paste(readLines(f, warn = FALSE), collapse = "\n")
  html <- sub("<head>", paste0("<head>\n<script>", diag, "</script>"), html, fixed = TRUE)
  writeLines(html, f)
}
cat("exported to _site/ (", round(sum(file.info(list.files("_site", recursive = TRUE, full.names = TRUE))$size) / 1e6, 1), "MB )\n")
