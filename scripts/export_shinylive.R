# Export the app to a static Shinylive site ------------------------------------
#
# Stages only what the app needs (app.R, R/, data/, www/) so the Obsidian notes,
# tests and .Rproj are not shipped, then writes the site to `_site/`.
# Local preview:  Rscript scripts/export_shinylive.R && Rscript -e 'httpuv::runStaticServer("_site")'
stage <- file.path(tempdir(), "dpr_stage"); unlink(stage, recursive = TRUE); dir.create(stage)
ok <- file.copy(c("app.R", "R", "data", "www"), stage, recursive = TRUE)
stopifnot(all(ok))
shinylive::export(stage, "_site", quiet = TRUE)
cat("exported to _site/ (", round(sum(file.info(list.files("_site", recursive = TRUE, full.names = TRUE))$size) / 1e6, 1), "MB )\n")
