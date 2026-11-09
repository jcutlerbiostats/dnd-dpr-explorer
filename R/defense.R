# 
# defense.R — AC construction --------------------------------------------------
ARMOR <- read_csv(file.path(DATA_DIR, "armor.csv"), show_col_types = FALSE)

# armor_ac(): standard AC from the armor table.
#   unarmored : NULL, or a function(dex, con, wis) used when armor == "None"
#               (Barbarian / Monk Unarmored Defense)
armor_ac <- function(armor = "None", dex_mod = 0, shield = FALSE, misc = 0,
                     unarmored = NULL, con_mod = 0, wis_mod = 0) {
  a <- ARMOR[ARMOR$name == armor, ]
  if (nrow(a) != 1) stop("unknown armor: ", armor)
  base <- if (armor == "None" && !is.null(unarmored)) unarmored(dex_mod, con_mod, wis_mod)
          else a$base + min(dex_mod, a$dex_cap)
  base + (if (shield) 2 else 0) + misc
}
