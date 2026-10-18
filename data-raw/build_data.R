##################################################
## Build the app's data tables from source
##
## Run from the repository root:
##   Rscript data-raw/build_data.R          # fetch latest SoyBase data
##   Rscript data-raw/build_data.R offline  # reuse data-raw/soybase/
##
## Outputs (in data/):
##   parentage.csv  individual, female, male (raw SoyBase pedigree strings)
##   lines.csv      one row per individual with PI number, synonyms,
##                  comments, and parentage flags
##   grin_traits.csv GRIN evaluations joined to lines by PI number
##   notation_profile.txt  summary of pedigree notation, for checking
##                  each new data release
##################################################

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

args <- commandArgs(trailingOnly = TRUE)
src  <- "data-raw/soybase"

if (!"offline" %in% args) {
  fetch_soybase_parentage(src)
}

parentage <- read_parentage(file.path(src, soybase_parentage_files[["parentage"]]))
synonyms  <- read_synonyms(file.path(src, soybase_parentage_files[["synonyms"]]))
comments  <- read_comments(file.path(src, soybase_parentage_files[["comments"]]))

lines <- build_lines_table(parentage, synonyms, comments)

# GRIN evaluations. The current file is the 2021 export carried over
# from SoybasePedigreeVisualization; replace it with a fresh export
# from GRIN-Global when one is available.
grin <- read_grin_export("data-raw/grin/grin_results_2021.txt")
grin_traits <- lines |>
  filter(!is.na(pi_number)) |>
  select(individual, pi_number) |>
  inner_join(grin, by = "pi_number")

dir.create("data", showWarnings = FALSE)
readr::write_csv(parentage, "data/parentage.csv", na = "")
readr::write_csv(lines, "data/lines.csv", na = "")
readr::write_csv(grin_traits, "data/grin_traits.csv", na = "")

profile <- profile_notation(parentage)
capture.output({
  cat(readLines(file.path(src, "SOURCE.txt")), sep = "\n")
  cat("\n")
  str(profile[setdiff(names(profile), "token_types")])
  cat("\nParent name tokens by type:\n")
  print(profile$token_types)
  cat("\nLines with GRIN evaluations:", nrow(grin_traits), "\n")
}, file = "data/notation_profile.txt")

message("Wrote ", nrow(parentage), " parentage rows, ",
        nrow(grin_traits), " lines with GRIN traits.")
