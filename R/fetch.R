##################################################
## Fetch the current SoyBase parentage data
##
## SoyBase's parentage viewer (https://www.soybase.org/tools/parentage)
## is backed by three tab-separated files kept in the soybase/parentage
## GitHub repository. This replaces the old scraped
## SoybasePedigreeData.csv table.
##################################################

soybase_parentage_files <- c(
  parentage = "parentage.tsv",
  synonyms  = "parentage-synonyms.tsv",
  comments  = "parentage-comments.tsv"
)

# Resolve a branch or tag in soybase/parentage to a commit SHA so a
# build can record exactly which version of the data it used.
resolve_soybase_ref <- function(ref = "main") {
  url <- sprintf("https://api.github.com/repos/soybase/parentage/commits/%s", ref)
  sha <- tryCatch(jsonlite::fromJSON(url)$sha, error = function(e) NA_character_)
  if (is.na(sha)) {
    warning("Could not resolve ref '", ref, "' to a commit; using it as given.")
    return(ref)
  }
  sha
}

# Download the parentage, synonym, and comment files into `dest`.
# Returns the paths of the downloaded files, named by type.
fetch_soybase_parentage <- function(dest = "data-raw/soybase", ref = "main") {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  sha <- resolve_soybase_ref(ref)

  paths <- file.path(dest, soybase_parentage_files)
  names(paths) <- names(soybase_parentage_files)

  for (i in seq_along(soybase_parentage_files)) {
    url <- sprintf("https://raw.githubusercontent.com/soybase/parentage/%s/data/%s",
                   sha, soybase_parentage_files[[i]])
    utils::download.file(url, paths[[i]], quiet = TRUE, mode = "wb")
  }

  writeLines(c(
    "source: https://github.com/soybase/parentage",
    paste("commit:", sha),
    paste("fetched:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
  ), file.path(dest, "SOURCE.txt"))

  paths
}
