##################################################
## Load the app's data tables
##
## Reads the tables written by data-raw/build_data.R and prepares the
## lookups the app needs, once at startup.
##################################################

load_app_data <- function(dir = "data") {
  ped <- readr::read_csv(
    file.path(dir, "pedigree_expanded.csv"),
    col_types = readr::cols(
      self_gens = readr::col_double(),
      named     = readr::col_logical(),
      repaired  = readr::col_logical(),
      .default  = readr::col_character()
    ),
    na = ""
  )
  lines <- readr::read_csv(
    file.path(dir, "lines.csv"),
    col_types = readr::cols(
      has_parents  = readr::col_logical(),
      nested_cross = readr::col_logical(),
      .default     = readr::col_character()
    ),
    na = ""
  )
  traits <- readr::read_csv(
    file.path(dir, "grin_traits.csv"),
    col_types = readr::cols(.default = readr::col_character()),
    na = ""
  )

  named_ids <- sort(ped$id[ped$named])
  info <- tibble(id = named_ids) |>
    left_join(lines |> select(id = individual, female, male, pi_number,
                              synonyms, comments),
              by = "id")

  list(
    ped     = ped,
    graph   = pedigree_graph(ped),
    lines   = info,
    traits  = traits,
    choices = line_choices(info),
    source  = data_source_note(dir)
  )
}

# Choices for the line search box. Synonyms (PI numbers, experimental
# names) are searchable but not shown in the box itself.
line_choices <- function(info) {
  tibble(
    value    = info$id,
    label    = info$id,
    synonyms = coalesce(info$synonyms, "")
  )
}

data_source_note <- function(dir) {
  src <- file.path(dirname(normalizePath(dir, mustWork = FALSE)),
                   "data-raw", "soybase", "SOURCE.txt")
  if (file.exists(src)) paste(readLines(src), collapse = "\n") else ""
}
