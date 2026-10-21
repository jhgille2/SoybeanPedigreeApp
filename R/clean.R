##################################################
## Read and tidy the SoyBase parentage files
##
## Notation notes (checked against the files, Oct 2026):
## - Each row is `individual  parent1  parent2`; parent1 is the female.
## - A parent may itself be a nested cross written "( A , B )".
## - A recurrent-parent dose is written "Name N", e.g. Williams 82 is
##   "Williams 7 x Kingwa" (Williams used 7 times, i.e. BC6). The old
##   table wrote this as "Williams (7)", and the 2021 code read it as
##   selfing generations, which was wrong. Because the parentheses are
##   gone, "Williams 7" has to be told apart from real names such as
##   "Williams 82" by checking which names exist as lines.
## - Some backcross programs are already expanded into BC1, BC2, ...
##   rows (e.g. 02JR310007BC1, 02JR310007BC2).
##################################################

library(dplyr)
library(stringr)
library(purrr)

# Trim, squash internal whitespace, drop leftover HTML spaces, and turn
# empty or "-" values into NA.
clean_name <- function(x) {
  x <- str_replace_all(x, "&nbsp;?", " ")
  x <- str_squish(x)
  x[x %in% c("", "-", "Unspecified", "Unknown")] <- NA_character_
  x
}

read_soybase_tsv <- function(path, col_names) {
  readr::read_tsv(path, comment = "#", col_names = col_names,
                  col_types = readr::cols(.default = "c"),
                  na = character(), trim_ws = FALSE, progress = FALSE) |>
    mutate(across(everything(), clean_name))
}

read_parentage <- function(path) {
  read_soybase_tsv(path, c("individual", "female", "male")) |>
    filter(!is.na(individual)) |>
    distinct()
}

read_synonyms <- function(path) {
  read_soybase_tsv(path, c("individual", "synonym")) |>
    filter(!is.na(individual), !is.na(synonym)) |>
    distinct()
}

read_comments <- function(path) {
  read_soybase_tsv(path, c("individual", "comment")) |>
    filter(!is.na(individual), !is.na(comment)) |>
    distinct()
}

# Split a pedigree string into the plain names it mentions, ignoring the
# "( , )" cross structure.
parent_tokens <- function(x) {
  x <- x[!is.na(x)]
  toks <- unlist(str_split(x, "[(),]"))
  toks <- str_squish(toks)
  toks[toks != ""]
}

# Classify a single name token from a parent field.
#   "line"  - the token is a known individual
#   "dose"  - "Name N" where Name is a known line and "Name N" is not,
#             read as Name used N times as recurrent parent
#   "ambiguous" - both "Name N" and "Name" are known lines
#   "unknown" - not a known line (often a parent with no row of its own)
classify_token <- function(token, known) {
  m <- str_match(token, "^(.+) (\\d{1,2})$")
  base <- m[, 2]
  n <- suppressWarnings(as.integer(m[, 3]))
  dose_like <- !is.na(base) & !is.na(n) & n >= 2 & n <= 12

  case_when(
    dose_like & token %in% known & base %in% known ~ "ambiguous",
    token %in% known ~ "line",
    dose_like & base %in% known ~ "dose",
    TRUE ~ "unknown"
  )
}

# Split "Name N" into base name and dose; tokens that are not doses come
# back with dose 1.
split_dose <- function(token, known) {
  type <- classify_token(token, known)
  m <- str_match(token, "^(.+) (\\d{1,2})$")
  tibble(
    token = token,
    type  = type,
    base  = if_else(type == "dose", m[, 2], token),
    dose  = if_else(type == "dose", as.integer(m[, 3]), 1L)
  )
}

# One row per individual, with PI number, synonyms, comments, and flags
# for how its parentage is recorded.
build_lines_table <- function(parentage, synonyms, comments) {
  pi <- synonyms |>
    filter(str_detect(synonym, "^PI ?\\d+")) |>
    mutate(pi_number = str_remove_all(synonym, " ")) |>
    group_by(individual) |>
    summarise(pi_number = first(pi_number), .groups = "drop")

  # Some lines are themselves named "PI 548585" etc.
  own_pi <- parentage |>
    filter(str_detect(individual, "^PI ?\\d+[A-Z]?$")) |>
    transmute(individual, own_pi = str_remove_all(individual, " "))

  syn <- synonyms |>
    group_by(individual) |>
    summarise(synonyms = paste(synonym, collapse = "; "), .groups = "drop")

  com <- comments |>
    group_by(individual) |>
    summarise(comments = paste(comment, collapse = " | "), .groups = "drop")

  parentage |>
    mutate(
      has_parents   = !is.na(female) | !is.na(male),
      nested_cross  = str_detect(coalesce(female, ""), "\\(") |
                      str_detect(coalesce(male, ""), "\\(")
    ) |>
    left_join(pi, by = "individual") |>
    left_join(own_pi, by = "individual") |>
    mutate(pi_number = coalesce(pi_number, own_pi)) |>
    select(-own_pi) |>
    left_join(syn, by = "individual") |>
    left_join(com, by = "individual")
}

# Counts of each kind of notation in the parent fields, for checking a
# new data release before trusting it.
profile_notation <- function(parentage) {
  known <- parentage$individual
  fields <- c(parentage$female, parentage$male)
  toks <- parent_tokens(fields)
  types <- classify_token(toks, known)

  list(
    individuals        = nrow(parentage),
    with_parents       = sum(!is.na(parentage$female) | !is.na(parentage$male)),
    nested_parent_fields = sum(str_detect(fields, "\\("), na.rm = TRUE),
    slash_parent_fields  = sum(str_detect(fields, "/"), na.rm = TRUE),
    token_types        = table(types),
    ambiguous_tokens   = sort(unique(toks[types == "ambiguous"])),
    expanded_bc_rows   = sum(str_detect(parentage$individual, "BC\\d+$"))
  )
}
