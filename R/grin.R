##################################################
## Tidy a GRIN-Global soybean evaluation export
##
## GRIN exports one row per accession with one column per descriptor.
## Repeated observations are joined with ";". Numeric descriptors are
## averaged; categorical ones keep their distinct values.
##################################################

library(dplyr)
library(stringr)
library(purrr)

# Mean of a ";"-separated string of numbers, or NA if none parse.
average_observations <- function(x) {
  map_dbl(x, function(v) {
    if (is.na(v)) return(NA_real_)
    nums <- suppressWarnings(as.numeric(str_split(v, ";")[[1]]))
    if (all(is.na(nums))) NA_real_ else mean(nums, na.rm = TRUE)
  })
}

# A column is numeric if nearly all its non-missing observations parse
# as numbers.
is_numeric_descriptor <- function(x, threshold = 0.95) {
  obs <- unlist(str_split(x[!is.na(x)], ";"))
  if (length(obs) == 0) return(FALSE)
  mean(!is.na(suppressWarnings(as.numeric(obs)))) >= threshold
}

read_grin_export <- function(path) {
  raw <- readr::read_tsv(path, col_types = readr::cols(.default = "c"),
                         progress = FALSE) |>
    janitor_names()

  raw <- raw |>
    select(-any_of(c("grin_link")), -starts_with("x")) |>
    rename(pi_number = grin_accession) |>
    mutate(pi_number = str_remove_all(pi_number, " "))

  numeric_cols <- names(raw)[map_lgl(raw, is_numeric_descriptor)]
  numeric_cols <- setdiff(numeric_cols, "pi_number")

  raw |>
    mutate(across(all_of(numeric_cols), average_observations),
           across(-all_of(c("pi_number", numeric_cols)),
                  ~ map_chr(str_split(.x, ";"),
                            function(v) paste(unique(v), collapse = ";"))),
           across(-all_of(c("pi_number", numeric_cols)),
                  ~ na_if(.x, "NA")))
}

# snake_case column names without needing the janitor package.
janitor_names <- function(df) {
  nm <- names(df) |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_remove_all("^_|_$")
  nm[nm == ""] <- paste0("x", seq_along(nm))[nm == ""]
  names(df) <- make.unique(nm, sep = "_")
  df
}
