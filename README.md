# Soybean Pedigree App
Shiny app for exploring soybean pedigrees from SoyBase. It combines this app with the scripts in [SoybasePedigreeVisualization](https://github.com/jhgille2/SoybasePedigreeVisualization).

## Running the app

```
Rscript -e 'shiny::runApp()'
```

Packages: shiny, bslib, visNetwork, DT, dplyr, tidyr, purrr, stringr, readr, igraph (and testthat for the tests). Shiny loads everything in `R/` before `app.R`.

- **Pedigree**: search a line by name, PI number or synonym; choose how many generations to show; hover a line for SoyBase notes and links, double-click to re-centre. "Show intermediate crosses" draws every F1 and backcross plant implied by the pedigree; off, named lines link straight to their named parents. Below the graph: a text pedigree in Purdy notation, COP of every line in the view with the selected line, all lines descended from it, and GRIN traits. The table downloads as CSV.
- **Coefficient of parentage**: COP matrix for any set of lines, picked, pasted, or uploaded as a file; downloads as CSV.
- `?line=Williams%2082` in the address opens the app on that line.

The 2021 app is still in `PedigreeNetwork/` for comparison.

## Data pipeline (in progress)

`data-raw/build_data.R` rebuilds the app's data from source:

```
Rscript data-raw/build_data.R          # download the latest SoyBase parentage data
Rscript data-raw/build_data.R offline  # rebuild from the copy in data-raw/soybase/
Rscript -e 'testthat::test_dir("tests/testthat")'
```

- **Pedigrees** come from [soybase/parentage](https://github.com/soybase/parentage), the files behind SoyBase's parentage viewer. Each build records the source commit in `data-raw/soybase/SOURCE.txt`.
- **Traits** come from a GRIN-Global evaluation export, joined to lines by PI number (taken from SoyBase synonyms). `data-raw/grin/grin_results_2021.txt` is the 2021 export and should be replaced with a fresh one.
- `data/notation_profile.txt` summarises the pedigree notation in each release so changes in SoyBase's format are easy to spot.

A recurrent-parent dose is written `Williams 7` in the current SoyBase files (`Williams (7)` in the old table): Williams 82 = Williams 7 × Kingwa, i.e. Williams used seven times (BC6). It is not a selfing generation.

## Unwinding pedigrees

`R/pedigree.R` turns SoyBase's pedigree strings into an explicit pedigree in which every cross is its own individual:

- `( A , B )` becomes an unnamed F1 of A (female) × B (male).
- `Williams 7 × Kingwa` becomes an F1 followed by BC1 to BC6, each crossed back to Williams.
- A named line is an inbred line selfed from the last cross in its pedigree (fully inbred by default; set `self_gens` for partly inbred material).
- Malformed strings (about 120 lines) are repaired and flagged in the `repaired` column.

`kinship_matrix(ped, ids)` gives kinship (equal to COP between inbred lines) using the tabular method over just the ancestors of `ids`. Named founders are treated as fully inbred and unrelated. The tests check hand-worked values, e.g. COP(Williams 82, Williams) = 1 - (1/2)^7.
