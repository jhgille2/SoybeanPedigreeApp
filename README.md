# Soybean Pedigree App
Shiny app for visualizing soybean pedigrees. For data processing, see [this repository](https://github.com/jhgille2/SoybasePedigreeVisualization).

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
