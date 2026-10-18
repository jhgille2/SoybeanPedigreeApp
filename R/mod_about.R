##################################################
## About module: where the data comes from and how it is read
##################################################

mod_about_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(6, 6),
    card(
      card_header("Data"),
      tags$p("Pedigrees, synonyms and notes come from SoyBase's parentage data",
             tags$a("(soybase/parentage)", href = "https://github.com/soybase/parentage", target = "_blank"),
             ". Trait values come from a 2021 GRIN-Global evaluation export, joined to lines by PI number."),
      tags$pre(textOutput(ns("source"))),
      tableOutput(ns("counts"))
    ),
    card(
      card_header("Reading SoyBase pedigrees"),
      tags$ul(
        tags$li("Each cross in a pedigree string becomes its own (unnamed) individual, shown as a small grey dot when intermediate crosses are shown."),
        tags$li("\"Williams 7\" means Williams used seven times as the recurrent parent (an F1 and six backcrosses), written Williams*7 here."),
        tags$li("A named line is taken to be a fully inbred selection from the last cross in its pedigree."),
        tags$li("Lines with one recorded parent are treated as crosses with an unknown parent."),
        tags$li("About 120 malformed pedigree strings were repaired automatically; those lines are flagged on the Pedigree page.")
      )
    )
  )
}

mod_about_server <- function(id, d) {
  moduleServer(id, function(input, output, session) {
    output$source <- renderText(d$source)
    output$counts <- renderTable({
      tibble(
        `Named lines`                = sum(d$ped$named),
        `Lines with parents`         = sum(d$ped$type == "self"),
        `Intermediate crosses`       = sum(d$ped$type == "cross"),
        `Repaired pedigree strings`  = sum(d$ped$repaired),
        `Lines with GRIN traits`     = nrow(d$traits)
      ) |> tidyr::pivot_longer(everything(), names_to = "Item", values_to = "Count")
    }, digits = 0)
  })
}
