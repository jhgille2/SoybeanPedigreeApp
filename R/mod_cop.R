##################################################
## Coefficient of parentage module: COP among a set of lines
##################################################

mod_cop_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      selectizeInput(ns("lines"), "Lines", choices = NULL, multiple = TRUE,
                     options = list(placeholder = "Search and add lines")),
      textAreaInput(ns("pasted"), "Or paste names", rows = 6,
                    placeholder = "One per line, or separated by commas"),
      fileInput(ns("upload"), "Or upload a file of names",
                accept = c(".csv", ".txt", ".tsv")),
      helpText("The first column of the file is used."),
      downloadButton(ns("download"), "Download COP matrix", class = "btn-sm")
    ),
    uiOutput(ns("unmatched")),
    card(
      full_screen = TRUE,
      card_header("Coefficient of parentage"),
      DTOutput(ns("matrix"))
    ),
    card(
      card_header("How COP is calculated"),
      tags$p("COP is the probability that alleles at a locus drawn from two lines are identical by descent.",
             "It is calculated from the pedigree with every intermediate cross written out,",
             "so backcrosses and multi-parent crosses are weighted correctly",
             "(COP of Williams 82 with Williams is 1 − (1/2)^7 = 0.992)."),
      tags$p("Lines with no recorded parents are treated as unrelated, fully inbred founders.",
             "A missing parent is treated as an unknown, unrelated line.",
             "Named lines are assumed fully inbred.")
    )
  )
}

mod_cop_server <- function(id, d, max_lines = 300) {
  moduleServer(id, function(input, output, session) {
    updateSelectizeInput(session, "lines", choices = d$choices, server = TRUE,
                         selected = c("Williams 82", "Essex", "Lee", "NC-Roy"),
                         options = list(searchField = c("label", "synonyms")))

    requested <- reactive({
      pasted <- str_split(input$pasted %||% "", "[\n,;\t]")[[1]]
      uploaded <- if (!is.null(input$upload)) read_name_file(input$upload$datapath) else character()
      x <- str_squish(c(input$lines, pasted, uploaded))
      unique(x[x != ""])
    })

    matched <- reactive(match_lines(requested(), d$lines))

    output$unmatched <- renderUI({
      m <- matched()
      msgs <- list(
        if (length(m$missing))
          tags$p(class = "text-warning", "Not found: ", paste(m$missing, collapse = ", ")),
        if (length(m$ids) > max_lines)
          tags$p(class = "text-warning", sprintf("Showing the first %d lines.", max_lines))
      )
      if (length(compact(msgs))) div(msgs)
    })

    cop <- reactive({
      ids <- head(matched()$ids, max_lines)
      validate(need(length(ids) >= 2, "Choose at least two lines."))
      round(kinship_matrix(d$ped, ids, d$graph), 4)
    })

    output$matrix <- renderDT({
      m <- cop()
      df <- data.frame(line = rownames(m), m, check.names = FALSE)
      datatable(df, rownames = FALSE, selection = "none",
                options = list(pageLength = 25, scrollX = TRUE)) |>
        formatStyle(colnames(m),
                    background = styleInterval(c(0.05, 0.1, 0.2, 0.3, 0.5, 0.75),
                                               c("#ffffff", "#f3f6fa", "#e1eaf4", "#c7d8eb",
                                                 "#a6c2df", "#7ea6cf", "#5a89bd")))
    })

    output$download <- downloadHandler(
      filename = function() "cop_matrix.csv",
      content  = function(file) {
        m <- cop()
        readr::write_csv(data.frame(line = rownames(m), m, check.names = FALSE), file)
      }
    )
  })
}

# Match typed names to lines, by name first and then by synonym.
match_lines <- function(x, info) {
  ids <- info$id[match(x, info$id)]
  miss <- is.na(ids)
  if (any(miss)) {
    syn <- info |>
      filter(!is.na(synonyms)) |>
      transmute(id, synonym = str_split(synonyms, "; ")) |>
      tidyr::unnest(synonym)
    norm <- function(s) str_to_lower(str_remove_all(s, "[ -]"))
    ids[miss] <- syn$id[match(norm(x[miss]), norm(syn$synonym))]
  }
  list(ids = unique(ids[!is.na(ids)]), missing = x[is.na(ids)])
}

read_name_file <- function(path) {
  x <- readr::read_lines(path)
  x <- str_remove(x, "[,\t].*$")
  x <- str_remove_all(x, '^"|"$')
  # Drop a header row that is not a line name.
  if (length(x) && str_to_lower(x[1]) %in% c("line", "name", "individual", "cultivar")) x <- x[-1]
  x
}

`%||%` <- function(a, b) if (is.null(a)) b else a
