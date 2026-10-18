##################################################
## Pedigree module: graph, text pedigree, line details, descendants
##################################################

mod_pedigree_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      selectizeInput(ns("line"), "Line", choices = NULL,
                     options = list(placeholder = "Search by name, PI or synonym")),
      sliderInput(ns("generations"), "Generations", min = 1, max = 20, value = 4),
      input_switch(ns("show_crosses"), "Show intermediate crosses", value = FALSE),
      helpText("Intermediate crosses are the unnamed F1 and backcross plants",
               "implied by the pedigree, e.g. Williams 82 = Williams*7/Kingwa",
               "has an F1 and six backcrosses to Williams."),
      helpText(HTML("With crosses shown, <span style='color:#b5487a'>pink</span> links come from the female parent,",
                    "<span style='color:#3c6e9f'>blue</span> from the male, and dashed links show a line selected from a cross.")),
      helpText("Hover over a line for its SoyBase notes and links.",
               "Double-click a line to re-centre on it."),
      downloadButton(ns("download"), "Download pedigree table", class = "btn-sm")
    ),
    card(
      full_screen = TRUE,
      card_header(textOutput(ns("graph_title"), inline = TRUE)),
      visNetworkOutput(ns("graph"), height = "650px")
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("Text pedigree"),
        verbatimTextOutput(ns("text"))
      ),
      card(
        card_header("About this line"),
        uiOutput(ns("info"))
      )
    ),
    navset_card_tab(
      nav_panel("Lines in this pedigree", DTOutput(ns("crosses"))),
      nav_panel(textOutput(ns("desc_title"), inline = TRUE), DTOutput(ns("descendants"))),
      nav_panel("GRIN traits", uiOutput(ns("traits")))
    )
  )
}

mod_pedigree_server <- function(id, d, default_line = "Williams 82") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    choose_line <- function(x) {
      updateSelectizeInput(session, "line", choices = d$choices, selected = x,
                           server = TRUE,
                           options = list(searchField = c("label", "synonyms")))
    }

    # Start on ?line=... if given, so a view can be shared by link.
    observe({
      q <- parseQueryString(session$clientData$url_search)$line
      choose_line(if (!is.null(q) && q %in% d$lines$id) q else default_line)
    }) |> bindEvent(TRUE, once = TRUE)

    observeEvent(input$recentre, {
      if (isTRUE(d$ped$named[match(input$recentre, d$ped$id)])) choose_line(input$recentre)
    })

    line <- reactive({
      req(input$line %in% d$lines$id)
      input$line
    })

    view <- reactive({
      ancestors(d$ped, line(), input$generations)
    })

    crosses <- reactive({
      cross_table(view(), d$ped, d$lines, line(), d$graph)
    })

    output$graph_title <- renderText({
      n <- sum(view()$named)
      sprintf("Pedigree of %s: %d lines over %d generations", line(), n, input$generations)
    })

    output$graph <- renderVisNetwork({
      net <- pedigree_network(view(), line(), d$lines, collapse = !input$show_crosses)
      pedigree_vis(net, ns("recentre"), compact = input$show_crosses)
    })

    output$text <- renderText(pedigree_text(crosses()))

    output$info <- renderUI({
      i <- match(line(), d$lines$id)
      r <- d$lines[i, ]
      p <- d$ped[match(line(), d$ped$id), ]
      tagList(
        tags$p(tags$b("Pedigree: "), purdy_string(d$ped, line()) %|na|% "no parents recorded (founder)"),
        if (!is.na(r$female) || !is.na(r$male))
          tags$p(tags$b("SoyBase parent fields: "), r$female %|na|% "-", " × ", r$male %|na|% "-"),
        if (isTRUE(p$repaired))
          tags$p(class = "text-warning",
                 "SoyBase's pedigree string for this line is malformed and was repaired; check it against the source."),
        if (!is.na(r$pi_number)) tags$p(tags$b("PI: "), tags$a(r$pi_number, href = grin_url(r$pi_number), target = "_blank")),
        if (!is.na(r$synonyms)) tags$p(tags$b("Also known as: "), r$synonyms),
        if (!is.na(r$comments)) tags$p(tags$b("SoyBase notes: "), r$comments),
        tags$p(tags$a("Open the SoyBase parentage viewer", href = soybase_parentage_url, target = "_blank"))
      )
    })

    output$crosses <- renderDT({
      datatable(
        crosses(),
        colnames = c("Line", "Pedigree", "SoyBase female", "SoyBase male",
                     paste("COP with", line())),
        rownames = FALSE, selection = "none",
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })

    desc <- reactive(descendants(d$ped, line(), d$graph))

    output$desc_title <- renderText(sprintf("Lines descended from %s (%d)", line(), nrow(desc())))

    output$descendants <- renderDT({
      datatable(desc(), colnames = c("Line", paste("COP with", line())),
                rownames = FALSE, selection = "none",
                options = list(pageLength = 10))
    })

    output$traits <- renderUI({
      tr <- d$traits[d$traits$individual == line(), ]
      if (!nrow(tr)) {
        return(tags$p("No GRIN evaluations for this line",
                      "(traits are joined by PI number from the 2021 GRIN export)."))
      }
      tr <- tr[1, !is.na(tr[1, ]) & !names(tr) %in% c("individual", "pi_number")]
      tags$table(
        class = "table table-sm",
        tags$tbody(map2(names(tr), unlist(tr), ~ tags$tr(tags$td(str_replace_all(.x, "_", " ")), tags$td(.y))))
      )
    })

    output$download <- downloadHandler(
      filename = function() paste0("pedigree_", make.names(line()), ".csv"),
      content  = function(file) readr::write_csv(crosses(), file, na = "")
    )
  })
}

# Draw a pedigree_network() result. Double-clicking a node sends its
# id to `recentre_input`.
# `compact` packs levels closer, for the much taller graph with
# intermediate crosses shown.
pedigree_vis <- function(net, recentre_input, compact = FALSE) {
  visNetwork(net$nodes, net$edges) |>
    visGroups(groupname = "focal",   shape = "box", color = list(background = "#f4c95d", border = "#9a7b1c")) |>
    visGroups(groupname = "line",    shape = "box", color = list(background = "#d6e6f5", border = "#3c6e9f")) |>
    visGroups(groupname = "founder", shape = "box", color = list(background = "#e6f2df", border = "#4f7f3a")) |>
    visGroups(groupname = "cross",   shape = "dot", size = 8, color = list(background = "#bbbbbb", border = "#777777"),
              font = list(size = 11, color = "#555555")) |>
    visGroups(groupname = "unknown", shape = "dot", size = 8, color = list(background = "#ffffff", border = "#999999")) |>
    visEdges(arrows = "to", smooth = FALSE) |>
    visHierarchicalLayout(direction = "UD", sortMethod = "directed",
                          levelSeparation = if (compact) 55 else 110,
                          nodeSpacing = if (compact) 110 else 140) |>
    visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = FALSE)) |>
    visInteraction(hover = TRUE, tooltipDelay = 150, navigationButtons = TRUE) |>
    visEvents(doubleClick = sprintf(
      "function(e) { if (e.nodes.length) Shiny.setInputValue('%s', e.nodes[0], {priority: 'event'}); }",
      recentre_input
    ))
}

`%|na|%` <- function(a, b) if (length(a) == 0 || is.na(a)) b else a
