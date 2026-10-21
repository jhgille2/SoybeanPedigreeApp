##################################################
## Turn the expanded pedigree into things the app shows
##
##   pedigree_network()  nodes and edges for the interactive graph
##   purdy_string()      Purdy notation for one named line
##   cross_table()       one row per named line in a view
##   descendants()       named lines with a given line in their pedigree
##################################################

# ---- Labels and links ------------------------------------------------

# Stage of an intermediate cross, from the path in its id:
# "Williams 82 {BC3}" -> "BC3", "Clark 63 {fx}" -> "F1".
cross_stage <- function(id) {
  path  <- str_match(id, "\\{(.*)\\}$")[, 2]
  stage <- str_extract(path, "BC\\d+$")
  if_else(is.na(stage), "F1", stage)
}

# The new SoyBase parentage viewer has no per-line address, so lines
# link to the viewer itself and, where a PI number is known, to GRIN.
soybase_parentage_url <- "https://www.soybase.org/tools/parentage/"

grin_url <- function(pi_number) {
  if_else(is.na(pi_number), NA_character_,
          paste0("https://npgsweb.ars-grin.gov/gringlobal/search?q=",
                 utils::URLencode(str_replace(pi_number, "^PI", "PI "),
                                  reserved = TRUE)))
}

line_tooltip <- function(id, info) {
  i <- match(id, info$id)
  pi <- info$pi_number[i]
  parts <- c(
    paste0("<b>", htmltools::htmlEscape(id), "</b>"),
    if_else(is.na(info$synonyms[i]), NA_character_,
            paste0("Also: ", htmltools::htmlEscape(info$synonyms[i]))),
    if_else(is.na(info$comments[i]), NA_character_,
            paste0("SoyBase note: ", htmltools::htmlEscape(info$comments[i]))),
    paste0("<a href='", soybase_parentage_url, "' target='_blank'>SoyBase parentage viewer</a>"),
    if_else(is.na(pi), NA_character_,
            paste0("<a href='", grin_url(pi), "' target='_blank'>GRIN: ", pi, "</a>"))
  )
  paste(parts[!is.na(parts)], collapse = "<br>")
}

# ---- Graph -----------------------------------------------------------

# Nodes and edges for visNetwork from a subset of the pedigree.
#   sub       rows of the expanded pedigree, from ancestors()
#   focal     the line the view is centred on
#   collapse  TRUE to hide intermediate crosses and link named lines
#             straight to the named lines behind them
pedigree_network <- function(sub, focal, info, collapse = TRUE) {
  if (collapse) {
    col <- collapse_intermediates(sub)
    nodes <- col |> transmute(id, label, kind = if_else(type == "founder", "founder", "line"))
    edges <- col |>
      select(to = id, from = named_parents) |>
      tidyr::unnest(from) |>
      transmute(from, to, role = "parent")
  } else {
    nodes <- sub |>
      transmute(
        id,
        label = case_when(
          named            ~ label,
          type == "unknown" ~ "?",
          TRUE              ~ cross_stage(id)
        ),
        kind = case_when(
          type == "founder" ~ "founder",
          named             ~ "line",
          type == "unknown" ~ "unknown",
          TRUE              ~ "cross"
        )
      )
    edges <- bind_rows(
      sub |> filter(!is.na(parent1)) |>
        transmute(from = parent1, to = id,
                  role = if_else(type == "self", "self", "female")),
      sub |> filter(!is.na(parent2)) |>
        transmute(from = parent2, to = id, role = "male")
    ) |>
      filter(from %in% sub$id)
  }

  named <- nodes$kind %in% c("line", "founder")
  cross_title <- function(ids) {
    r <- sub[match(ids, sub$id), ]
    paste0(cross_stage(ids), ": ", htmltools::htmlEscape(r$parent1), " &times; ",
           htmltools::htmlEscape(r$parent2),
           "<br><i>in the pedigree of ", htmltools::htmlEscape(r$source_line), "</i>")
  }
  nodes <- nodes |>
    mutate(
      title = case_when(
        named              ~ map_chr(id, line_tooltip, info = info),
        kind == "unknown"  ~ "Unknown parent",
        TRUE               ~ cross_title(id)
      ),
      group = if_else(id == focal, "focal", kind),
      level = NULL
    )
  edges <- edges |>
    mutate(
      title  = c(female = "female parent", male = "male parent",
                 self = "selected from this cross", parent = "parent")[role],
      dashes = role == "self",
      color  = unname(edge_colours[role])
    )
  list(nodes = as.data.frame(nodes), edges = as.data.frame(edges))
}

edge_colours <- c(female = "#b5487a", male = "#3c6e9f",
                  self = "#9a9a9a", parent = "#7a7a7a")

# ---- Text pedigree ---------------------------------------------------

# Purdy notation for a named line, with its named parents written as
# names: A/B, A/B//C, A/B/3/C, and R*n/D or D/n*R for n doses of a
# recurrent parent (e.g. Williams 82 = Williams*7/Kingwa).
purdy_string <- function(ped, id) {
  row <- match(id, ped$id)
  if (is.na(row)) stop("Unknown individual: ", id)
  if (ped$type[row] != "self") return(NA_character_)

  idx <- function(x) match(x, ped$id)
  is_cross <- function(x) !is.na(x) && ped$type[idx(x)] == "cross"

  expr <- function(x) {
    i <- idx(x)
    if (ped$type[i] == "unknown") return(list(text = "unknown", level = 0))
    if (ped$type[i] != "cross") return(list(text = ped$id[i], level = 0))

    p1 <- ped$parent1[i]; p2 <- ped$parent2[i]
    # Backcross chain: one named parent used at every step.
    for (side in 1:2) {
      rec <- c(p1, p2)[side]
      if (is_cross(rec)) next
      n <- 0; cur <- x; rec_side <- side
      while (is_cross(cur)) {
        j <- idx(cur)
        pp <- c(ped$parent1[j], ped$parent2[j])
        if (!rec %in% pp || all(pp == rec)) break
        n <- n + 1
        cur <- pp[pp != rec]
      }
      if (n >= 2) {
        don <- expr(cur)
        level <- don$level + 1
        text <- if (rec_side == 1) paste0(rec, "*", n, sep(level), don$text)
                else paste0(don$text, sep(level), n, "*", rec)
        return(list(text = text, level = level))
      }
    }
    f <- expr(p1); m <- expr(p2)
    level <- max(f$level, m$level) + 1
    list(text = paste0(f$text, sep(level), m$text), level = level)
  }
  expr(ped$parent1[row])$text
}

sep <- function(level) {
  if (level == 1) "/" else if (level == 2) "//" else paste0("/", level, "/")
}

# One row per named line in a view: Purdy pedigree, SoyBase's own
# parent fields, and COP with the focal line.
cross_table <- function(sub, ped, info, focal, graph = pedigree_graph(ped)) {
  ids <- sub$id[sub$named]
  cop <- kinship_with(ped, focal, graph)[ids]
  i <- match(ids, info$id)
  tibble(
    line            = ids,
    pedigree        = map_chr(ids, purdy_string, ped = ped),
    soybase_female  = info$female[i],
    soybase_male    = info$male[i],
    cop_with_focal  = round(unname(cop), 4)
  ) |>
    arrange(desc(line == focal), desc(cop_with_focal), line)
}

# Text version of the view, as in the original app:
# "Line = pedigree" for every named line with parents.
pedigree_text <- function(tab) {
  tab <- tab[!is.na(tab$pedigree), ]
  paste0(tab$line, " = ", tab$pedigree, collapse = "\n")
}

# ---- Descendants -----------------------------------------------------

# Named lines that have `id` anywhere in their pedigree, with the COP
# between each and `id`.
descendants <- function(ped, id, graph = pedigree_graph(ped), cop = TRUE) {
  down <- names(igraph::subcomponent(graph, id, mode = "out"))
  down <- setdiff(down[ped$named[match(down, ped$id)]], id)
  out <- tibble(line = down)
  if (cop && length(down)) {
    out$cop <- round(unname(kinship_with(ped, id, graph)[down]), 4)
    out <- arrange(out, desc(cop), line)
  }
  out
}
