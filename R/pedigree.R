##################################################
## Unwind SoyBase pedigree notation into an explicit pedigree
##
## Every cross in a pedigree string becomes its own individual, so that
## relationships can be calculated with ordinary pedigree rules:
##
##   "( A , B )"     an unnamed F1 of A (female) x B (male)
##   "R n x D"       a backcross program with R as the recurrent parent,
##                   used n times: F1 = R x D, BC1 = R x F1, ...,
##                   BC(n-1) = R x BC(n-2)
##   named line      an inbred line derived by selfing from the last
##                   cross in its pedigree ("self" node)
##
## Node types in the expanded pedigree:
##   founder  a named line with no recorded parents, assumed inbred
##   cross    a single plant from crossing parent1 (female) x parent2
##   self     derived from parent1 by `self_gens` generations of selfing
##            (Inf = fully inbred line, the usual case for a cultivar)
##   unknown  a missing parent; unrelated to everything else
##################################################

library(dplyr)
library(stringr)
library(purrr)

# ---- Parsing ---------------------------------------------------------

# Parse one parent field into a nested list:
#   list(type = "name", name = "Essex")
#   list(type = "cross", female = <node>, male = <node>)
#
# About 65 source strings are malformed (unbalanced brackets, truncated
# text, or three parents in one bracket). With `strict = FALSE` these
# are repaired: missing closing brackets are added, stray ones dropped,
# and "( A , B , C )" is read left to right as "( ( A , B ) , C )", the
# usual Purdy reading of A/B//C. Repaired results carry
# attr(, "repaired") = TRUE.
parse_pedigree_string <- function(x, strict = FALSE) {
  if (is.na(x) || x == "") return(list(type = "unknown"))

  chars <- strsplit(x, "")[[1]]
  pos <- 1
  repaired <- FALSE

  problem <- function(msg) {
    if (strict) stop(msg, " at position ", pos, " in: ", x, call. = FALSE)
    repaired <<- TRUE
  }
  skip_space <- function() {
    while (pos <= length(chars) && chars[pos] == " ") pos <<- pos + 1
  }

  parse_node <- function() {
    skip_space()
    if (pos <= length(chars) && chars[pos] == "(") {
      pos <<- pos + 1
      kids <- list(parse_node())
      repeat {
        skip_space()
        if (pos > length(chars)) { problem("Expected ')'"); break }
        if (chars[pos] == ")") { pos <<- pos + 1; break }
        if (chars[pos] == ",") pos <<- pos + 1 else problem("Expected ','")
        kids[[length(kids) + 1]] <- parse_node()
      }
      if (length(kids) == 1) { problem("Expected ','"); return(kids[[1]]) }
      if (length(kids) > 2) problem("Expected ')'")
      reduce(kids, function(f, m) list(type = "cross", female = f, male = m))
    } else {
      start <- pos
      while (pos <= length(chars) && !chars[pos] %in% c(",", ")", "(")) pos <<- pos + 1
      name <- if (pos > start) str_squish(paste(chars[start:(pos - 1)], collapse = "")) else ""
      if (name == "" || name == "-") list(type = "unknown") else list(type = "name", name = name)
    }
  }

  node <- parse_node()
  skip_space()
  if (pos <= length(chars)) problem("Unexpected text")
  if (repaired) attr(node, "repaired") <- TRUE
  node
}

# ---- Expansion -------------------------------------------------------

# Expand the whole parentage table into an explicit pedigree.
# Returns a tibble with one row per individual (named or intermediate):
#   id, label, type, parent1, parent2, self_gens, named, source_line
expand_pedigree <- function(parentage, self_gens = Inf) {
  # A few lines appear twice, once without parents; keep the row that
  # has parentage.
  parentage <- parentage |>
    arrange(is.na(female) & is.na(male)) |>
    distinct(individual, .keep_all = TRUE)
  known <- parentage$individual
  rows <- list()
  n_rows <- 0
  add <- function(...) {
    n_rows <<- n_rows + 1
    rows[[n_rows]] <<- tibble(...)
  }
  unknown_count <- 0

  # Return the id of the node for a parsed parent, adding any
  # intermediate crosses it implies. `owner` names the line whose
  # pedigree this is, so intermediate ids are unique and traceable.
  node_id <- function(node, owner, path) {
    if (node$type == "unknown") {
      unknown_count <<- unknown_count + 1
      id <- paste0("?", unknown_count)
      add(id = id, label = "unknown", type = "unknown", parent1 = NA_character_,
          parent2 = NA_character_, self_gens = NA_real_, named = FALSE,
          source_line = owner)
      return(id)
    }
    if (node$type == "name") {
      return(split_dose(node$name, known)$base)
    }
    cross_id(node$female, node$male, owner, path)
  }

  # A cross between two parsed parents. Handles a recurrent-parent dose
  # on either side by building the backcross chain.
  cross_id <- function(female, male, owner, path) {
    f_dose <- dose_of(female)
    m_dose <- dose_of(male)

    if (f_dose$dose > 1 || m_dose$dose > 1) {
      # The side with the larger dose is the recurrent parent.
      recurrent_is_female <- f_dose$dose >= m_dose$dose
      rec  <- if (recurrent_is_female) f_dose else m_dose
      don  <- if (recurrent_is_female) male else female
      rec_id <- rec$base
      don_id <- node_id(strip_dose(don), owner, paste0(path, "d"))

      prev <- add_cross(rec_id, don_id, recurrent_is_female, owner,
                        paste0(path, "F1"), "F1")
      for (k in seq_len(rec$dose - 1)) {
        prev <- add_cross(rec_id, prev, recurrent_is_female, owner,
                          paste0(path, "BC", k), paste0("BC", k))
      }
      return(prev)
    }

    f_id <- node_id(female, owner, paste0(path, "f"))
    m_id <- node_id(male, owner, paste0(path, "m"))
    add_cross(f_id, m_id, TRUE, owner, paste0(path, "x"), "F1")
  }

  add_cross <- function(a, b, a_is_female, owner, path, stage) {
    p1 <- if (a_is_female) a else b
    p2 <- if (a_is_female) b else a
    id <- paste0(owner, " {", path, "}")
    add(id = id, label = paste0(short_label(p1), " x ", short_label(p2), " ", stage),
        type = "cross", parent1 = p1, parent2 = p2, self_gens = NA_real_,
        named = FALSE, source_line = owner)
    id
  }

  dose_of <- function(node) {
    if (node$type != "name") return(list(base = NA_character_, dose = 1L))
    d <- split_dose(node$name, known)
    list(base = d$base, dose = d$dose)
  }
  strip_dose <- function(node) {
    if (node$type == "name") node$name <- split_dose(node$name, known)$base
    node
  }

  for (i in seq_len(nrow(parentage))) {
    ind <- parentage$individual[[i]]
    f <- parentage$female[[i]]
    m <- parentage$male[[i]]

    if (is.na(f) && is.na(m)) {
      add(id = ind, label = ind, type = "founder", parent1 = NA_character_,
          parent2 = NA_character_, self_gens = NA_real_, named = TRUE,
          source_line = ind)
      next
    }

    parsed <- tryCatch(
      list(parse_pedigree_string(f), parse_pedigree_string(m)),
      error = function(e) { warning(conditionMessage(e)); NULL }
    )
    if (is.null(parsed)) {
      add(id = ind, label = ind, type = "founder", parent1 = NA_character_,
          parent2 = NA_character_, self_gens = NA_real_, named = TRUE,
          source_line = ind)
      next
    }

    last_cross <- cross_id(parsed[[1]], parsed[[2]], ind, "")
    add(id = ind, label = ind, type = "self", parent1 = last_cross,
        parent2 = NA_character_, self_gens = self_gens, named = TRUE,
        source_line = ind,
        repaired = any(map_lgl(parsed, ~ isTRUE(attr(.x, "repaired")))))
  }

  ped <- bind_rows(rows) |> mutate(repaired = coalesce(repaired, FALSE))

  # Parents named in pedigree strings that have no row of their own
  # become founders.
  referenced <- unique(c(ped$parent1, ped$parent2))
  missing <- setdiff(referenced[!is.na(referenced)], ped$id)
  if (length(missing)) {
    ped <- bind_rows(ped, tibble(
      id = missing, label = missing, type = "founder", parent1 = NA_character_,
      parent2 = NA_character_, self_gens = NA_real_, named = TRUE,
      source_line = missing, repaired = FALSE
    ))
  }
  ped
}

short_label <- function(id) str_remove(id, " \\{.*\\}$")

# ---- Ordering --------------------------------------------------------

# Order the pedigree so parents come before offspring, and drop any
# individuals caught in a cycle (bad source data) with a warning.
order_pedigree <- function(ped) {
  g <- igraph::graph_from_data_frame(
    bind_rows(
      tibble(from = ped$parent1, to = ped$id),
      tibble(from = ped$parent2, to = ped$id)
    ) |> filter(!is.na(from)),
    vertices = ped["id"]
  )
  if (!igraph::is_dag(g)) {
    comp <- igraph::components(g, mode = "strong")
    in_cycle <- comp$csize[comp$membership] > 1
    self_loop <- igraph::V(g)$name %in%
      igraph::ends(g, igraph::E(g)[igraph::which_loop(g)])[, 1]
    cyc <- igraph::V(g)$name[in_cycle | self_loop]
    warning(length(cyc), " individuals are in pedigree cycles and were made founders: ",
            paste(head(cyc, 10), collapse = ", "))
    ped <- ped |>
      mutate(
        cyclic  = id %in% cyc,
        type    = if_else(cyclic, "founder", type),
        parent1 = if_else(cyclic, NA_character_, parent1),
        parent2 = if_else(cyclic, NA_character_, parent2)
      ) |>
      select(-cyclic)
    return(order_pedigree(ped))
  }
  ord <- names(igraph::topo_sort(g, mode = "out"))
  ped[match(ord, ped$id), ]
}

# ---- Relationships ---------------------------------------------------

# Kinship f(a, b) is the probability that alleles drawn at random from
# a and b are identical by descent. For two inbred lines it equals the
# coefficient of parentage (COP).
#
# Named founders are assumed fully inbred and unrelated to each other,
# the standard assumption behind soybean COP estimates.
#
# kinship_matrix() uses the tabular method over the ancestors of `ids`
# only, filling one row at a time in pedigree order, so it needs no
# recursion and handles pedigrees of any depth.
kinship_matrix <- function(ped, ids, graph = pedigree_graph(ped)) {
  anc <- unique(unlist(lapply(
    igraph::ego(graph, order = igraph::vcount(graph), nodes = ids, mode = "in"),
    names
  )))
  sub <- ped[ped$id %in% anc, ]            # ped is in pedigree order
  n <- nrow(sub)
  idx <- setNames(seq_len(n), sub$id)
  p1 <- unname(idx[sub$parent1])
  p2 <- unname(idx[sub$parent2])
  K <- matrix(0, n, n, dimnames = list(sub$id, sub$id))

  for (i in seq_len(n)) {
    prev <- seq_len(i - 1)
    switch(sub$type[i],
      founder = { K[i, i] <- 1 },
      unknown = { K[i, i] <- 0.5 },
      cross = {
        row <- (K[p1[i], prev] + K[p2[i], prev]) / 2
        K[i, prev] <- row; K[prev, i] <- row
        K[i, i] <- (1 + K[p1[i], p2[i]]) / 2
      },
      self = {
        row <- K[p1[i], prev]
        K[i, prev] <- row; K[prev, i] <- row
        f_parent <- 2 * K[p1[i], p1[i]] - 1
        g <- sub$self_gens[i]
        f_self <- if (is.infinite(g)) 1 else 1 - (1 - f_parent) / 2^g
        K[i, i] <- (1 + f_self) / 2
      }
    )
  }
  K[ids, ids, drop = FALSE]
}

pedigree_graph <- function(ped) {
  edges <- bind_rows(
    tibble(from = ped$parent1, to = ped$id),
    tibble(from = ped$parent2, to = ped$id)
  ) |> filter(!is.na(from))
  igraph::graph_from_data_frame(edges, vertices = ped["id"])
}

# A function f(x, y) returning kinship for paired vectors of ids.
kinship_calculator <- function(ped) {
  g <- pedigree_graph(ped)
  function(x, y) {
    bad <- setdiff(c(x, y), ped$id)
    if (length(bad)) stop("Unknown individual: ", paste(bad, collapse = ", "))
    K <- kinship_matrix(ped, unique(c(x, y)), g)
    K[cbind(x, y)]
  }
}

# Coefficient of parentage matrix for a set of named lines.
cop_matrix <- function(ped, lines) kinship_matrix(ped, lines)

# ---- Subsetting for display -----------------------------------------

# All ancestors of `id` (including itself), optionally limited to
# `generations` named-line generations back.
ancestors <- function(ped, id, generations = Inf, collapse = FALSE) {
  p <- split(c(ped$parent1, ped$parent2), c(ped$id, ped$id))
  named <- setNames(ped$named, ped$id)
  keep <- character()
  frontier <- tibble(id = id, depth = 0)
  while (nrow(frontier)) {
    keep <- union(keep, frontier$id)
    nxt <- map2(frontier$id, frontier$depth, function(i, d) {
      par <- p[[i]]
      par <- par[!is.na(par) & !par %in% keep]
      tibble(id = par, depth = d + as.integer(named[par]))
    }) |> bind_rows(tibble(id = character(), depth = numeric()))
    frontier <- distinct(filter(nxt, depth <= generations), id, .keep_all = TRUE)
  }
  sub <- ped[ped$id %in% keep, ]
  if (collapse) collapse_intermediates(sub) else sub
}

# Replace each named line's chain of intermediate crosses with direct
# links to the named lines behind it, for a simpler graph.
collapse_intermediates <- function(sub) {
  par <- split(c(sub$parent1, sub$parent2), c(sub$id, sub$id))
  named <- setNames(sub$named, sub$id)
  named_parents <- function(i, seen = character()) {
    out <- character()
    for (q in par[[i]]) {
      if (is.na(q) || q %in% seen || !q %in% names(named)) next
      out <- c(out, if (named[[q]]) q else named_parents(q, c(seen, q)))
    }
    unique(out)
  }
  sub |>
    filter(named) |>
    mutate(named_parents = map(id, named_parents)) |>
    select(id, label, type, named_parents)
}
