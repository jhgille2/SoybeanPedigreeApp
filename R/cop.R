##################################################
## Kinship of one line with every individual
##
## kinship_matrix() needs the joint ancestry of every line it compares,
## which gets slow for thousands of lines (all descendants of Williams
## 82, say). For one line `a` against everything, a single pass in
## pedigree order is enough:
##   - for `a` and its ancestors, use kinship_matrix() on that small set
##   - for anyone else x, f(a, x) is the mean of f(a, parent) over x's
##     parents for a cross, f(a, parent1) for a selfed line, and 0 for
##     a founder or unknown parent
##################################################

kinship_with <- function(ped, a, graph = pedigree_graph(ped)) {
  anc <- names(igraph::subcomponent(graph, a, mode = "in"))
  K <- kinship_matrix(ped, anc, graph)

  ord <- order_index(ped)
  f <- setNames(numeric(nrow(ped)), ped$id)
  f[anc] <- K[a, anc]
  done <- ped$id %in% anc
  p1 <- match(ped$parent1, ped$id)
  p2 <- match(ped$parent2, ped$id)
  type <- ped$type

  for (i in ord) {
    if (done[i]) next
    f[i] <- switch(type[i],
      cross = (f[p1[i]] + f[p2[i]]) / 2,
      self  = f[p1[i]],
      0
    )
  }
  f
}

# Row order with parents before offspring. The saved pedigree is
# already in this order; check rather than assume.
order_index <- function(ped) {
  pos <- seq_len(nrow(ped))
  p1 <- match(ped$parent1, ped$id)
  p2 <- match(ped$parent2, ped$id)
  ok <- (is.na(p1) | p1 < pos) & (is.na(p2) | p2 < pos)
  if (all(ok)) return(pos)
  match(order_pedigree(ped)$id, ped$id)
}
