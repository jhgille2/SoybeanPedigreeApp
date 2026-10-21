for (f in list.files(test_path("..", "..", "R"), pattern = "\\.R$", full.names = TRUE)) source(f)

toy <- tibble::tribble(
  ~individual,   ~female,                   ~male,
  "Williams",    NA,                        NA,
  "Kingwa",      NA,                        NA,
  "Clark",       NA,                        NA,
  "L49-4091",    NA,                        NA,
  "Blackhawk",   NA,                        NA,
  "A",           NA,                        NA,
  "B",           NA,                        NA,
  "C",           NA,                        NA,
  "D",           NA,                        NA,
  "Williams 82", "Williams 7",              "Kingwa",
  "Clark 63",    "( Clark 5 , L49-4091 )",  "( Clark 6 , Blackhawk )",
  "Three-way",   "( A , B )",               "C",
  "Four-way",    "( A , B )",               "( C , D )",
  "Top",         "( ( A , B ) , C )",       "C",
  "Kid",         "Three-way",               "Williams 82",
  "Half-known",  "A",                       NA
)

ped <- order_pedigree(expand_pedigree(toy))
g <- pedigree_graph(ped)
info <- tibble::tibble(id = ped$id[ped$named], female = NA_character_, male = NA_character_,
                       pi_number = NA_character_, synonyms = NA_character_, comments = NA_character_)

test_that("Purdy strings follow cross order and recurrent-parent doses", {
  expect_equal(purdy_string(ped, "Williams 82"), "Williams*7/Kingwa")
  expect_equal(purdy_string(ped, "Three-way"), "A/B//C")
  expect_equal(purdy_string(ped, "Four-way"), "A/B//C/D")
  expect_equal(purdy_string(ped, "Top"), "A/B//2*C")
  expect_equal(purdy_string(ped, "Clark 63"), "Clark*5/L49-4091//Clark*6/Blackhawk")
  expect_equal(purdy_string(ped, "Half-known"), "A/unknown")
  expect_true(is.na(purdy_string(ped, "Williams")))
})

test_that("kinship_with matches the full kinship matrix", {
  for (a in c("Williams", "Williams 82", "Three-way", "Kid")) {
    k <- kinship_with(ped, a, g)
    km <- kinship_matrix(ped, ped$id[ped$named], g)
    expect_equal(unname(k[colnames(km)]), unname(km[a, ]), tolerance = 1e-12)
  }
})

test_that("descendants lists every named line with the line in its pedigree", {
  d <- descendants(ped, "Williams", g)
  expect_setequal(d$line, c("Williams 82", "Kid"))
  expect_equal(d$cop[d$line == "Williams 82"], round(1 - 0.5^7, 4))
  expect_equal(nrow(descendants(ped, "Kid", g)), 0)
})

test_that("the collapsed graph links named lines directly", {
  sub <- ancestors(ped, "Kid")
  net <- pedigree_network(sub, "Kid", info, collapse = TRUE)
  expect_false(any(grepl("\\{", net$nodes$id)))
  expect_setequal(net$edges$from[net$edges$to == "Williams 82"], c("Williams", "Kingwa"))
  expect_equal(net$nodes$group[net$nodes$id == "Kid"], "focal")
})

test_that("the full graph shows every intermediate cross", {
  sub <- ancestors(ped, "Williams 82")
  net <- pedigree_network(sub, "Williams 82", info, collapse = FALSE)
  crosses <- net$nodes[net$nodes$group == "cross", ]
  expect_setequal(crosses$label, c("F1", paste0("BC", 1:6)))
  expect_true(all(net$edges$from %in% net$nodes$id))
  expect_equal(net$edges$role[net$edges$to == "Williams 82"], "self")
  expect_equal(sum(net$edges$from == "Williams"), 7)
})

test_that("the generations limit counts named lines only", {
  sub <- ancestors(ped, "Kid", generations = 1)
  expect_setequal(sub$id[sub$named], c("Kid", "Three-way", "Williams 82"))
})

test_that("cross table gives COP with the focal line", {
  tab <- cross_table(ancestors(ped, "Williams 82"), ped, info, "Williams 82", g)
  expect_equal(tab$line[1], "Williams 82")
  expect_equal(tab$cop_with_focal[tab$line == "Williams"], round(1 - 0.5^7, 4))
  expect_match(pedigree_text(tab), "Williams 82 = Williams\\*7/Kingwa")
})

test_that("pasted names match by name or synonym", {
  inf <- tibble::tibble(id = c("Williams 82", "Essex"), synonyms = c("PI 518671; Winchester", NA))
  m <- match_lines(c("Essex", "PI518671", "Nope"), inf)
  expect_equal(m$ids, c("Essex", "Williams 82"))
  expect_equal(m$missing, "Nope")
})
