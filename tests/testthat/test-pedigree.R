for (f in list.files(test_path("..", "..", "R"), pattern = "\\.R$", full.names = TRUE)) source(f)

toy <- tibble::tribble(
  ~individual,   ~female,                                   ~male,
  "Williams",    NA,                                        NA,
  "Kingwa",      NA,                                        NA,
  "Clark",       NA,                                        NA,
  "L49-4091",    NA,                                        NA,
  "Blackhawk",   NA,                                        NA,
  "A",           NA,                                        NA,
  "B",           NA,                                        NA,
  "C",           NA,                                        NA,
  "Williams 82", "Williams 7",                              "Kingwa",
  "Clark 63",    "( Clark 5 , L49-4091 )",                  "( Clark 6 , Blackhawk )",
  "Sib1",        "A",                                       "B",
  "Sib2",        "A",                                       "B",
  "Three-way",   "( A , B )",                               "C",
  "Kid",         "Sib1",                                    "C",
  "Half-known",  "A",                                       NA
)

ped <- order_pedigree(expand_pedigree(toy))
kin <- kinship_calculator(ped)

test_that("nested crosses parse into female/male trees", {
  p <- parse_pedigree_string("( ( PI 594445 , Jake ) , Jake)")
  expect_equal(p$type, "cross")
  expect_equal(p$female$female$name, "PI 594445")
  expect_equal(p$male$name, "Jake")
  expect_equal(parse_pedigree_string(NA)$type, "unknown")
  expect_error(parse_pedigree_string("( A , B", strict = TRUE), "Expected")
  # Repairs: missing bracket, three parents, stray bracket
  r <- parse_pedigree_string("( A , B")
  expect_true(attr(r, "repaired"))
  expect_equal(r$male$name, "B")
  r3 <- parse_pedigree_string("( Mack , Wayne , ( Clark , Adams ) )")
  expect_equal(r3$female$female$name, "Mack")
  expect_equal(r3$male$female$name, "Clark")
  expect_true(attr(parse_pedigree_string("( A , B ) )"), "repaired"))
})

test_that("a backcross dose becomes an F1 and n-1 backcrosses", {
  w82 <- ped |> dplyr::filter(source_line == "Williams 82", type == "cross")
  expect_equal(nrow(w82), 7)  # F1 + BC1..BC6
  expect_true(all(w82$parent1 == "Williams"))
  expect_equal(sum(stringr::str_detect(w82$label, "BC6$")), 1)
})

test_that("COP matches hand-worked values", {
  # Named inbred lines are fully inbred
  expect_equal(kin("Williams 82", "Williams 82"), 1)
  # Single cross: half from each parent
  expect_equal(kin("Sib1", "A"), 0.5)
  # Inbred full sibs from the same cross
  expect_equal(kin("Sib1", "Sib2"), 0.5)
  # BC6: 1 - (1/2)^7 from the recurrent parent
  expect_equal(kin("Williams 82", "Williams"), 1 - 0.5^7)
  expect_equal(kin("Williams 82", "Kingwa"), 0.5^7)
  # Three-way cross: C contributes 1/2, A and B 1/4 each
  expect_equal(kin("Three-way", "C"), 0.5)
  expect_equal(kin("Three-way", "A"), 0.25)
  # Grandparent through a named line
  expect_equal(kin("Kid", "A"), 0.25)
  # Clark 63 = (Clark 5 x L49-4091) x (Clark 6 x Blackhawk):
  # (1 - 1/2^5)/2 + (1 - 1/2^6)/2 from Clark
  expect_equal(kin("Clark 63", "Clark"), ((1 - 0.5^5) + (1 - 0.5^6)) / 2)
  # Unknown parent contributes nothing known
  expect_equal(kin("Half-known", "A"), 0.5)
  expect_equal(kin("A", "B"), 0)
})

test_that("intermediate crosses are not inbred, F1 of related parents is", {
  f1 <- ped$id[ped$source_line == "Sib1" & ped$type == "cross"]
  expect_equal(kin(f1, f1), 0.5)  # F1 of unrelated inbreds: F = 0
  bc <- ped$id[ped$source_line == "Williams 82" & stringr::str_detect(ped$label, "BC1$")]
  # BC1 = Williams x F1: F = f(Williams, F1) = 1/2
  expect_equal(kin(bc, bc), (1 + 0.5) / 2)
})

test_that("finite selfing generations give partial inbreeding", {
  ped3 <- order_pedigree(expand_pedigree(toy, self_gens = 3))
  k3 <- kinship_calculator(ped3)
  # F3-derived... S3 from an F1 with F = 0: F = 1 - 1/8
  expect_equal(k3("Sib1", "Sib1"), (1 + (1 - 1 / 8)) / 2)
})

test_that("collapsed view links named lines directly", {
  sub <- ancestors(ped, "Clark 63", collapse = TRUE)
  np <- sub$named_parents[[which(sub$id == "Clark 63")]]
  expect_setequal(np, c("Clark", "L49-4091", "Blackhawk"))
})
