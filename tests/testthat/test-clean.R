for (f in list.files(test_path("..", "..", "R"), pattern = "\\.R$", full.names = TRUE)) source(f)

known <- c("Williams", "Williams 82", "Kingwa", "Clark", "Clark 63",
           "Manchu", "Manchu 3")

test_that("recurrent-parent doses are told apart from real names", {
  expect_equal(classify_token("Williams 7", known), "dose")
  expect_equal(classify_token("Williams 82", known), "line")
  expect_equal(classify_token("Clark 63", known), "line")
  expect_equal(classify_token("Kingwa", known), "line")
  expect_equal(classify_token("Manchu 3", known), "ambiguous")
  expect_equal(classify_token("Nobody 4", known), "unknown")
})

test_that("split_dose returns base line and dose", {
  out <- split_dose(c("Williams 7", "Kingwa", "Williams 82"), known)
  expect_equal(out$base, c("Williams", "Kingwa", "Williams 82"))
  expect_equal(out$dose, c(7L, 1L, 1L))
})

test_that("parent_tokens ignores nested-cross punctuation", {
  expect_equal(parent_tokens("( Clark 5 , L49-4091 )"), c("Clark 5", "L49-4091"))
  expect_equal(parent_tokens(c(NA, "Essex")), "Essex")
})

test_that("clean_name normalises missing values and whitespace", {
  expect_equal(clean_name(c(" Lee  68 ", "-", "", "HC85-2206 &nbsp;")),
               c("Lee 68", NA, NA, "HC85-2206"))
})

test_that("GRIN observations are averaged", {
  expect_equal(average_observations(c("1;3", NA, "x")), c(2, NA, NA))
})
