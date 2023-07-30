

big_dir_location <- "big_data/UKDA-6614-stata/stata/stata13_se/ukhls"
indresp_files <- dir(here::here(big_dir_location), pattern = "[a-z]_indresp.dta", full.names = TRUE)
test_dta <- haven::read_dta(indresp_files[1])[1:100,1:5]
test_dta_converted <- return_labels_as_factors(test_dta)

test_that("can see ukhls files in expected location", {
  big_dir_location <- "big_data/UKDA-6614-stata/stata/stata13_se/ukhls"
  indresp_files <- dir(here::here(big_dir_location), pattern = "[a-z]_indresp.dta", full.names = TRUE)
    expect_equal(
      12,
      length(indresp_files)
    )
})

test_that("return_labels_as_factors will fail if not given a dataset", {
  expect_error(
    return_labels_as_factors("Wrong input")
  )
})

test_that("return_labels_as_factors will not convert a column that is not haven_labelled", {
  testthat::expect_true(class(test_dta$pidp) == class(test_dta_converted$pidp))
})


test_that("return_labels_as_factors will convert a column that is haven_labelled", {
  expect_true("haven_labelled" %in% class(test_dta$a_hidp))
  expect_false("haven_labelled" %in% class(test_dta_converted$a_hidp))
})

test_that("can return correct labels for database columns", {
  colLabels <- get_col_labels(test_dta)
  print(colLabels[1:5])

  expect_equal(
    c(
      pidp = "cross-wave person identifier (public release)",
      a_hidp = "household identifier (public release)",
      a_pno = "person number",
      a_hhorig = "Sample origin, household",
      a_memorig = "Sample origin, individual"
    ),
    colLabels[1:5]
  )
})

test_that("can return correct values and labels for a given column", {

  varlabLookup1 <- get_labels_for_col(test_dta$a_pno)

  varlabLookup2 <- get_labels_for_col(test_dta$a_memorig)

  expect_equal(
    c(-9, -8, -7, -2, -1),
    unname(varlabLookup1)
  )

  expect_equal(
    c("missing", "inapplicable", "proxy respondent", "refusal", "don't know"),
    names(varlabLookup1)
  )

  expect_equal(
    c(missing = -9, inapplicable = -8, refusal = -2, `don't know` = -1,
      `ukhls gb 2009-10` = 1, `ukhls ni 2009-10` = 2, `bhps gb 1991` = 3,
      `bhps sco 1999` = 4, `bhps wal 1999` = 5, `bhps ni 2001` = 6,
      `ukhls emboost 2009-10` = 7, `ukhls iemb 2014-15` = 8, `ip original sample 2008` = 11,
      `ip4 refreshment sample 2011` = 12, `ip7 refreshment sample 2014` = 13,
      `ECHP - SCPR` = 14, `ECHP - ONS` = 15, `ECHP - NI` = 16),
    varlabLookup2
  )
})


test_that("standardise_scores works as espected", {

  set.seed(50)
  x <- rnorm(100, 4, 2)

  z <- standardise_scores(x)
  testthat::expect_true(
    all.equal(mean(x), 4, tolerance = 0.1)
  )
  testthat::expect_true(
    all.equal(sd(x), 2, tolerance = 0.1)
  )


  testthat::expect_true(
    all.equal(mean(z), 0, tolerance = 0.1)
  )
  testthat::expect_true(
    all.equal(sd(z), 1, tolerance = 0.1)
  )


})
