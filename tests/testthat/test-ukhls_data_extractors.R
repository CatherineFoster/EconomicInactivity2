test_that("regexified variables are as expected", {
  # No variables - expect error
  expect_error(
    convert_varname_selection_to_regex()
  )

  # One additional variable
  expect_equal(
    convert_varname_selection_to_regex("sex"),
    "pidp|^[a-z]{1}_sex$"
  )
  # multiple additional variables
  expect_equal(
    convert_varname_selection_to_regex(c("jbstat", "sex", "dvage")),
    "pidp|^[a-z]{1}_jbstat$|^[a-z]{1}_sex$|^[a-z]{1}_dvage$"
  )

})


test_that("read_and_slim_data works as expected for ind level data", {

  base_dir_location <- "big_data/UKDA-6614-stata/stata/stata13_se/ukhls"

  indresp_files <- dir(
    here::here(base_dir_location),
    pattern = "[a-z]_indresp.dta",
    full.names = TRUE
  )

  # No filename - expect error
  expect_error(
    read_and_slim_data()
  )

  # No variable name - expect error (downstream)
  expect_error(
    read_and_slim_data(indresp_files[1])
  )

  # One valid variable and label type
  df <- read_and_slim_data(indresp_files[1], "sex", "labels")
  print(head(df))
  expect_equal(
    ncol(df),
    4
  )

  # Two variables and label types: as the data are in long format this should
  # still be four columns

  df <- read_and_slim_data(indresp_files[1], c("sex", "dvage"), c("labels", "values"))
  print(head(df))
  expect_equal(
    ncol(df),
    4
  )

})
