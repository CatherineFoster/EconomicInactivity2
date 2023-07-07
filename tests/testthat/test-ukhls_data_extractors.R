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

  # # One valid variable and label type
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


test_that("read_and_slim_data will not fail if not all variables requested are found", {
  base_dir_location <- "big_data/UKDA-6614-stata/stata/stata13_se/ukhls"

  indresp_files <- dir(
    here::here(base_dir_location),
    pattern = "[a-z]_indresp.dta",
    full.names = TRUE
  )

  df <- read_and_slim_data(indresp_files[1], c("sex", "dvage", "cheese_consumption"), c("labels", "values", "labels"))

  expect_s3_class(df, "data.frame")

  unique_vars <- df$variable %>% unique() %>% length()

  expect_equal(2, unique_vars)
})

test_that("extract_eq_income_and_num_dependents works when correct file is passed", {

  base_dir_location <- "big_data/UKDA-6614-stata/stata/stata13_se/ukhls"

  hhresp_files <- dir(
    here::here(base_dir_location),
    pattern = "[a-z]{1}_hhresp.dta",
    full.names = TRUE
  )

  df_a <- extract_eq_income_and_num_dependents(hhresp_files[1])

  expect_s3_class(df_a, "data.frame")
})

test_that("extract_pid_with_hid_and_wave works as expected", {
  base_dir_location <- here::here("big_data/UKDA-6614-stata/stata/stata13_se/ukhls/")
  ind_files_only <- dir(
    base_dir_location,
    pattern = "^[a-z]{1}_indresp.dta",
    full.names = TRUE
  )

  df <- extract_pid_with_hid_and_wave(ind_files_only[1])

  expect_s3_class(df, "data.frame")

  expect_true(
    c("pidp", "hidp", "wave") %in% names(df) %>% all()
  )
})
