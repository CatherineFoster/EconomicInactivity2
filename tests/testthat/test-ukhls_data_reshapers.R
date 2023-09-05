test_that("extract_ind_from_waves input guards work", {

  expect_error(
    extract_ind_from_waves(
      varnames = c("blah1", "blah2"),
      vartypes = c("labels", "labels", "labels")
    )
  )

  expect_error(
    extract_ind_from_waves(
      varnames = c("blah", "blah2"),
      vartypes = c("labels", "libels")
    )
  )

  expect_error(
    extract_ind_from_waves(
      varnames = c("blah")
    )
  )

  expect_error(
    extract_ind_from_waves(
      vartypes = c("blah")
    )
  )

  expect_error(
    extract_ind_from_waves(
      varnames =  c(
        "jbstat", "dvage", "sex", "sf12mcs_dv", "sf12pcs_dv" #, "ethn_dv"
      ),

      vartypes = c(
        "labels", "values", "labels", "values", "values" #, "labels"
      ),

      base_dir_location = "nonsense_location"

    )
  )


})

test_that("extract_ind_from_waves returns valid output when given valid input", {

  out <- extract_ind_from_waves(
    varnames =  c(
      "jbstat", "dvage", "sex", "sf12mcs_dv", "sf12pcs_dv" #, "ethn_dv"
    ),

    vartypes = c(
      "labels", "values", "labels", "values", "values" #, "labels"
    )
  )

  expect_s3_class(
    out, "data.frame"
  )

})


test_that("extract_ind_from_waves can extract specific waves instead of just all waves", {

  # Case where there are multiple contiguous waves
  df <-
    extract_ind_from_waves(varnames = c('jbstat', 'dvage', 'sex'), vartypes = c('labels', 'values', 'labels'),
                           waves = letters[1:4])
  # print(head(df))

  expect_equal(
    length(unique(df$wave)),
    4
  )
  expect_true(
    dplyr::setequal(
      c("a", "b", "c", "d"),
      unique(df$wave)
    )
  )

  # Case where there are multiple non-contiguous waves
  df <-
    extract_ind_from_waves(varnames = c('jbstat', 'dvage', 'sex'), vartypes = c('labels', 'values', 'labels'),
                           waves = c("a", "g", "d"))
  expect_equal(
    length(unique(df$wave)),
    3
  )
  expect_true(
    dplyr::setequal(
      c("a", "g",  "d"),
      unique(df$wave)
    )
  )

  # Case where there is a single wave

  # Case where there are multiple non-contiguous waves
  df <-
    extract_ind_from_waves(varnames = c('jbstat', 'dvage', 'sex'), vartypes = c('labels', 'values', 'labels'),
                           waves = c("d"))
  expect_equal(
    length(unique(df$wave)),
    1
  )
  expect_true(
    dplyr::setequal(
      c("d"),
      unique(df$wave)
    )
  )

  # case where all waves is specified explicitly

  df <-
    extract_ind_from_waves(varnames = c('jbstat', 'dvage', 'sex'), vartypes = c('labels', 'values', 'labels'),
                           waves = "all")
  expect_equal(
    length(unique(df$wave)),
    12
  )
  expect_true(
    dplyr::setequal(
      letters[1:12],
      unique(df$wave)
    )
  )

})


test_that("smartly_widen_ind_dataframe input guards work", {


  expect_error(
    smartly_widen_ind_dataframe(
      df = data.frame(cheese = FALSE, onion = 7)
    )
  )

  expect_error(
    smartly_widen_ind_dataframe(
      df = data.frame(
        wave = c('a'), pidp = c(2229), variable = c("var1"), value = c(50)
      )
    )
  )

  expect_error(
    smartly_widen_ind_dataframe(
      df = data.frame(
        wave = c('a'), pidp = c(2229), variable = c("var1"), value = c(50)
      ),
      varnames = c("var1")
    )
  )

  expect_error(
    smartly_widen_ind_dataframe(
      df = data.frame(
        wave = c('a'), pidp = c(2229), variable = c("var1"), value = c(50)
      ),
      vartypes = c("labels")
    )
  )

  expect_error(
    smartly_widen_ind_dataframe(
      df = data.frame(
        wave = c('a'), pidp = c(2229), variable = c("var1"), value = c(50)
      ),
      varnames = c("var1"),
      vartypes = c("labels", "values")
    )
  )

  expect_error(
    smartly_widen_ind_dataframe(
      df = data.frame(
        wave = c('a'), pidp = c(2229), variable = c("var1"), value = c(50)
      ),
      varnames = c("var1", "var2"),
      vartypes = c("labels", "aroma")
    )
  )


})


test_that("smartly_widen_ind_dataframe_works_as_expected_with_valid_input", {

  # The following is taken from the real data
  test_df <-
    structure(
      list(
        pidp = c(138839687, 206645211, 409217887, 476384211,
      680581411, 1156124447, 1156666407, 1293139687, 1360769091, 1429960459,
      138839687, 206645211, 409217887, 476384211, 680581411, 1156124447,
      1156666407, 1293139687, 1360769091, 1429960459, 138839687, 206645211,
      409217887, 476384211, 680581411, 1156124447, 1156666407, 1293139687,
      1360769091, 1429960459, 138839687, 206645211, 409217887, 476384211,
      680581411, 1156124447, 1156666407, 1293139687, 1360769091, 1429960459,
      138839687, 206645211, 409217887, 476384211, 680581411, 1156124447,
      1156666407, 1293139687, 1360769091, 1429960459, 138839687, 206645211,
      409217887, 476384211, 680581411, 1156124447, 1156666407, 1293139687,
      1360769091, 1429960459),
        wave = c("a", "a", "a", "a", "a", "a",
    "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a",
    "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "b", "b",
    "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b",
    "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b",
    "b", "b"),
      variable = c("sex", "sex", "sex", "sex", "sex", "sex",
    "sex", "sex", "sex", "sex", "dvage", "dvage", "dvage", "dvage",
    "dvage", "dvage", "dvage", "dvage", "dvage", "dvage", "jbstat",
    "jbstat", "jbstat", "jbstat", "jbstat", "jbstat", "jbstat", "jbstat",
    "jbstat", "jbstat", "sex", "sex", "sex", "sex", "sex", "sex",
    "sex", "sex", "sex", "sex", "dvage", "dvage", "dvage", "dvage",
    "dvage", "dvage", "dvage", "dvage", "dvage", "dvage", "jbstat",
    "jbstat", "jbstat", "jbstat", "jbstat", "jbstat", "jbstat", "jbstat",
    "jbstat", "jbstat"),
      value = c("male", "female", "female", "male",
   "female", "female", "female", "male", "female", "male", "61",
   "54", "63", "41", "76", "55", "47", "76", "41", "16", "retired",
   "Paid employment(ft/pt)", "retired", "Paid employment(ft/pt)",
   "retired", "Paid employment(ft/pt)", "LT sick or disabled", "retired",
   "Paid employment(ft/pt)", "full-time student", "male", "female",
   "female", "male", "female", "female", "female", "male", "female",
   "male", "62", "55", "64", "42", "77", "56", "48", "77", "42",
   "17", "LT sick or disabled", "Paid employment(ft/pt)", "Family care or home",
   "Paid employment(ft/pt)", "retired", "Paid employment(ft/pt)",
   "LT sick or disabled", "Paid employment(ft/pt)", "Paid employment(ft/pt)",
   "full-time student")),
   row.names = c(NA, -60L),
   class = c("tbl_df",
      "tbl", "data.frame"
    )
  )

  test_varnames <- c("jbstat", "dvage", "sex")
  test_vartypes <- c("labels", "values", "labels")

  widened_df <- smartly_widen_ind_dataframe(test_df, varnames = test_varnames, vartypes = test_vartypes)

  expected_df <- structure(
    list(
      pidp = c(
        138839687, 206645211, 409217887, 476384211,
         680581411, 1156124447, 1156666407, 1293139687, 1360769091, 1429960459,
         138839687, 206645211, 409217887, 476384211, 680581411, 1156124447,
         1156666407, 1293139687, 1360769091, 1429960459),
      wave = c("a",
         "a", "a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b",
         "b", "b", "b", "b", "b", "b"),
      jbstat = c("retired", "Paid employment(ft/pt)",
       "retired", "Paid employment(ft/pt)", "retired", "Paid employment(ft/pt)",
       "LT sick or disabled", "retired", "Paid employment(ft/pt)", "full-time student",
     "LT sick or disabled", "Paid employment(ft/pt)", "Family care or home",
     "Paid employment(ft/pt)", "retired", "Paid employment(ft/pt)",
     "LT sick or disabled", "Paid employment(ft/pt)", "Paid employment(ft/pt)",
     "full-time student"),
     dvage = c(61, 54, 63, 41, 76, 55, 47, 76,
      41, 16, 62, 55, 64, 42, 77, 56, 48, 77, 42, 17),
     sex = c("male",
    "female", "female", "male", "female", "female", "female", "male",
          "female", "male", "male", "female", "female", "male", "female",
          "female", "female", "male", "female", "male")),
    row.names = c(NA,
    -20L),
    class = c("tbl_df", "tbl", "data.frame")
  )

  expect_identical(widened_df, expected_df)

  # If the number of variables in the dataframe equals the number specified in varnames then
  # there should be no warning

  expect_no_warning(
    smartly_widen_ind_dataframe(test_df, varnames = test_varnames, vartypes = test_vartypes)
  )

  # If fewer variables than exist in the dataset are specified, this should produce a warning

  test_varnames <- c("jbstat", "dvage")
  test_vartypes <- c("labels", "values")

  expect_warning(
    smartly_widen_ind_dataframe(test_df, varnames = test_varnames, vartypes = test_vartypes)
  )

})

