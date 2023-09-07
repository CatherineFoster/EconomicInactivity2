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


test_that("simplify_econ_status_categories input guards work", {

  expect_error(
    simplify_econ_status_categories(
      original = c("Govt training scheme", "doing something nonspecific")
  ))

  expect_no_error(
    simplify_econ_status_categories(
      original = c("Govt training scheme", "retired")
    )
  )

  expect_error(
    simplify_econ_status_categories(
      original = c("Govt training scheme", "retired"),
      level = 6
    )
  )

  expect_no_error(
    simplify_econ_status_categories(
      original = c("Govt training scheme", "retired"),
      level = 1
    )
  )

})

test_that("simplify_econ_status_categories works as expected with valid inputs", {

  example_original <- c("retired", "Paid employment(ft/pt)", "retired", "Paid employment(ft/pt)",
    "retired", "Family care or home", "Paid employment(ft/pt)", "Paid employment(ft/pt)",
    "retired", "Retired", "retired", "Retired", "Paid employment(ft/pt)",
    "Paid employment(ft/pt)", "Paid employment(ft/pt)", "Paid employment(ft/pt)",
    "Paid employment(ft/pt)", "Unemployed", "Family care or home",
    "Family care or home")

  # Level 3 (default) example
  expect_equal(
    simplify_econ_status_categories(example_original),
    c("Inactive retired", "Employed", "Inactive retired", "Employed",
      "Inactive retired", "Inactive care", "Employed", "Employed",
      "Inactive retired", "Inactive retired", "Inactive retired", "Inactive retired",
      "Employed", "Employed", "Employed", "Employed", "Employed", "Unemployed",
      "Inactive care", "Inactive care")
  )

  expect_equal(
    simplify_econ_status_categories(example_original, level = 1),
    c("Economically Inactive", "Economically Active", "Economically Inactive",
      "Economically Active", "Economically Inactive", "Economically Inactive",
      "Economically Active", "Economically Active", "Economically Inactive",
      "Economically Inactive", "Economically Inactive", "Economically Inactive",
      "Economically Active", "Economically Active", "Economically Active",
      "Economically Active", "Economically Active", "Economically Active",
      "Economically Inactive", "Economically Inactive")
  )

  expect_equal(
    simplify_econ_status_categories(example_original, level = 2),
    c("Inactive", "Employed", "Inactive", "Employed", "Inactive",
      "Inactive", "Employed", "Employed", "Inactive", "Inactive", "Inactive",
      "Inactive", "Employed", "Employed", "Employed", "Employed", "Employed",
      "Unemployed", "Inactive", "Inactive")
  )

  expect_equal(
    simplify_econ_status_categories(example_original, level = 3),
    simplify_econ_status_categories(example_original)
  )

  expect_equal(
    simplify_econ_status_categories(c("inapplicable", "retired", "refusal")),
    c(NA, "Inactive retired", NA)
  )

  expect_equal(
    simplify_econ_status_categories(c("inapplicable", "retired", "refusal"), missingAsNa = FALSE),
    c("Missing", "Inactive retired", "Missing")
  )


})


test_that("pull_next_wave_status input guards work as expected", {

  # No pidp
  expect_error(
    pull_next_wave_status(
      df = data.frame(
        wave = c('g')
      )
    )
  )

  # no wave
  expect_error(
    pull_next_wave_status(
      df = data.frame(
        pidp = 4585
      )
    )
  )

  # wave and pidp - should work

  expect_no_error(
    pull_next_wave_status(
      df = data.frame(
        wave = 'g',
        pidp = 4585
      )
    )
  )

  # invalid recodeLevel - should fail
  expect_error(
    pull_next_wave_status(
      df = data.frame(
        wave = 'g',
        pidp = 4585
      ),
      recodeLevel = 7
    )
  )

  # valid recodeLevel - should not fail
  expect_no_error(
    pull_next_wave_status(
      df = data.frame(
        wave = 'g',
        pidp = 4585
      ),
      recodeLevel = 2
    )
  )

})

test_that("pull_next_wave_status works as expected with valid input", {
  # ten valid pidps each from three waves
  test_input_df <-
    structure(
      list(
        wave = c("a", "a", "a", "a", "a", "a", "a", "a",
          "a", "a", "f", "f", "f", "f", "f", "f", "f", "f", "f", "f", "g",
          "g", "g", "g", "g", "g", "g", "g", "g", "g"),
        pidp = c(408611335,
         69301527, 748401887, 1633093451, 273102967, 1428953367, 1632874487,
         1360909847, 613678255, 1292439967, 410570411, 1632807167, 612602503,
         829746209, 545126091, 157065725, 348275730, 273200211, 613009817,
         613094127, 776169009, 1632458327, 340392369, 340425007, 1157080531,
         1088240051, 1496898291, 1020238011, 137389931, 564674045)),
      class = c("tbl_df",
   "tbl", "data.frame"),
   row.names = c(NA, -30L)
   )

  test_output_l3_df <-
structure(list(wave = c("a", "a", "a", "a", "a", "f", "f", "f",
  "f", "f", "f", "f", "f", "f", "g", "g", "g", "g", "g", "g", "g",
  "g"), pidp = c(273102967, 408611335, 1360909847, 1428953367,
                 1633093451, 157065725, 273200211, 410570411, 545126091, 612602503,
                 613009817, 613094127, 829746209, 1632807167, 340425007, 564674045,
                 776169009, 1020238011, 1088240051, 1157080531, 1496898291, 1632458327
  ), next_status = c("Employed", "Employed", "Employed", "Inactive care",
                     "Employed", "Inactive retired", "Employed", "Employed", "Employed",
                     "Employed", "Inactive student", "Inactive retired", "Employed",
                     "Inactive retired", "Inactive care", "Inactive retired", "Inactive retired",
                     "Employed", "Inactive retired", "Employed", "Employed", "Inactive retired"
  )), row.names = c(NA, -22L), class = c("tbl_df", "tbl", "data.frame"
  ))

  expect_equal(
    pull_next_wave_status(test_input_df),
    test_output_l3_df
  )

})


test_that("pull_next_wave_status will fail if given pidps from the last wave", {
  last_wave_pidps <-
    structure(
      list(
        pidp = structure(c(272262485, 884040819, 1224235287,
      1579238930, 557763964, 272771809, 1361081211, 884986007, 612455609,
      163737901),
      label = "cross-wave person identifier (public release)", format.stata = "%12.0g"),
     wave = c("l", "l", "l", "l", "l", "l", "l", "l", "l", "l"
   )), row.names = c(NA, -10L), class = c("tbl_df", "tbl", "data.frame"
   ))

  expect_error(
    pull_next_wave_status(last_wave_pidps)
  )

  # By contrast there should be no error if using penultimate wave

penultimate_wave_pidps <-
  structure(
    list(
      pidp = structure(c(137283847, 1632425691, 410102567,
      1156444051, 274478617, 817048567, 136690207, 478913809, 953047286,
      340580727), label = "cross-wave person identifier (public release)", format.stata = "%12.0g"),
     wave = c("k", "k", "k", "k", "k", "k", "k", "k", "k", "k"
     )), row.names = c(NA, -10L), class = c("tbl_df", "tbl", "data.frame"
   ))

  expect_no_error(
    pull_next_wave_status(penultimate_wave_pidps)
  )

})


test_that("get_ind_level_vars_for_selected_waves works as expected", {
  expected_wave_c_first_5 <-
    structure(
      list(
        pidp = c(280165, 541285, 541965, 665045, 956765),
        wave = c("c", "c", "c", "c", "c"),
        dvage = c(32, 25, 23, 29, 56),
        sex = c("female", "male", "female", "male", "male"),
        this_status = c("Employed", "Employed", "Employed", "Employed", "Employed"),
        next_status = c("Employed", "Unemployed", NA, "Employed", NA)),
      row.names = c(NA, -5L),
      class = c("tbl_df", "tbl", "data.frame")
    )

  expect_equal(
    get_ind_level_vars_for_selected_waves(
      varnames = c("jbstat", "dvage", "sex"),
      vartypes = c("labels", "values", "labels"),
      waves = "c") |> dplyr::slice(1:5),
    expected_wave_c_first_5
  )


})


test_that("add_hh_variables input guards work", {

  # df is wrong - werve not wave
  expect_error(
    add_hh_variables(
      df = data.frame(werve = c("a", "d"), pidp = c(1, 2), something_else = c("cheese", "beans")),
      varnames = "nkids_dv",
      vartypes = "values"
    )
  )

  # df is not wrong
  expect_no_error(
    add_hh_variables(
      df = data.frame(wave = c("a", "d"), pidp = c(1, 2), something_else = c("cheese", "beans")),
      varnames = "nkids_dv",
      vartypes = "values"
    )
  )

  expect_error(
    add_hh_variables(
      df = data.frame(wave = c("a", "d"), pidp = c(1, 2), something_else = c("cheese", "beans")),
      varnames = "nkids_dv",
      vartypes = "integers" #should be values or labels
    )

  )


})

test_that("add_hh_variables works as expected with valid input", {

  # First example: A single wave
  ind_df <-
    get_ind_level_vars_for_selected_waves(
      varnames = c("jbstat", "dvage", "sex"),
      c("labels", "values", "labels"),
      waves = "c"
    )

  ind_joined_df <- ind_df |>
    add_hh_variables(
      varnames = c("numadult", "hhsize"),
      vartypes = c("values", "values")
    )

  expected_joined_head <-
    structure(
      list(
        pidp = c(280165, 541285, 541965, 665045, 956765),
        wave = c("c", "c", "c", "c", "c"),
        dvage = c(32, 25, 23, 29, 56),
        sex = c("female", "male", "female", "male", "male"),
        this_status = c("Employed", "Employed", "Employed", "Employed", "Employed"),
        next_status = c("Employed", "Unemployed", NA, "Employed", NA),
        hidp = structure(c(759532804,  146730404, 146730404, 215192804, 758832404),
                         label = "household identifier (public release)", format.stata = "%12.0g"),
    hhsize = structure(c(3, 4, 4, 4, 2), label = "number of people in household"),
    numadult = structure(c(2, 4, 4, 4, 2), label = "number of people in household")),
    row.names = c(NA, -5L),
    class = c("tbl_df", "tbl", "data.frame")
  )

  expect_equal(
    ind_joined_df |> dplyr::slice(1:5),
    expected_joined_head
  )


  # Second example, three non-contiguous waves

  ind_df <-
    get_ind_level_vars_for_selected_waves(
      varnames = c("jbstat", "dvage", "sex"),
      c("labels", "values", "labels"),
      waves = c("b", "g", "j")
    )

  ind_joined_df <- ind_df |>
    add_hh_variables(
      varnames = c("numadult", "hhsize"),
      vartypes = c("values", "values")
    )

  expect_multiwave_first_three <-
    structure(list(pidp = c(280165, 956765, 987365, 22445, 29925,
    76165, 22445, 29925, 76165), wave = c("b", "b", "b", "g", "g",
    "g", "j", "j", "j"), dvage = c(31, 55, 20, 30, 38, 32, 33, 41,
    35), sex = c("female", "male", "female", "female", "female",
    "female", "female", "female", "female"), this_status = c("Employed",
    "Employed", "Inactive student", "Employed", "Employed", "Employed",
    "Employed", "Employed", "Employed"), next_status = c("Employed",
    "Employed", "Employed", "Employed", "Employed", "Employed", "Employed",
    "Employed", "Employed"), hidp = c(783876802, 783230802, 170639202,
    278447092, 620316412, 142378412, 277059218, 618630018, 141460418
    ), numadult = structure(c(2, 2, 2, NA, NA, NA, NA, NA, NA), label = "number of adults (aged 16 or over) in household"),
    hhsize = structure(c(3, 2, 2, 1, 3, 2, 3, 3, 3), label = "number of adults (aged 16 or over) in household")), class = c("tbl_df",
    "tbl", "data.frame"), row.names = c(NA, -9L))

  expect_equal(
    ind_joined_df |>
      dplyr::group_by(wave) |>
      dplyr::slice(1:3) |>
      dplyr::ungroup(wave),
    expect_multiwave_first_three
  )

})
