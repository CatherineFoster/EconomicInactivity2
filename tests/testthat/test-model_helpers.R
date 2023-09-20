test_that("calculate_baseline_counterfactual_distribution input guards work as expected", {

 # Arrange
  varnames <-  c(
    "jbstat", "dvage", "sex", "health"
  )

  vartypes <- c(
    "labels", "values", "labels", "labels"
  )

  df_ind <- get_ind_level_vars_for_selected_waves(varnames = varnames, vartypes = vartypes, waves = letters[1])

  # Clean the data
  df_ind_health_standardised <-
    df_ind |>
    # dvage uses negative values to indicate missing. The code below explicitly turns them all to missing values
    dplyr::mutate(across(dvage, function(x) ifelse(x < 0, NA, x))) |>
    # This renames dvage to age
    dplyr::rename(age = dvage) |>
    dplyr::filter(dplyr::between(age, 16, 64))  |>
    dplyr::mutate(
      lt_condition = dplyr::case_when(
        health %in% c("No", "no") ~ FALSE,
        health %in% c("Yes", "yes") ~ TRUE,
        TRUE ~ NA_integer_
      ) |> as.logical()
    ) %>%
      dplyr::filter(complete.cases(.))

  mod_01 <-
    nnet::multinom(
      next_status ~ this_status * sex + splines::bs(age, 5) + lt_condition,
      data = df_ind_health_standardised
    )

  df_baseline <- df_ind_health_standardised |>
    dplyr::filter(wave == 'a')

  df_counter <- df_baseline |>
    dplyr::mutate(lt_condition = FALSE)

  # Will fail if not all inputs provided

  expect_error(
    calculate_baseline_counterfactual_distribution()
  )

  expect_no_error(
    calculate_baseline_counterfactual_distribution(
      m = mod_01,
      d_base = df_baseline,
      d_counter = df_counter
    )
  )

  bad_model <- lm(hp ~ wt, data = mtcars)
  # # m needs to be a multnomial model
  expect_error(
    calculate_baseline_counterfactual_distribution(
      m = bad_model,
      d_base = df_baseline,
      d_counter = df_counter
    )
  )

  # # d_base needs to be a data.frame
  # stopifnot(
  #   "d_base is not a data.frame" =
  #     "data.frame" %in% class(d_base)
  # )
  expect_error(
    calculate_baseline_counterfactual_distribution(
      m = mod_01,
      d_base = df_baseline$sex,
      d_counter = df_counter
    )
  )
  #
  # # d_counter needs to be a data.frame

  expect_error(
    calculate_baseline_counterfactual_distribution(
      m = mod_01,
      d_base = df_baseline,
      d_counter = df_counter$age
    )
  )

  #
  # # d_base and d_counter need to have the same columns
  expect_error(
    calculate_baseline_counterfactual_distribution(
      m = mod_01,
      d_base = df_baseline |> dplyr::select(-age),
      d_counter = df_counter
    )
  )

  # # d_base and d_counter need to have the same number of rows
  expect_error(
    calculate_baseline_counterfactual_distribution(
      m = mod_01,
      d_base = df_baseline |> dplyr::slice(-c(50:52)),
      d_counter = df_counter
    )
  )

  # d_base cannot contain any missing values
  df_baseline_miss <- df_baseline
  df_baseline_miss[7, 3] <- NA

  expect_error(
    calculate_baseline_counterfactual_distribution(
      m = mod_01,
      d_base = df_baseline_miss,
      d_counter = df_counter
    )
  )

  # d_base cannot contain any missing values
  df_counter_miss <- df_counter
  df_counter_miss[23, 2] <- NA

  expect_error(
    calculate_baseline_counterfactual_distribution(
      m = mod_01,
      d_base = df_baseline,
      d_counter = df_counter_miss
    )
  )

})

test_that("calculate_baseline_counterfactual_distribution returns expected output structure with valid input", {

  varnames <- c(
    "jbstat", "dvage", "sex", "health"
  )

  vartypes <- c(
    "labels", "values", "labels", "labels"
  )

  df_ind <- get_ind_level_vars_for_selected_waves(varnames = varnames, vartypes = vartypes, waves = letters[1])

  # Clean the data
  df_ind_health_standardised <-
    df_ind |>
    # dvage uses negative values to indicate missing. The code below explicitly turns them all to missing values
    dplyr::mutate(across(dvage, function(x) ifelse(x < 0, NA, x))) |>
    # This renames dvage to age
    dplyr::rename(age = dvage) |>
    dplyr::filter(dplyr::between(age, 16, 64))  |>
    dplyr::mutate(
      lt_condition = dplyr::case_when(
        health %in% c("No", "no") ~ FALSE,
        health %in% c("Yes", "yes") ~ TRUE,
        TRUE ~ NA_integer_
      ) |> as.logical()
    ) %>%
    dplyr::filter(complete.cases(.))

  mod_01 <-
    nnet::multinom(
      next_status ~ this_status * sex + splines::bs(age, 5) + lt_condition,
      data = df_ind_health_standardised
    )

  df_baseline <- df_ind_health_standardised |>
    dplyr::filter(wave == 'a')

  df_counter <- df_baseline |>
    dplyr::mutate(lt_condition = FALSE)

  default_output <-
    calculate_baseline_counterfactual_distribution(
      m = mod_01,
      d_base = df_baseline,
      d_counter = df_counter
    )

  expect_equal(
    length(unique(default_output$output)),
    2
  )

  expect_true(
    dplyr::setequal(names(default_output), c("output", "scenario", "state", "value"))
  )

  abs_output <-
    calculate_baseline_counterfactual_distribution(
      m = mod_01,
      d_base = df_baseline,
      d_counter = df_counter,
      returnType = "absolute"
    )

  expect_equal(
    length(unique(abs_output$output)),
    1
  )

  rel_output <-
    calculate_baseline_counterfactual_distribution(
      m = mod_01,
      d_base = df_baseline,
      d_counter = df_counter,
      returnType = "relative"
    )

  expect_equal(
    length(unique(rel_output$output)),
    1
  )

})

test_that("calculate_absolutebaseline_counterfactual_distribution returns expected output structure with valid input", {

  varnames <- c(
    "jbstat", "dvage", "sex", "health"
  )

  vartypes <- c(
    "labels", "values", "labels", "labels"
  )

  df_ind <- get_ind_level_vars_for_selected_waves(varnames = varnames, vartypes = vartypes, waves = letters[1])

  # Clean the data
  df_ind_health_standardised <-
    df_ind |>
    # dvage uses negative values to indicate missing. The code below explicitly turns them all to missing values
    dplyr::mutate(across(dvage, function(x) ifelse(x < 0, NA, x))) |>
    # This renames dvage to age
    dplyr::rename(age = dvage) |>
    dplyr::filter(dplyr::between(age, 16, 64))  |>
    dplyr::mutate(
      lt_condition = dplyr::case_when(
        health %in% c("No", "no") ~ FALSE,
        health %in% c("Yes", "yes") ~ TRUE,
        TRUE ~ NA_integer_
      ) |> as.logical()
    ) %>%
    dplyr::filter(complete.cases(.))

  mod_01 <-
    nnet::multinom(
      next_status ~ this_status * sex + splines::bs(age, 5) + lt_condition,
      data = df_ind_health_standardised
    )

  df_baseline <- df_ind_health_standardised |>
    dplyr::filter(wave == 'a')

  df_counter <- df_baseline |>
    dplyr::mutate(lt_condition = FALSE)

  default_output <-
    calculate_absolute_baseline_counterfactual_distribution(
      m = mod_01,
      d_base = df_baseline,
      d_counter = df_counter
    )


  expect_true(
    dplyr::setequal(names(default_output), c("scenario", "state", "value"))
  )


})


test_that("plot_scenario_comparisons input guards work as expected", {

  # Arrange
  varnames <- c(
    "jbstat", "dvage", "sex", "health"
  )

  vartypes <- c(
    "labels", "values", "labels", "labels"
  )

  df_ind <- get_ind_level_vars_for_selected_waves(varnames = varnames, vartypes = vartypes, waves = letters[1])

  # Clean the data
  df_ind_health_standardised <-
    df_ind |>
    # dvage uses negative values to indicate missing. The code below explicitly turns them all to missing values
    dplyr::mutate(across(dvage, function(x) ifelse(x < 0, NA, x))) |>
    # This renames dvage to age
    dplyr::rename(age = dvage) |>
    dplyr::filter(dplyr::between(age, 16, 64))  |>
    dplyr::mutate(
      lt_condition = dplyr::case_when(
        health %in% c("No", "no") ~ FALSE,
        health %in% c("Yes", "yes") ~ TRUE,
        TRUE ~ NA_integer_
      ) |> as.logical()
    ) %>%
    dplyr::filter(complete.cases(.))

  mod_01 <-
    nnet::multinom(
      next_status ~ this_status * sex + splines::bs(age, 5) + lt_condition,
      data = df_ind_health_standardised
    )

  df_baseline <- df_ind_health_standardised |>
    dplyr::filter(wave == 'a')

  df_counter <- df_baseline |>
    dplyr::mutate(lt_condition = FALSE)

  default_output <-
    calculate_baseline_counterfactual_distribution(
      m = mod_01,
      d_base = df_baseline,
      d_counter = df_counter
    )

  ##########

  # Check guard on plotWhat
  expect_error(
    plot_scenario_comparisons(default_output, plotWhat = "something_else")
  )

  expect_no_error(
    plot_scenario_comparisons(default_output)
  )

  # check guard on dists
  expect_error(
    plot_scenario_comparisons(
      tibble(a = 1:3, b = 4:6)
    )
  )


})

test_that("make_tabular_summary input guards work as expected", {

  varnames <- c(
    "jbstat", "dvage", "sex", "health"
  )

  vartypes <- c(
    "labels", "values", "labels", "labels"
  )

  df_ind <- get_ind_level_vars_for_selected_waves(varnames = varnames, vartypes = vartypes, waves = letters[1])

  # Clean the data
  df_ind_health_standardised <-
    df_ind |>
    # dvage uses negative values to indicate missing. The code below explicitly turns them all to missing values
    dplyr::mutate(across(dvage, function(x) ifelse(x < 0, NA, x))) |>
    # This renames dvage to age
    dplyr::rename(age = dvage) |>
    dplyr::filter(dplyr::between(age, 16, 64))  |>
    dplyr::mutate(
      lt_condition = dplyr::case_when(
        health %in% c("No", "no") ~ FALSE,
        health %in% c("Yes", "yes") ~ TRUE,
        TRUE ~ NA_integer_
      ) |> as.logical()
    ) %>%
    dplyr::filter(complete.cases(.))

  mod_01 <-
    nnet::multinom(
      next_status ~ this_status * sex + splines::bs(age, 5) + lt_condition,
      data = df_ind_health_standardised
    )

  df_baseline <- df_ind_health_standardised |>
    dplyr::filter(wave == 'a')

  df_counter <- df_baseline |>
    dplyr::mutate(lt_condition = FALSE)

  default_output <-
    calculate_baseline_counterfactual_distribution(
      m = mod_01,
      d_base = df_baseline,
      d_counter = df_counter
    )

  expect_error(
    make_tabular_summary(default_output, outputType = "Something not valid")
  )

  expect_no_error(
    make_tabular_summary(default_output, outputType = "kable")
  )


})


test_that("make_tabular_summary works with valid inputs", {
  # Make the valid inputs ('arrange')
  varnames <- c(
    "jbstat", "dvage", "sex", "health"
  )

  vartypes <- c(
    "labels", "values", "labels", "labels"
  )

  df_ind <- get_ind_level_vars_for_selected_waves(varnames = varnames, vartypes = vartypes, waves = letters[1])

  # Clean the data
  df_ind_health_standardised <-
    df_ind |>
    # dvage uses negative values to indicate missing. The code below explicitly turns them all to missing values
    dplyr::mutate(across(dvage, function(x) ifelse(x < 0, NA, x))) |>
    # This renames dvage to age
    dplyr::rename(age = dvage) |>
    dplyr::filter(dplyr::between(age, 16, 64))  |>
    dplyr::mutate(
      lt_condition = dplyr::case_when(
        health %in% c("No", "no") ~ FALSE,
        health %in% c("Yes", "yes") ~ TRUE,
        TRUE ~ NA_integer_
      ) |> as.logical()
    ) %>%
    dplyr::filter(complete.cases(.))

  mod_01 <-
    nnet::multinom(
      next_status ~ this_status * sex + splines::bs(age, 5) + lt_condition,
      data = df_ind_health_standardised
    )

  df_baseline <- df_ind_health_standardised |>
    dplyr::filter(wave == 'a')

  df_counter <- df_baseline |>
    dplyr::mutate(lt_condition = FALSE)

  default_output <-
    calculate_baseline_counterfactual_distribution(
      m = mod_01,
      d_base = df_baseline,
      d_counter = df_counter
    )

  tabular_summary <- make_tabular_summary(default_output)

  expect_true(
    dplyr::setequal(
      c("state", "baseline", "counterfactual", "abs_label", "rel_label"),
      colnames(tabular_summary)
    )
  )

  expect_equal(
    dim(tabular_summary)[1],
    7
  )

})
