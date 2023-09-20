

#' Calculate absolute baseline-counterfactual distribution
#' Calculates only the absolute numbers in the scenarios. The idea being that this will
#' be required for the calculation of the relative, but not vice-versa.
#' @param m the model. Must contain age, sex, this_state, next_state, and other predictor variables
#' @param d_base The baseline dataset. Could be observed data from a specific wave, or purely hypothetical data
#' @param d_counter The counterfactual dataset. Could be a modification of d_base, and/or purely hypothetical data
#'
#' @return a tibble object with the specified values
#' @export
#'
#' @examples
calculate_absolute_baseline_counterfactual_distribution <- function(m, d_base, d_counter){
  # m needs to be a multnomial model
  stopifnot(
    "m is not a model of required class (multinom)" =
      "multinom" %in% class(m)
  )

  # d_base needs to be a data.frame
  stopifnot(
    "d_base is not a data.frame" =
      "data.frame" %in% class(d_base)
  )

  # d_counter needs to be a data.frame
  stopifnot(
    "d_counter is not a data.frame" =
      "data.frame" %in% class(d_counter)
  )

  # d_base and d_counter need to have the same columns
  stopifnot(
    "d_base and d_counter do not have the same columns" =
      dplyr::setequal(names(d_base), names(d_counter))
  )

  # d_base and d_counter need to have the same number of rows
  stopifnot(
    "d_base and d_counter have different numbers of observations" =
      dim(d_base)[1] == dim(d_counter)[1]
  )

  # d_base cannot have any missing observations
  stopifnot(
    "d_base contains missing observations" =
      !any(!complete.cases(d_base))
  )

  # d_counter cannot have any missing observations
  stopifnot(
    "d_counter contains missing observations" =
      !any(!complete.cases(d_counter))
  )

  preds_baseline <- predict(m, newdata = d_base, type = "probs")
  preds_counter <- predict(m, newdata = d_counter, type = "probs")

  resultsMatrixAbsolute <- cbind(
    apply(preds_baseline, 2, sum),
    apply(preds_counter, 2, sum)
  )

  colnames(resultsMatrixAbsolute) <- c("baseline", "counterfactual")


  out <- dplyr::bind_rows(
    tibble::as_tibble(resultsMatrixAbsolute, rownames = "state")
  )

  out <- out |>
    tidyr::pivot_longer(
      cols = c("baseline", "counterfactual"),
      values_to = "value",
      names_to = "scenario"
    ) |>
    dplyr::select(scenario, state, value)

  out

}

#' Calculate baseline and counterfactual state distributions
#' Given a model, a baseline dataset, and a counterfactual dataset, calculate the estimated distribution of
#' the population in each economic (in)activity state in the next wave
#' @param m the model. Must contain age, sex, this_state, next_state, and other predictor variables
#' @param d_base The baseline dataset. Could be observed data from a specific wave, or purely hypothetical data
#' @param d_counter The counterfactual dataset. Could be a modification of d_base, and/or purely hypothetical data. Could also be a named list of data frames
#' @param returnType Either absolute, relative, or both. Whether to return the absolute numbers in each state under each scenario, or
#' the change from the baseline scenario
#'
#' @return a tibble object with the specified values
#' @export
#'
#' @examples
calculate_baseline_counterfactual_distribution <- function(m, d_base, d_counter, returnType = c("absolute", "relative")){

  # m needs to be a multnomial model
  stopifnot(
    "m is not a model of required class (multinom)" =
    "multinom" %in% class(m)
  )

  # d_base needs to be a data.frame
  stopifnot(
    "d_base is not a data.frame" =
      "data.frame" %in% class(d_base)
  )

  stopifnot(
    "d_counter is not a data.frame or named list of data.frames" =
      "data.frame" %in% class(d_counter) |
      "list" %in% class(d_counter)
  )

  if ("data.frame" %in% class(d_counter)) {
    # d_base and d_counter need to have the same columns
    stopifnot(
      "d_base and d_counter do not have the same columns" =
        dplyr::setequal(names(d_base), names(d_counter))
    )

    # d_base and d_counter need to have the same number of rows
    stopifnot(
      "d_base and d_counter have different numbers of observations" =
        dim(d_base)[1] == dim(d_counter)[1]
    )

    # d_counter cannot have any missing observations
    stopifnot(
      "d_counter contains missing observations" =
        !any(!complete.cases(d_counter))
    )
  } else if ("list" %in% class(d_counter)) {

    # all elements in the list need to be named
    names_d_counter <- names(d_counter)
    named_names_d_counter <- names_d_counter[names_d_counter != ""]

    stopifnot(
      "not all elements in d_counter list are named" =
      length(names_d_counter) == length(named_names_d_counter)
    )

    # all elements in d_counter are data.frames
    stopifnot(
      "not all elements in d_counter are of data.frame class" =
        sapply(d_counter, function(x) "data.frame" %in% class(x)) |> all()
    )

    # all data.frame objects in d_counter have the same columns
    N <- length(d_counter)
    stopifnot(
      "Not all data.frame elements in d_counter have hte same columns" =
      purrr::map2(d_counter[-1], d_counter[-N], setequal) |>
        purrr::list_simplify() |>
        all()
    )

  }


  # d_base cannot have any missing observations
  stopifnot(
    "d_base contains missing observations" =
      !any(!complete.cases(d_base))
  )


  # the columns in d_counter and d_base must include the predictors in the model
  # TO DO!

  # return must contain 'absolute' or 'relative'
  # return must not contain anything other than 'absolute' or 'relative'
  stopifnot(
    "returnType not valid (should be exclusively 'absolute' or 'relative')" =
      ("absolute" %in% returnType | "relative" %in% returnType) &
      length(returnType[!returnType %in% c("absolute", "relative")]) == 0
  )

  preds_baseline <- predict(m, newdata = d_base, type = "probs")
  preds_counter <- predict(m, newdata = d_counter, type = "probs")

  resultsMatrixAbsolute <- cbind(
    apply(preds_baseline, 2, sum),
    apply(preds_counter, 2, sum)
  )

  colnames(resultsMatrixAbsolute) <- c("baseline", "counterfactual")

  if ("relative" %in% returnType) {
    resultsMatrixRelative <- apply(resultsMatrixAbsolute, 1, function(x) x / x[1]) |> t()
  }

  if ("relative" %in% returnType & "absolute" %in% returnType) {
    out <- dplyr::bind_rows(
      tibble::as_tibble(resultsMatrixAbsolute, rownames = "state") |>
        dplyr::mutate(output = "absolute"),
      tibble::as_tibble(resultsMatrixRelative, rownames = "state") |>
        dplyr::mutate(output = "relative")
    )
  } else if ("relative" %in% returnType) {
    out <- tibble::as_tibble(resultsMatrixRelative, rownames = "state") |>
      dplyr::mutate(output = "relative")
  } else if ("absolute" %in% returnType) {
    out <- tibble::as_tibble(resultsMatrixAbsolute, rownames = "state") |>
      dplyr::mutate(output = "absolute")
  } else {
    stop("Invalid returnType (This should have been caught earlier!)")
  }

  out <- out |>
    tidyr::pivot_longer(
      cols = c("baseline", "counterfactual"),
      values_to = "value",
      names_to = "scenario"
    ) |>
    dplyr::select(output, scenario, state, value)


  out
}


#' Plot scenario comparison information
#' Given the output of calculate_baseline_counterfactual_distribution, produce nice plots to visually summarise the information
#'
#' @param dists an object created by calculate_baseline_counterfactual_distribution, which should be of class distFrame
#' @param title What to put on the title of the figure
#' @param plotWhat Whether to plot absolute, relative, or both
#' @param simplify Whether to reduce from seven mutually exclusive states to 2 (Will only work with absolute)
#' @param polar Whether to show distribution as polar cooardinates
#' @param dynamic Whether to show a static ggplot2 chart (default) or a plotly interactive chart
#'
#' @return
#' @export
#'
#' @examples
plot_scenario_comparisons <- function(
    dists,
    title = "Default Title",
    plotWhat = "absolute",
    simplify = FALSE,
    polar = FALSE,
    dynamic = FALSE
) {

  stopifnot(
    "dists object not of expected type (should contain columns output, scenario, state and value)" =
      dplyr::setequal(names(dists), c("output", "scenario", "state", "value"))
  )

  # We now need to remove this additional class as it screws up other functions!
  dists <- strip_distro_class(dists)

  stopifnot(
    "plotWhat contains invalid values (should be 'absolute' or 'relative')" =
      plotWhat == 'absolute' | plotWhat == 'relative'
  )


  if (plotWhat == 'absolute') {
    out <- make_abs_colchart(dists)
  } else if (plotWhat == 'relative'){
    out <- make_rel_barchart(dists)
  } else {
    stop("plotWhat not of valid type (should be absolute or relative)")
  }


  out
}

strip_distro_class <- function(x){
  class(x) <- class(x)[class(x) != 'distFrame']
  x
}


make_abs_colchart <- function(x, title = NULL, subtitle = NULL, caption = NULL){

  gg <-
    x |>
      strip_distro_class() |>
      dplyr::filter(output == "absolute")  |>
      dplyr::mutate(
        state =
          ordered(
            state,
            levels = c(
              "Employed", "Unemployed", "Inactive long term sick",
              "Inactive student", "Inactive care",
              "Inactive retired", "Inactive other"
            )
          )
      ) |>
      dplyr::group_by(scenario) |>
      dplyr::arrange(state) |>
      dplyr::mutate(cumulative_count = cumsum(value)) |>
      dplyr::mutate(mid_y = lag(cumulative_count, default = 0) + value / 2) |>
      dplyr::mutate(
        label = glue::glue("{state}: {round(value, 0)} ({round(100 * value / sum(value), 1)}%)")
        ) |>
      ggplot2::ggplot(
        ggplot2::aes(x = scenario, y = value, fill = state)
      ) +
      ggplot2::geom_col() +
      shadowtext::geom_shadowtext(
        ggplot2::aes(
          label = label, x = scenario, y = max(cumulative_count) - mid_y
        )
      ) +
      ggplot2::theme(
        legend.position = "none"
      ) +
      ggplot2::labs(
        x = "Scenario",
        y = "Count of persons",
        title = title,
        subtitle = subtitle,
        caption = caption
      )
  gg
}

make_rel_barchart <- function(x, title = NULL, subtitle = NULL, caption = NULL) {

  relDat <-
    x |>
    dplyr::filter(output == 'relative') |>
    dplyr::filter(scenario == 'counterfactual') |>
    dplyr::mutate(
      state =
        ordered(
          state,
          levels = c(
            "Employed", "Unemployed", "Inactive long term sick",
            "Inactive student", "Inactive care",
            "Inactive retired", "Inactive other"
          )
        )
    ) |>
    mutate(value = 100 * value) |>
    mutate(
      label = dplyr::if_else(
        value > 100,
        glue::glue("{round(value - 100)}% up"),
        glue::glue("{round(100 - value)}% down")
        )
    ) |>
    mutate(
      lab_x_pos = dplyr::if_else(
        value > 100,
        value + 5,
        value + (100 - value) / 2
      )
    )
  gg <-
    relDat |>
      ggplot2::ggplot(ggplot2::aes(x = value, y= state)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::geom_vline(xintercept = 100) +
      ggplot2::labs(
        x = "Percent relative to counterfactual",
        y = "Economic Status"
      ) +
      shadowtext::geom_shadowtext(ggplot2::aes(x = lab_x_pos, y = state, label = label)) +
      ggplot2::expand_limits(x = c(0, max(relDat$value) * 1.1))
  gg
}


#' Produce a tabular summary of results from plot_scenario_comparisons
#'
#' @param x A valid output from plot_scenario_comparisons
#' @param title the title to give the table
#' @param subtitle the subtitle to give the table
#'
#' @return the summary output
#' @export
#'
#' @examples
make_tabular_summary <- function(
    x, title = NULL, subtitle = NULL, outputType = "standard"
  ) {

  stopifnot(
    "outputType is not valid (should be one of 'standard' or 'kable')" =
    length(outputType) == 1 & outputType %in% c('standard', 'kable')
  )

  output <-
  x |>
    tidyr::pivot_wider(names_from = "scenario", values_from = "value") |>
    dplyr::mutate(
      state =
        ordered(
          state,
          levels = c(
            "Employed", "Unemployed", "Inactive long term sick",
            "Inactive student", "Inactive care",
            "Inactive retired", "Inactive other"
          )
        )
    ) |>
    dplyr::arrange(state) |>
    dplyr::filter(output == "absolute") |>
    dplyr::mutate(
      abs_diff = counterfactual - baseline
    ) |>
    dplyr::mutate(
      relative_difference = abs_diff / baseline
    ) |>
    dplyr::select(-output) |>
    dplyr::mutate(
      abs_label = dplyr::if_else(
        abs_diff > 0,
        glue::glue("{round(abs_diff, 0)} more"),
        glue::glue("{abs(round(abs_diff, 0))} less")
      )
    ) |>
    dplyr::mutate(
      rel_label = dplyr::if_else(
        relative_difference > 0,
        glue::glue("{round(relative_difference * 100, 1)}% up"),
        glue::glue("{round(abs(relative_difference) * 100, 1)}% down"),
      )
    ) |>
    dplyr::select(state, baseline, counterfactual, abs_label, rel_label)

  if (outputType == 'kable') {
    output$state <- c("Employed", "Unemployed", "Long-term Sick", "Student", "Full-time Carer", "Retired", "Other")
     output <- output |>
       kableExtra::kbl(
         col.names = c("", "Baseline", "Counterfactual", "Absolute", "Relative"),
         digits = 0, caption = title,
         table.attr = "style = \"color: black; background-color: white\""
        ) |>
       kableExtra::kable_styling() |>
       kableExtra::add_header_above(c(" ", "Scenarios" = 2, "Comparisons" = 2)) |>
       kableExtra::pack_rows("Active", 1,2) |>
       kableExtra::pack_rows("Inactive", 3,7) |>
       kableExtra::footnote(general = subtitle, general_title = "")
  }

  output
}


