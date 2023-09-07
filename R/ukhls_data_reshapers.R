

# UKHLS Data Reshaper Functions

# The aim of this collection of functions is to simplify and standardise standard
# types of data refining and reshaping as required by the modelling

# Examples of processes we want to do multiple times
# 1. Extract individual level data for multiple waves
# 2. Convert jbstat to a simplified grouping
# 3. Get simplified jbstat for an individual in the next wave
# 4. Reshape ind level variables from long to wide
# 5. Attach hh level variables to ind level variables
# 6. Equivalise household incomes and costs


#' Intelligently widen long individual level dataframes
#'
#' @param df the dataframe in long format. Must contain wave and pidp variables,
#' and columns called variable and value
#' @param varnames the variable names in the variable column to work on
#' @param vartypes The type of variable to convert the variables to (labels or values)
#' @param verbose Boolean. Whether to report on progress while executing
#'
#' @return out a widened dataframe with pipd and wave as columns, followed by additional columns for each
#' variable specified in varnames and found in df
#' @export
#'
#' @examples
smartly_widen_ind_dataframe <- function(df, varnames, vartypes, verbose = FALSE){

  stopifnot(
    "dataframe input does not have expected variables" =
    dplyr::setequal(
      c("wave", "pidp", "variable", "value"),
      names(df)
    )
  )

  stopifnot(
    "varnames is NULL" =
    !is.null(varnames)
  )

  stopifnot(
    "vartypes is NULL" =
      !is.null(vartypes)
  )

  stopifnot(
    "Not all vartypes listed are acceptable (should be one of labels or values)" =
      unique(vartypes) %in% c("labels", "values") |> all()
  )

  stopifnot(
    "The number of varnames is not equal to the number of vartypes" =
      length(varnames) == length(vartypes)
  )

  # What do we want to do?
  # For each varname in varnames
  # find corresponding vartype
  # filter df on whether variable == varname
  # select wave, pidp, value
  # rename value to varname
  # use zap labels if vartype is values
  # use as_factor as.character if vartype is labels
  # save above as one element in list
  # iteratively full join all elements in list by wave and pidp

  unique_vars_found <- unique(df$variable)

  if (verbose) {
    message("Found ", length(unique_vars_found), " variables in dataset")
  }

  if (length(unique_vars_found) != length(varnames)) {
    warning("The number of variables found in the dataset is not equal to the number of variables specified")
  } else {
    if (verbose) {
      message("The number of variables found in the dataset is equal to the number of variables specified")
    }
  }

  rename_widen <- function(varname, vartype, df) {
    out <- df |>
      dplyr::filter(variable == varname) |>
      dplyr::rename({{varname}} := value) |>
      dplyr::select(-variable)

    if (vartype == "values") {
      if (verbose) {
        message("converting ", varname, " to numeric")
      }
      out[[varname]] <- as.numeric(out[[varname]])
    } else if (vartype == "labels") {
      if (verbose) {
        message("keeping ", varname, " as character")
      }
    }
    out
  }
  var_elements_wide_df <- purrr::map2(varnames, vartypes, rename_widen, df = df) |>
    purrr::reduce(dplyr::inner_join, by = c("pidp", "wave"))
  var_elements_wide_df
}

#' Extract individual level data from all waves
#'
#' @param varnames The names of the variables to extract
#' @param vartypes The types of the variables to extract (labels or values)
#' @param strict If TRUE, the function will fail if not all variables are found in all waves
#' @param verbose Whether to report information about progress or not
#' @param waves Either the keyword 'all', or a vector of letters corresponding to which waves to include
#' @param base_dir_location Where the collection of _indresp files can be found. Do not change unless necessary
#'
#' @return Dataframe with wave, pid and other variables as columns (i.e. pre-widened)
#' @export
#'
#' @examples
extract_ind_from_waves <- function(
    varnames, vartypes, strict = FALSE, verbose = FALSE, waves = 'all',
    base_dir_location =   "big_data/UKDA-6614-stata/stata/stata13_se/ukhls"
                                   ){



  stopifnot(
    "no varnames supplied" =
      !is.null(varnames)
  )

  stopifnot(
    "no vartypes supplied" =
      !is.null(vartypes)
  )

  stopifnot(
    "Not all vartypes listed are acceptable (should be one of labels or values)" =
      unique(vartypes) %in% c("labels", "values") |> all()
  )

  stopifnot(
    "The number of varnames is not equal to the number of vartypes" =
      length(varnames) == length(vartypes)
  )

  stopifnot(
    "waves is not either 'all' or one or more letters" =
      (length(waves) == 1 & waves == 'all') |
      (unique(waves) %in% letters |> all())
  )



  indresp_pattern = if (length(waves) == 1) {
    if (waves == 'all') {
      "[a-z]_indresp.dta"
    } else {
      paste0(
        "[",
        waves,
        "]_indresp.dta"
      )
    }
  } else {
    paste0(
      "[",
        paste(waves, collapse = "|"),
      "]_indresp.dta"
    )
  }

  indresp_files <- dir(here::here(base_dir_location), pattern = indresp_pattern, full.names = TRUE)

  stopifnot(
    "No relevant files found in base directory location specified" =
      length(indresp_files) > 0
  )

  if (verbose) {
    overall_start_time = Sys.time()

  }

  long_slimmed_datalist <- lapply(indresp_files,
                                  read_and_slim_data,
                                  varnames = varnames,
                                  extract_what = vartypes,
                                  verbose = verbose
  )

  long_slimmed_data <- dplyr::bind_rows(long_slimmed_datalist)

  if (verbose){
    overall_end_time = Sys.time()

    print(paste(
      "Overall process took",
      difftime(overall_end_time, overall_start_time, units = "mins"),
      "minutes"
    ))

  }

  rm(long_slimmed_datalist)
  long_slimmed_data
}

#' Simplify economic status categories
#' Convert economic status categories to simplified and standardised groupings
#'
#' @param original Usually the variable jbstat. Can be either a data.frame of 1 column, or a character vector
#' @param level The level of aggregation to perform. Options are
#' 1) 2 levels (Active, Inactive)
#' 2) 3 levels (Employed, Unemployed, Inactive)
#' 3) 7 levels (2 active levels; 5 inactive levels)
#' 4) Multiple levels - effectively just tidying original variable labels, rather than reaggregating
#' @param newName The name of the new variable to create
#' @param missingAsNa Whether to convert the category 'Missing' to NA (default TRUE)
#'
#' @return revised The revised categorisations. Just a character vector.
#' @export
#'
#' @examples
simplify_econ_status_categories <- function(original, level = 3, newName = "econ_status", missingAsNa = TRUE) {

  # Checks
  # Check that original only contains variables the categorisation schema can work with
  validOrigCategories <- c("Paid employment(ft/pt)", "retired", "doing something else",
                           "Govt training scheme", "refusal", "Self employed", "On maternity leave",
                           "inapplicable", "unemployed", "LT sick or disabled", "self employed",
                           "Unpaid, family business", "missing", "Doing something else",
                           "Unemployed", "On furlough", "full-time student", "Family care or home",
                           "on maternity leave", "don't know", "On apprenticeship", "Retired",
                           "Full-time student", "Temporarily laid off/short term working"
  )

  stopifnot("Original categories vectors contains some unexpected labels" =
              all(original %in% validOrigCategories)
  )


  stopifnot(
    "level value not valid (should be 1, 2, 3 or 4)" =
      level %in% 1:4
  )

  stopifnot(
    "level input not valid (should be a single integer)" =
      length(level) == 1
  )



  if (level == 1) {
    origRecode <- econActGroups |> dplyr::select(original, recoded = level_1_broadest)
  } else if (level == 2) {
    origRecode <- econActGroups |> dplyr::select(original, recoded = level_2_meso)
  } else if (level == 3) {
    origRecode <- econActGroups |> dplyr::select(original, recoded = level_3)
  } else if (level == 4) {
    origRecode <- econActGroups |> dplyr::select(original, recoded = level_4)
  } else {
    stop("The level variable is not valid. (Should be 1, 2, 3 or 4)")
  }


  out <-
    data.frame(original = original) |>
    dplyr::left_join(
      origRecode,
      by = c("original" = "original")
    )

  if (missingAsNa) {
    out <- out |>
      dplyr::mutate(recoded = ifelse(recoded == "Missing", NA, recoded))
  }

  out |> dplyr::pull(recoded)
}

pull_next_wave_status <- function(df, recodeLevel = 3, verbose = FALSE) {

  # df must contain wave and pidp
  stopifnot(
    "df does not contain pidp and wave" =
    all(c("wave", "pidp") %in% names(df))
  )

  stopifnot(
    "recodeLevel not valid (must be 1, 2, 3 or 4)" =
    recodeLevel %in% 1:4
  )

  # We need to know how many unique waves there are in df and do a separate file
  # query for each

  unique_waves <- df$wave |> unique()

  if (verbose) {
    message("Looking for ", length(unique_waves), " unique waves")
  }

  grouped_nested_df <- df |>
    dplyr::nest_by(wave)


  get_econ_status_next_wave_and_match_pidps <- function(active_wave, pidp_data, verbose = FALSE) {

    active_wavenum <- match(active_wave, letters)
    next_wave <- letters[active_wavenum + 1]

    if (verbose) {
      message("active_wave is ", active_wave, " so next_wave is ", next_wave)
    }

    next_wave_file_location <-
      paste0(
        here::here("big_data/UKDA-6614-stata/stata/stata13_se/ukhls/"),
        next_wave, "_indresp.dta"
      )

    if (file.exists(next_wave_file_location)) {
      next_wave_all_pidp_jbstat <- read_and_slim_data(
        next_wave_file_location,
        varnames = "jbstat",
        extract_what = "labels",
        verbose = FALSE
      )
    } else {
      stop("Cannot find next wave's file. Is this the last wave?")
    }

    out <- next_wave_all_pidp_jbstat |>
      dplyr::filter(pidp %in% pidp_data) |>
      dplyr::select(pidp, next_status = value)
    out
  }

  out <- grouped_nested_df |>
    dplyr::mutate(
      next_data = purrr::map2(wave, data, get_econ_status_next_wave_and_match_pidps, verbose = verbose)
    ) |>
    # Now to unnest and recode according to preferred econ activity grouping
    dplyr::select(wave, next_data) |>
    tidyr::unnest(next_data) |>
    dplyr::ungroup(wave) |>
    dplyr::mutate(next_status = simplify_econ_status_categories(next_status, level = recodeLevel))


  out
}


#' Get individual level variables for selected waves
#'
#' @param waves character vector of valid waves
#' @param varnames the names of the variables to extract. Character vector of length k
#' @param vartypes the type of data (labels or values) to extract. Character vector of length k
#' @param econLevel the level of aggregation to perform on the jbstat variable. Defaults to 3 (7 levels)
#' @param verbose Whether to give a detailed report of progress
#'
#'
#' @return out - a widened and tidied dataset with wave, pidp, and selected variables as columns,
#' INCLUDING the state in the next wave
#' @export
#'
#' @examples
get_ind_level_vars_for_selected_waves <- function( varnames, vartypes, waves = letters[1:11], econLevel = 3, verbose = FALSE){

  stopifnot(
    "waves inputs are not valid" =
    waves %in% letters
  )

  stopifnot(
    "varlabs are not of type character" =
      is.character(varnames)
  )

  stopifnot(
    "vartypes are not of permitted type (labels or values)" =
      all(vartypes %in% c('labels', 'values'))
  )

  df <- extract_ind_from_waves(
    varnames = varnames,
    vartypes = vartypes,
    verbose = verbose,
    waves = waves
  ) |>
    smartly_widen_ind_dataframe(
      varnames = varnames, vartypes = vartypes
  )

  next_statuses <- pull_next_wave_status(df |> dplyr::select(wave, pidp))

  df <- df |>
    dplyr::mutate(this_status = simplify_econ_status_categories(jbstat)) |>
    dplyr::left_join(next_statuses, by = c("wave"="wave", "pidp" = "pidp")) |>
    dplyr::select(-jbstat)
  df
}


add_hh_variables <- function(df, varnames, vartypes, verbose = FALSE){

  stopifnot(
    "df does not contain wave and pidp" =
    "pidp" %in% names(df) & "wave" %in% names(df)
  )

  stopifnot(
    "varlabs are not of type character" =
      is.character(varnames)
  )

  stopifnot(
    "vartypes are not of permitted type (labels or values)" =
      all(vartypes %in% c('labels', 'values'))
  )

  # For each wave in df, want to extract the hidp for each pidp

  df_grouped <- df |>
    dplyr::nest_by(wave) |>
    dplyr::mutate(
      ind_file_location =
        here::here(paste0(
          "big_data/UKDA-6614-stata/stata/stata13_se/ukhls/",
          wave, "_indresp.dta"
        )
      )
    )

  pid_hid_lookups <-
    lapply(
      df_grouped$ind_file_location,
      FUN = extract_pid_with_hid_and_wave
    ) |>
    dplyr::bind_rows()


  df <- df |>
    dplyr::left_join(pid_hid_lookups, by = c("pidp", "wave"))

 # Now to add hh level variables

  active_wave_hidps <- df |>
    dplyr::select(wave, hidp) |>
    dplyr::distinct()

  # Now get the relevant hh file for each distinct wave
  grouped_active_wave_hidps <- active_wave_hidps |>
    dplyr::nest_by(wave) |>
    dplyr::mutate(
      hh_file_location = here::here(paste0(
        "big_data/UKDA-6614-stata/stata/stata13_se/ukhls/",
        wave,
        "_hhresp.dta"
      )
    )) |>
    dplyr::mutate(
      hh_long = purrr::map(
        hh_file_location,
        read_and_slim_data,
        varnames = varnames, extract_what = vartypes, level = "household"
      )
    )

  all_hh_long <- dplyr::bind_rows(grouped_active_wave_hidps$hh_long)

  # Now to make this wide and format appropriately
  all_hh_wide <- all_hh_long |>
    tidyr::pivot_wider(names_from = "variable", values_from = "value")

  for (i in seq_along(varnames)) {

    if (vartypes[i] == 'labels') {
      all_hh_wide[[varnames[[i]]]] <- haven::as_factor(all_hh_wide[[varnames[[i]]]]) |> as.character(.)
    } else if (vartypes[i] == 'values') {
      all_hh_wide[[varnames[[i]]]] <- haven::zap_labels(all_hh_wide[[varnames[[i]]]])
    } else {
      stop("Invalid vartype found (should have been caught earlier...)")
    }

  }

  all_hh_wide

  out <- df |>
    dplyr::left_join(all_hh_wide, by = c("wave", "hidp"))

  out
}
