

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
