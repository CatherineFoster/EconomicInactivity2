#' Convert variable names to regex search string
#'
#' @param varnames list of variables except for pidp, and without {w}_ prefix part
#'
#' @return out : a string for passing to the matches function in the col_select parameter
#' in haven::read_dta
#' @export
#'
#' @examples
convert_varname_selection_to_regex <- function(varnames){
  stopifnot("no varnames supplied. Please supply at least one" = !is.null(varnames))
  wave_varnames_regexed <- sapply(varnames, FUN = function(x) paste0("^[a-z]{1}_", x, "$"))
  pidp_wave_varnames_regexed <- c("pidp", wave_varnames_regexed)
  out <- paste(pidp_wave_varnames_regexed, collapse = "|")
  out
}

#' Extract individual level variables and convert to long format
#'
#' @param dta a dataframe from ukhls
#' @param varnames character vector, length K: the names of the variables to extract (apart from
#' pidp which is implicit)
#' @param extract_what a vector of either length 1 or length K, each of whose elements must be one of
#' 'labels', 'values', or 'both'
#' @param verbose boolean. If true then time taken to extract is reported
#'
#' @return
#' dataframe with only selected variables, and in long format
#' @export
#'
#' @examples
extract_vars_and_make_long <- function(dta, varnames, extract_what = 'labels', verbose = TRUE){

  #dta should be a dataframe
  stopifnot("dta is not a dataframe" = "data.frame" %in% class(dta))

  #varnames should be a vector of strings of length 1 or above
  stopifnot("varnames does not have contents" = length(varnames) > 0)
  stopifnot("varnames is not of mode character" = mode(varnames) == "character")

  # extract_what should be either of length 1 or the length of varnames
  stopifnot("extract_what is not length 1 or length of varnames" =
              length(extract_what) == 1 |
              length(extract_what) == length(varnames)
  )

  # each element of extract_what should be one of 'labels', 'values' or 'both
  stopifnot("elements of extract_what are not each one of 'labels', 'values' or 'both'" =
              sapply(extract_what,
                     FUN = function(x) x %in% c("values", "labels", "both")
              ) %>%
              all()
  )

  start_time = Sys.time()
  # Want to adapt this to work with multiple varnames
  var_list <- list()


  extract_var_and_make_long <- function(dta, varname, extract_what){
    out <- dta %>%
      # hard-coded for now
      dplyr::select(pidp, matches(paste0("^[a-z]{1}_", varname, "$"))) %>%
      tidyr::pivot_longer(-pidp) %>%
      tidyr::separate_wider_delim(
        name,
        delim = "_",
        too_many = 'merge',
        names = c("wave", "variable")
      ) %>%
      dplyr::mutate(value = haven::as_factor(value, levels = extract_what) %>%
               as.character()
      )
    out
  }

  for (i  in seq_along(varnames)){
    what_to_extract = if (length(extract_what) == 1) {
      extract_what
    } else {
      extract_what[i]
    }
    var_list[[i]] <- extract_var_and_make_long(dta, varnames[i], extract_what = what_to_extract)
  }

  out <- dplyr::bind_rows(var_list)
  end_time = Sys.time()
  if (verbose){
    print(
      paste(
        "extracted", length(varnames), "variables in",
        difftime(end_time, start_time, units = "secs"),
        "seconds"
      )
    )
  }
  out
}

#' Read individual level file and extract variables
#'
#' @param file_location string. the name of the file
#' @param varnames string vector of length K. The names of the variables (except pidp which is implicit)
#' excluding the {w}_ prefix expected for most variables in ukhls
#' @param extract_what string vector of length 1 or K. Each element can be one of
#' 'values', 'labels', or 'both
#' @param verbose boolean. Whether to report which files are being ingested and how long
#'  the process is taking.
#'
#' @return
#' @export
#'
#' @examples
read_and_slim_data <- function(file_location, varnames, extract_what, verbose = TRUE){

  if (verbose) {
    print(paste("extracting file:",file_location))
    start_time = Sys.time()
  }

  varnames_regex_string <- convert_varname_selection_to_regex(varnames)

  full_data <- haven::read_dta(
    file_location,
    col_select = matches(varnames_regex_string)
  )

  if (verbose) {
    end_time = Sys.time()
    print(paste("read file in", difftime(end_time, start_time, units = "secs"), "seconds"))
    print(paste("slimming file..."))

  }

  slim_long_data <- extract_vars_and_make_long(
    full_data,
    varnames = varnames,
    extract_what = extract_what,
    verbose = verbose
  )
  slim_long_data
}





