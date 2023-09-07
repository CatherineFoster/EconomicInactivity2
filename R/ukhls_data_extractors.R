#' Convert variable names to regex search string
#'
#' @param varnames list of variables except for pidp, and without {w}_ prefix part
#'
#' @return out : a string for passing to the matches function in the col_select parameter
#' in haven::read_dta
#' @export
#'
#' @examples
convert_varname_selection_to_regex <- function(varnames, level = "individual"){
  stopifnot("level not valid. Should be either individual or household" = level %in% c("individual", "household"))
  stopifnot("no varnames supplied. Please supply at least one" = !is.null(varnames))

  if (level == "individual") {
    wave_varnames_regexed <- sapply(varnames, FUN = function(x) paste0("^[a-z]{1}_", x, "$"))
    pidp_wave_varnames_regexed <- c("pidp", wave_varnames_regexed)
    out <- paste(pidp_wave_varnames_regexed, collapse = "|")

  } else if (level == "household") {
    varnames <- c("hidp", varnames)
    wave_varnames_regexed <- sapply(varnames, FUN = function(x) paste0("^[a-z]{1}_", x, "$"))
    out <- paste(wave_varnames_regexed, collapse = "|")
  }
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
extract_vars_and_make_long <- function(dta, varnames, extract_what = 'labels', level = "individual", verbose = TRUE){

  #level should be individual or household
  stopifnot("level should be individual or household" = level %in% c("individual", "household"))

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


  extract_var_and_make_long <- function(dta, varname, extract_what, level = "individual"){

    stopifnot("level should be individual or household" = level %in% c("individual", "household"))

    if (level == "individual") {
      out <- dta %>%
        dplyr::select(pidp, matches(paste0("^[a-z]{1}_", varname, "$"))) %>%
        tidyr::pivot_longer(-pidp)
      out <- out %>%
        tidyr::separate_wider_delim(
          name,
          delim = "_",
          too_many = 'merge',
          names = c("wave", "variable")
        ) %>%
        dplyr::mutate(value = haven::as_factor(value, levels = extract_what) %>%
                        as.character()
        )

    } else if (level == "household") {
      #if household then the first variable should be something like w_hidp
      wave <- stringr::str_extract(names(dta)[1], "^[a-z]{1}")
      names(dta) <- sapply(names(dta), FUN = function(x) stringr::str_remove(x, "^[a-z]{1}_"))
      dta <- dta %>%
        dplyr::mutate(wave = wave) %>%
        dplyr::select(wave, everything())

      out <- dta %>%
        dplyr::select(wave, hidp, matches(varname)) %>%
        tidyr::pivot_longer(-c('wave', 'hidp'), names_to = "variable", values_to = "value")
    }
    out
  }

  for (i  in seq_along(varnames)){
    what_to_extract = if (length(extract_what) == 1) {
      extract_what
    } else {
      extract_what[i]
    }
    var_list[[i]] <- extract_var_and_make_long(dta, varnames[i], extract_what = what_to_extract, level = level)
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

#' Read individual and household level file and extract variables
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
read_and_slim_data <- function(file_location, varnames, extract_what, verbose = FALSE, level = "individual"){

  stopifnot("level is not individual or household" = level %in% c("individual", "household"))

  if (verbose) {
    print(paste("extracting file:",file_location))
    print(paste("Attempting to find", length(varnames), "variables"))
    start_time = Sys.time()
  }

  varnames_regex_string <- convert_varname_selection_to_regex(varnames, level = level)

  full_data <- haven::read_dta(
    file_location,
    col_select = matches(varnames_regex_string)
  )

  num_variables_found <- ncol(full_data) - 1

  if (verbose) {
    if (num_variables_found != length(varnames)) {
      print(
        paste("WARNING! Only", num_variables_found,
              "of the", length(varnames), "requested have been found"
        )
      )
    } else {
      print("All variables requested found")
    }
  }



  if (verbose) {
    end_time = Sys.time()
    print(paste("read file in", difftime(end_time, start_time, units = "secs"), "seconds"))
    print(paste("slimming file..."))

  }

  found_varnames <- names(full_data)
  if (level == "individual") {
    found_varnames <- found_varnames[found_varnames != 'pidp'] %>%
      stringr::str_remove("^[a-z]_")

  } else if (level == "household") {
    # For household data the hh variables are of the form w_hidp so the w part needs to be extracted along with
    # all others
    found_varnames <- found_varnames[!grepl("^[a-z]{1}_hidp", found_varnames)] %>%
      stringr::str_remove("^[a-z]_")

  }

  # If not all variables are found, not not all extract_what positions are relevant
  found_extract_what <- extract_what[varnames %in% found_varnames]

  slim_long_data <- extract_vars_and_make_long(
    full_data,
    varnames = found_varnames,
    extract_what = found_extract_what,
    verbose = verbose, level = level
  )
  slim_long_data
}


#' Extract equivalised monthly income and number of dependent children from hhresp file
#'
#' @param file_location The location of a household level datafile of type hhresp
#'
#' @return dta_hh. A dataframe with variables in long format and with columns named in a
#' more informative way
#' @export
#'
#' @examples
extract_eq_income_and_num_dependents <- function(file_location){
  # First want to extract only those variables of interest, using a pattern that generalises across files
  dta_hh <- haven::read_dta(
    file_location,
    col_select = c(
      ends_with('hidp'),
      ends_with('fihhmnnet1_dv'),
      ends_with('ieqmoecd_dv'),
      ends_with('nkids_dv')
    )
  )

  # Next want to extract the wave letter, then remove it from the column names
  dta_hh_colnames <- names(dta_hh)
  wave_letters <- stringr::str_extract(dta_hh_colnames, "^[a-z]{1}")
  # Now to check all wave letters are identical
  stopifnot("not all implied waves are the same" = length(unique(wave_letters)) == 1)

  wave_letter <- wave_letters[1]

  # Rename columns to exclude wave letter
  names(dta_hh) <- dta_hh_colnames %>% stringr::str_remove("^[a-z]{1}_")
  # add wave back as separate column
  dta_hh <- dta_hh %>%
    dplyr::mutate(
      wave = wave_letter
    ) %>%
    # Also tidy and rename variables
    dplyr::mutate(
      net_monthly_income = fihhmnnet1_dv,
      equivalisation_factor = ieqmoecd_dv,
      number_of_dependent_children = nkids_dv,
      equivalised_monthly_income = net_monthly_income / equivalisation_factor
    ) %>%
    dplyr::select(wave, hidp, equivalised_monthly_income, number_of_dependent_children)



  dta_hh
}


extract_pid_with_hid_and_wave <- function(file_location){

  ind_data <- haven::read_dta(
    file_location,
    col_select = c(
      "pidp",
      ends_with('hidp')
    )
  )

  # now want to know the wave prefix for a_hidp

  jj <- names(ind_data)[stringr::str_detect(names(ind_data), "_hidp$")]
  wave_letter <- stringr::str_extract(jj, "^[a-z]{1}")
  rm(jj)
  # Now want to remove wave prefix from {w}_hidp
  names_without_wave_prefix <- names(ind_data) %>% stringr::str_remove("^[a-z]{1}_")
  names(ind_data) <- names_without_wave_prefix
  ind_data <- ind_data %>%
    dplyr::mutate(wave = wave_letter)

  ind_data
}
