#' Return labels as factors
#'
#' If the dataframe contains haven labelled variables, this turns all those
#' columns that are haven labelled into more straightforward characters
#' @param dta
#' The dataset containing some columns which are haven labelled
#' @return
#' @export
#'
#' @examples
return_labels_as_factors <- function(dta){
  stopifnot("Input is not dataframe" = is.data.frame(dta))

  out <- dta %>%
    dplyr::mutate_if(
      function(x) "haven_labelled" %in% class(x),
      ~ haven::as_factor(., levels = "both")  %>% as.character
    )
  out
}

#' Get full column labels
#' If the data are
#' @param dta
#' A dataframe containing some columns which are haven_labelled
#' @return out
#' A vector with the column labels
#' @export
#'
#' @examples
get_col_labels <- function(dta) {
  stopifnot("Input is not dataframe" = is.data.frame(dta))

  out <- sapply(dta, function(x) attr(x, 'label'))
  out
}

#' get labels for column
#'
#' @param x
#' a haven_labelled column passed as a simple vector
#' @return
#' an object showing the value and labels lookup for the haven_labelled column
#' @export
#'
#' @examples
get_labels_for_col <- function(x) {
  # print(x)
  # print(haven::print_labels(x))

  attr(x, "labels")
}


#' Standardise scores
#'
#' @param x A vector of type numeric/double
#'
#' @return out: A vector of the same length as x, but with scores standardised
#' @export
#'
#' @examples
standardise_scores <- function(x){
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  z <- (x - mean_x) / sd_x
  z
}



