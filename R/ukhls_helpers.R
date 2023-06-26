return_labels_as_factors <- function(dta){
  stopifnot("Input is not dataframe" = is.data.frame(dta))

  out <- dta %>%
    dplyr::mutate_if(
      function(x) "haven_labelled" %in% class(x),
      ~ haven::as_factor(., levels = "both")  %>% as.character
    )
  out
}

get_col_labels <- function(dta) {
  stopifnot("Input is not dataframe" = is.data.frame(dta))

  out <- sapply(dta, function(x) attr(x, 'label'))
  out
}

get_labels_for_col <- function(x) {
  # print(x)
  # print(haven::print_labels(x))

  attr(x, "labels")
}
