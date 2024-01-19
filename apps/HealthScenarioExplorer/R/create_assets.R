devtools::load_all(here::here("R"))

base_dir_location <- "big_data/UKDA-6614-stata/stata/stata13_se/ukhls"
library(tidyverse)
library(nnet)
library(knitr)
library(kableExtra)
library(DiagrammeR)

varnames <-  c(
  "jbstat", "dvage", "sex", "sf12mcs_dv", "sf12pcs_dv"
)

extract_what <- c(
  "labels", "values", "labels", "values", "values"
)

ind_data <- get_ind_level_vars_for_selected_waves(
  varnames = varnames, vartypes = extract_what
)

ind_data_standardised <-
  ind_data |>
  rename(age = dvage) |>
  mutate(across(c(age, sf12mcs_dv, sf12pcs_dv), function(x) ifelse(x < 0, NA, x))) %>%
  mutate(across(c(sf12mcs_dv, sf12pcs_dv), standardise_scores)) |>
  filter(between(age, 16, 64)) %>%
  filter(complete.cases(.))

mod_phmh <-
  nnet::multinom(
    next_status ~ this_status * sex + splines::bs(age, 5) + sf12pcs_dv*sf12mcs_dv,
    data = ind_data_standardised,
    maxit = 200
  )

saveRDS(mod_phmh, here::here("apps", "HealthScenarioExplorer", "assets", "mod_phmh.rds"))
saveRDS(ind_data_standardised, here::here("apps", "HealthScenarioExplorer", "assets", "ind_data_standardised.rds"))
