


econActGroups <- readxl::read_excel(
  path = here::here("data-raw/economic_activities_categories.xlsx"),
  sheet = 'categories') |>
  janitor::clean_names()

usethis::use_data(econActGroups, overwrite = TRUE)

