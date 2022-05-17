library("readr")
library("dplyr")
amy <- readr::read_csv(here::here("data", "2022-04-07-maintainers-from-amy.csv"))
manual <- readr::read_csv(here::here("data", "2022-04-07-retired-maintainers.csv"))

no_gh <- manual |>
  select(email_clean, gh_name_clean) |>
  filter(is.na(gh_name_clean) & !is.na(email_clean)) |>
  distinct()

no_nothing <- manual |>
  select(Name, email_clean, gh_name_clean) |>
  filter(is.na(gh_name_clean), is.na(email_clean)) |>
  distinct()

gh_by_email <- bind_rows(
inner_join(no_gh, amy, by = c("email_clean" = "person_email"), na_match = "never"),
inner_join(no_gh, amy, by = c("email_clean" = "person_secondary_email"), na_match = "never")
)

gh_by_name <- inner_join(no_nothing, amy, by = c("Name" = "person_name"), na_match = "never")

