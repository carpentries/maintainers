library(stringr)

clean_gh_name <- function(gh_name) {
  gh_name %>%
    stringr::str_remove_all(pattern = "@") %>%
    stringr::str_remove_all(pattern = "\\s") %>%
    stringr::str_trim() %>%
    stringr::str_to_lower()
}

stopifnot(clean_gh_name(" @chen da niely ") == "chendaniely")
