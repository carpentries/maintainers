library(stringr)

clean_email <- function(email) {
  email %>%
    stringr::str_trim() %>%
    stringr::str_to_lower()
}
