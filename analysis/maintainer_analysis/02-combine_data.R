library(readr)
library(here)
library(dplyr)
library(stringr)
library(purrr)

std_repo_url <- function(url) {
  if (is.na(url)) {
    return(url)
  }
  
  url <- stringr::str_to_lower(url)
  
  if (!stringr::str_ends(url, "/")) {
    url <- paste0(url, "/")
  }
  return(url)
}

lessons <- readr::read_tsv(here("./data/lessons.tsv"))
maintainers <- readr::read_tsv(here("./data/maintainers.tsv"))

lessons <- lessons %>%
  dplyr::mutate(repo_url = purrr::map_chr(repo_url, std_repo_url))

maintainers <- maintainers %>%
  dplyr::mutate(repo_url = purrr::map_chr(repo_url, std_repo_url))

combined <- dplyr::full_join(maintainers, lessons, by = "repo_url")

names(combined)

readr::write_tsv(combined, here("./data/combined_maintainer_lessons.tsv"))
