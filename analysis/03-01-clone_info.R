library(readr)
library(here)
library(dplyr)
library(git2r)
library(purrr)
library(stringr)

repos <- readr::read_tsv(here("./data/combined_maintainer_lessons.tsv"))

repo_urls <- repos %>%
  dplyr::filter(!is.na(maintainers)) %>%
  dplyr::select(carpentry, repo_url, repo) %>%
  dplyr::mutate(repo_name = fs::path_file(repo_url),
                clone_pth = here("./data/original/github/repos/", repo_name))

# the original repo data has NA missing values
repo_urls$repo == repo_urls$repo_name

# make sure all repo names across all carpentries are unique
stopifnot(!any(duplicated(repo_urls)))

fs::dir_create(here("./data/processed/github"))
readr::write_tsv(repo_urls, here("./data/processed/github/repo_path_urls.tsv"))
