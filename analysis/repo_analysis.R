library(fs)
library(glue)
library(here)
library(git2r)
library(purrr)
library(ggplot2)

repo_path_urls <- readr::read_tsv(here("./data/processed/github/repo_path_urls.tsv"))

nrow(repo_path_urls)

system(glue('du -hs {here("./data/original/github/repos")}'))

repo_contributions <- function(pth, carpentry, repo_name) {
  git2r::contributions(pth, breaks = "year", by = "commits") %>%
    dplyr::mutate(carpentry = carpentry,
                  repo = repo_name)
}

repo_contributions(pth = repo_path_urls$clone_pth[1],
                   carpentry = repo_path_urls$carpentry[1],
                   repo_name = repo_path_urls$repo_name[1])

contributions_all <- purrr::pmap_df(list(pth = repo_path_urls$clone_pth,
                    carpentry = repo_path_urls$carpentry,
                    repo_name = repo_path_urls$repo_name),
               repo_contributions)

ggplot(data = contributions_all, aes(x = when, y = n)) +
  geom_line(aes(color = repo)) +
  facet_wrap(~ carpentry, ncol = 1) +
  theme_minimal()
