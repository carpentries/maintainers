library(readr)
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(txtplot)

lesson_maintainers <- readr::read_tsv("./data/combined_maintainer_lessons.tsv")

maintainers <- dplyr::select(lesson_maintainers, carpentry, repo_url, maintainers)
maintainers

maintainers <- maintainers %>%
  dplyr::mutate(maintainers = strsplit(maintainers, ",")) %>%
  tidyr::unnest(maintainers) %>%
  dplyr::mutate(maintainers = stringr::str_trim(maintainers),
                maintainers = stringr::str_remove_all(maintainers, "\\*"),
                maintainers = stringr::str_remove_all(maintainers, "\\(looking for Maintainers\\)")
                )

maintainers %>%
  arrange(maintainers) %>%
  View()

maintainer_repo_counts <- maintainers %>%
  dplyr::count(maintainers) %>%
  dplyr::arrange(-n)

count_counts <- maintainer_repo_counts %>%
  tidyr::drop_na() %>%
  dplyr::count(n)

count_counts %>%
  ggplot(aes(x = n, y = nn)) +
    geom_bar(stat = "identity")

txtbarchart(as.factor(rep(count_counts$n, count_counts$nn)), width = 80, height = 30)

txt_data <- as.factor(rep(count_counts$n, count_counts$nn))

txtplot(count_counts$n, count_counts$nn, width = 80, height = 20)

count_counts

maintainer_repo_counts %>%
  filter(n > 1) %>%
  tidyr::drop_na() %>%
  View()

maintainers %>%
  dplyr::filter(is.na(maintainers)) %>%
  dplyr::pull(repo_url) %>%
  paste(collapse = "\n") %>%
  cat

maintainers %>%
  dplyr::filter(is.na(maintainers)) %>%
  dplyr::pull(repo_url) %>%
  length()
