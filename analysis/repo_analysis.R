library(fs)
library(glue)
library(here)
library(git2r)
library(purrr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tibble)

repo_path_urls <- readr::read_tsv(here("./data/processed/github/repo_path_urls.tsv"))

nrow(repo_path_urls)

system(glue('du -hs {here("./data/original/github/repos")}'))


## data processing -----

repo_contributions <- function(pth, carpentry, repo_name, breaks = "year") {
  git2r::contributions(pth, breaks = breaks, by = "commits") %>%
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

# showing all the cumulative commits for each year.
# only looking at official repos, so highly active incupator/lab repos are not listed
# community is currently in maintenance mode
ggplot(data = contributions_all, aes(x = when, y = n)) +
  geom_line(aes(color = repo)) +
  facet_wrap(~ carpentry, ncol = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Number of commits in each year, by carpentry and lesosn",
       x = "Year",
       y = "Cumulative commits") +
  NULL

# looking at commits by week
contributions_week <- purrr::pmap_df(list(pth = repo_path_urls$clone_pth,
                                         carpentry = repo_path_urls$carpentry,
                                         repo_name = repo_path_urls$repo_name),
                                    repo_contributions,
                                    breaks = "week")

# how many weeks from the start
first_day <- contributions_week %>%
  dplyr::mutate(when = lubridate::ymd(when)) %>%
  dplyr::arrange(when) %>%
  dplyr::group_by(repo) %>%
  dplyr::slice(1) %>%
  dplyr::select(when, carpentry, repo) %>%
  dplyr::rename(first_day = when)

contributions_week <- contributions_week %>%
  dplyr::left_join(first_day, by = c("carpentry", "repo")) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(age = as.numeric(when - first_day, units = "days"))


contributions_week <- contributions_week %>%
  dplyr::filter(age != 0) %>%
  dplyr::mutate(weight_log = log(age),
                weight_linear = age,
                adjusted_n_log = n * weight_log,
                adjusted_n_linear = n * weight_linear,
                year = lubridate::year(when))

## Yearly commit counts

contributions_week %>%
  dplyr::group_by(year, carpentry, repo) %>%
  dplyr::summarize(
    total_year = sum(n),
    .groups = "drop"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(year, carpentry) %>%
  ggplot(aes(x = as.factor(year), y = total_year, color = repo, group = repo)) +
  geom_line() +
  facet_wrap(~ carpentry, ncol = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  #scale_y_log10() +
  labs(title = "Total number of commits in each year",
       x = "Year",
       y = "Number of commits") +
  NULL

ggsave(here("./output/repo_analysis/yearly_commit_count.png"))

## Log adjusted yearly counts -----

adjusted_contributions_week <- contributions_week %>%
  dplyr::group_by(year, carpentry, repo) %>%
  dplyr::summarize(
    adjusted_n_log = sum(adjusted_n_log),
    adjusted_n_linear = sum(adjusted_n_linear),
    .groups = "drop"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(year, carpentry)
  
adjusted_contributions_week %>%
  ggplot(aes(x = as.factor(year), y = adjusted_n_linear, color = repo, group = repo)) +
  geom_line() +
  facet_wrap(~ carpentry, ncol = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  #scale_y_log10() +
  labs("Number of commits in each year (linear adjustment)",
       x = "Year",
       y = "Linear adjusted commit counts") +
  NULL

ggsave(here("./output/repo_analysis/yearly_commit_count_linear.png"))

## Weekly commits -----

contributions_week %>%
  ggplot(aes(x = age,
             y = n,
             group = repo,
             color = repo)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0,
                                  max(contributions_week$age),
                                  by = 365)) +
  facet_wrap(~ carpentry, ncol = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Number of commits over repo age",
       x = "Repo Age (days)",
       y = "Number of commits")

ggsave(here("./output/repo_analysis/weekly_commit_count.png"))

contributions_week %>%
  ggplot(aes(x = age,
             y = adjusted_n_linear,
             group = repo,
             color = repo)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0,
                                  max(contributions_week$age),
                                  by = 365)) +
  facet_wrap(~ carpentry, ncol = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Adjusted number of commits over repo age (linear)",
       x = "Repo Age (days)",
       y = "Linear Adjusted number of commits")

ggsave(here("./output/repo_analysis/weekly_commit_count_linear.png"))

adjusted_contributions_week %>%
  ggplot(aes(x = as.factor(year),
             y = adjusted_n_log,
             color = repo,
             group = repo)) +
  geom_line() +
  facet_wrap(~ carpentry, ncol = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  #scale_y_log10() +
  labs("Number of commits in each year (log adjustment)",
       x = "Year",
       y = "Log adjusted commit counts") +
  NULL

ggsave(here("./output/repo_analysis/yearly_commit_count_log.png"))

contributions_week %>%
  ggplot(aes(x = age,
             y = adjusted_n_log,
             group = repo,
             color = repo)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0,
                                  max(contributions_week$age),
                                  by = 365)) +
  facet_wrap(~ carpentry, ncol = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Adjusted number of commits over repo age (log)",
       x = "Repo Age (days)",
       y = "Log Adjusted number of commits")

ggsave(here("./output/repo_analysis/weekly_commit_count_log.png"))
