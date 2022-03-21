library(tidyverse)
library(here)
library(ggplot2)

source(here::here("./R/clean_email.R"))
source(here::here("./R/clean_github.R"))

clean_gh_repo_responses <- function(repo_text) {
  if (is.na(repo_text)) {
    return(NA_character_)
    
  } else if (repo_text == "Ecology Spreadsheet lessons") {
    return("datacarpentry/spreadsheet-ecology-lesson")
    
  } else if (repo_text %in% c("Library Carpentry SQL", "LibraryCarpentry/lc-sql: https://github.com/LibraryCarpentry/lc-sql")) {
    return("LibraryCarpentry/lc-sql")
    
  } else if (repo_text == "Git") {
    return("swcarpentry/git-novice")

  } else if (stringr::str_to_lower(repo_text) == "n/a") {
    return(NA_character_)
  }
  
  clean_splits <- stringr::str_replace_all(repo_text, "( )|(\\\n)|(,)", ",")
  slugs <- stringr::str_remove_all(clean_splits, "https\\:\\/\\/github\\.com\\/")
  
  fix_slugs <- slugs %>%
    stringr::str_replace_all("https\\:\\/\\/librarycarpentry\\.org", "LibraryCarpentry")
  
  return(fix_slugs)
}

responses <- readr::read_csv(here::here("./data/The Carpentries Yearly Maintainer Opt-In 2022.csv"))

responses_clean <- responses %>%
  dplyr::mutate(
    gh_name_clean = purrr::map_chr(`What is your GitHub username`, clean_gh_name),
    email_clean = purrr::map_chr(`E-mail`, clean_email),
    num_gh_repos = 1 + stringr::str_count(`Link(s) to GitHub URL(s) of maintained lesson(s) (comma or space separate multiple URLs)`, "\\s|,"),
    gh_text = purrr::map_chr(`Link(s) to GitHub URL(s) of maintained lesson(s) (comma or space separate multiple URLs)`, clean_gh_repo_responses)
  )

ggplot(responses_clean,
       aes(x = `Do you want to continue staying on as a Maintainer for your lesson(s)`)) +
  geom_bar() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  coord_flip() +
  theme_minimal()

responses_clean$gh_text



  
repos <- responses_clean %>%
  tidyr::separate_rows(gh_text, sep = ",")

repos %>%
  count(`Do you want to continue staying on as a Maintainer for your lesson(s)`)

repos %>%
  count(
    `Do you want to continue staying on as a Maintainer for your lesson(s)`,
    gh_text,
    sort = TRUE
  )


repos %>%
  filter(
    stringr::str_starts(`Do you want to continue staying on as a Maintainer for your lesson(s)`, "No")
  ) %>%
  count(
    `Do you want to continue staying on as a Maintainer for your lesson(s)`,
    gh_text,
    sort = TRUE
  )

# at least 11 maintainers for 11 lessons

repos %>%
  filter(
    stringr::str_detect(`Do you want to continue staying on as a Maintainer for your lesson(s)`, "soft transition")
  ) %>%
  count(
    `Do you want to continue staying on as a Maintainer for your lesson(s)`,
    gh_text,
    sort = TRUE
  )

# 6 more for soft transition
