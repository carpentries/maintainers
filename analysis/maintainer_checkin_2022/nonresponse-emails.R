library(tidyverse)
library(here)

source(here::here("./R/clean_email.R"))
source(here::here("./R/clean_github.R"))

maintainers_all <- readr::read_csv(here::here("./data/private_all_badged_maintainers_2022_02_01.csv"))
responses <- readr::read_csv(here::here("./data/The Carpentries Yearly Maintainer Opt-In 2022.csv"))

names(maintainers_all)
names(responses)

maintainers_all_clean <- maintainers_all %>%
  dplyr::mutate(
    gh_name_clean = purrr::map_chr(github, clean_gh_name),
    email_1_clean = purrr::map_chr(person_email, clean_email),
    email_2_clean = purrr::map_chr(person_secondary_email, clean_email),
  )

responses_clean <- responses %>%
  dplyr::mutate(
    gh_name_clean = purrr::map_chr(`What is your GitHub username`, clean_gh_name),
    email_clean = purrr::map_chr(`E-mail`, clean_email)
  )

# matching on github username seems better than email because of the double email columns
# also not every person has an email address from responses

not_yet_responded_gh <- maintainers_all_clean %>%
  dplyr::anti_join(responses_clean, by = "gh_name_clean")

email_list_1 <- c(not_yet_responded_gh$email_1_clean, not_yet_responded_gh$email_2_clean) %>%
  na.omit()

# not all maintainers have a gh username in AMY

responded_emails <- c(responses_clean$email_clean) %>%
  na.omit()

no_amy_gh <- maintainers_all_clean %>%
  dplyr::filter(is.na(gh_name_clean))

# yes you can do this much simplier, but this lets you double check the process
amy_email_no_responses <- no_amy_gh %>%
  dplyr::mutate(
    match_email_1 = email_1_clean %in% responded_emails,
    match_email_2 = email_2_clean %in% responded_emails,
    any = purrr::map2_lgl(match_email_1, match_email_2, any)
  ) %>%
  dplyr::filter(any == FALSE) # no email matches in either column

amy_email_no_responses

email_list_2 <- c(amy_email_no_responses$email_1_clean, amy_email_no_responses$email_2_clean) %>%
  na.omit()

emails_to_resend <- c(email_list_1, email_list_2)
emails_to_resend

# 2022-02-07: length(emails_to_resend) == 83

# create string to copy/paste into email client

paste(emails_to_resend, collapse = ", ")

