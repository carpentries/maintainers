library("gh")
library("here")
library("cli")
library("jsonlite")

options(modify.maintainer.permissions = FALSE)
on.exit(Sys.setenv(GITHUB_TOKEN = NULL))
if (file.exists(here::here("data", "teams.json"))) {
  source(here::here("R", "get-gh-token.R"))
} else {
  source(here::here("R", "get-teams.R"))
}

fix_repo_names <- function(repos) {
  for (i in seq(repos)) {
    repo <- repos[i]
    if (is.na(repo)) next
    repos[i] <- tolower(if (endsWith(repo, "/")) repo else paste0(repo, "/"))
  }
  repos
}

get_maintainer_team <- function(teams) {
  team_names <- purrr::keep(teams, ~grepl("maintainer", .x$slug[[1]]))
  if (length(team_names)) team_names[[1]] else NULL
}

username_in_team <- function(member, team) {
  url <- paste0(team$url[[1]], "/members/", member)
  res <- tryCatch(gh::gh(url),
    github_error = function(e) {
      if (inherits(e, "http_error_404")) {
        # when we get a 404 here, that means the user does not exist
        return(FALSE)
      } else {
        stop(e)
      }
    }
  )
  if (length(res)) return(res) else return(TRUE)
}

remove_username_from_teams <- function(username, repo, teams) {
  danger_scope <- grepl("admin:org", gh::gh_whoami()$scopes)
  for (team in teams) {
    team_slug <- team$slug[[1]]
    if (username_in_team(username, team)) {
      cli_alert_success("SUCCESS! {.strong {username}} is a member of {.strong {team_slug}}")
      if (danger_scope) {
        org <- strsplit(repo, "/")[[1]][1]
        deeleet <- "DELETE /orgs/{org}/teams/{team_slug}/memberships/{username}"
        cli_alert_info("Running {.code DELETE /orgs/{org}/teams/{team_slug}/memberships/{username}}")
        gh::gh(deeleet, org = org, team_slug = team_slug, username = username)
      }
    } else {
      cli_alert_danger("{.emph (屮ﾟДﾟ)屮 {.strong {username}} is not a member of {.strong {team_slug}}}")
    }
  }
}

teams <- jsonlite::read_json(here::here("data", "teams.json"))
names(teams) <- fix_repo_names(names(teams))
maintainer_teams <- purrr::map(teams, get_maintainer_team) 
retirement <- read.csv(here::here("analysis", "maintainer_checkin_2022", "2022-03-21-retiring_maintainers-clean.csv"))
user_repo <- retirement[c("gh_name_clean", "gh_text")]
user_repo$gh_text <- fix_repo_names(user_repo$gh_text)



for (i in seq(nrow(user_repo))) {
  this_repo <- user_repo$gh_text[i]
  this_user <- user_repo$gh_name_clean[i]
  if (this_repo %in% names(teams)) {
    remove_username_from_teams(this_user, this_repo, teams[[this_repo]])
  } else {
    cli_alert_warning(sprintf("%s is not a lesson repository", this_repo))
  }
}

