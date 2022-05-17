library("gh")
library("here")
library("dplyr")
library("purrr")
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

get_org_names <- function(repos) {
  purrr::map_chr(repos, ~strsplit(.x, "/")[[1]][1])
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

remove_username_from_team <- function(username, team, delete = FALSE) {
  org <- team$org
  team_slug <- team$slug[[1]]
  if (username_in_team(username, team)) {
    cli_alert_success("{.strong {username}} is a member of {.strong {team_slug}}")
    if (delete) {
      deeleet <- "DELETE /orgs/{org}/teams/{team_slug}/memberships/{username}"
      cli_alert_info("Running DELETE /orgs/{org}/teams/{team_slug}/memberships/{username}")
      gh::gh(deeleet, org = org, team_slug = team_slug, username = username)
    }
    status <- "removed"
  } else {
    noh <- "{.emph {.strong {username}} is not a member of {.strong {team_slug}}}"
    cli_alert_danger(noh)
    status <- NA
  }
  return(list(username = username, team_slug = team_slug, team_org = org, 
    team_status = status))
}

remove_username_from_all_teams <- function(username, teams, delete = FALSE) {
  res <- purrr::map(teams, ~remove_username_from_team(username, .x, delete = delete))
  dplyr::bind_rows(res[lengths(res) > 0])
}


# read in team data from stored json file and filter into a list with all teams
# that are not bots, the core team, staff teams, or curriculum advisors
teams <- jsonlite::read_json(here::here("data", "teams.json"))
names(teams) <- fix_repo_names(names(teams))
all_orgs <- rep(get_org_names(names(teams)), lengths(teams))
all_teams <- purrr::flatten(teams) |> 
  purrr::map2(all_orgs, function(x, y) modifyList(x, list(org = y)))
names(all_teams) <- purrr::map_chr(all_teams, list("slug", 1))
team_filter <- !grepl("bots|core|staff|curriculum-advis", names(all_teams)) &
  !duplicated(names(all_teams))
all_teams <- all_teams[team_filter]
stopifnot(
  "core-team is in teams" = !"core-team" %in% names(all_teams),
  "curriculum advisors is in teams" = !grepl("curriculum-advis", names(all_teams)),
  "bots is in teams" = !grepl("bots", names(all_teams)),
  "staff is in teams" = !grepl("staff", names(all_teams))
)

retirement <- readr::read_csv(here::here("data", "2022-inactive.csv"))

retiring_maintainers <- dplyr::filter(retirement, 
  status == "Unresponsive" & !is.na(github)) |>
  dplyr::distinct(github)

# Check each github name of officially retiring maintainers against 
status <- purrr::map_dfr(retiring_maintainers$github,
  ~remove_username_from_all_teams(.x, all_teams, delete = FALSE))

# redo scope
options(modify.maintainer.permissions = TRUE)
on.exit(Sys.setenv(GITHUB_TOKEN = NULL))
source(here::here("R", "get-gh-token.R"))

danger_scope <- grepl("admin:org", gh::gh_whoami()$scopes)
final_status <- status |>
 dplyr::filter(!is.na(team_status)) |>
 mutate(team = all_teams[team_slug]) |>
 dplyr::rowwise() |>
 dplyr::summarise(res = data.frame(
   remove_username_from_team(username = username, team = team, delete = danger_scope))) |>
 tidyr::unnest(cols = res)

# bring it all into a spreadsheet. 
# NOTE: there is duplication because the `gh_text` feild was the repository, but
# we needed to remove them from teams, which is what we did here.
#


