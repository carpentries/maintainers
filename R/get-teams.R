library("gh")
library("here")
library("purrr")
library("jsonlite")

# step 1: create a throwaway github token ----------------------------------------
# 
# This opens a browser window with a token that allows you to read organisation information.
source(here::here("R", "get-gh-token.R"))

# Step 2: get the repositories and use the GitHub API to get the teams from the repositories

# Get teams for curriculum
get_teams_for_repo <- function(repo) {
  gh::gh("/repos/{repo}/teams", repo = repo, .token = Sys.getenv("GITHUB_TOKEN"), .limit = Inf, per_page = 300)
}

get_teams <- function(repos) {
  purrr::map(repos, get_teams_for_repo)
}

less_src <- "https://feeds.carpentries.org/lessons.json"
lessons_json <- jsonlite::read_json(less_src)
repos <- purrr::map_chr(lessons_json, ~paste0(.x$carpentries_org, "/", .x$repo))
teams <- get_teams(repos)
names(teams) <- repos

out <- here::here("data", "teams.json")
message("writing teams data to", out)
jsonlite::write_json(teams, out)
rm(teams)

