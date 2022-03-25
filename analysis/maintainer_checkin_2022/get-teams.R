library("gh")
library("purrr")
library("jsonlite")
library("askpass")

# step 1: create a throwaway github token ----------------------------------------
# 
# This opens a browser window with a token that allows you to read organisation information.

url <- "https://github.com/settings/tokens/new?scopes=public_repo,read:org&description='Read org data'"
browseURL(url)

## Step 2: save the token for use here ---------------------------------------------
#You should copy and paste the token in your prompt after running this command. 
Sys.setenv("GITHUB_TOKEN" = askpass::askpass("Copy and past your Github PAT:\n"))

# Step 3: get the repositories and use the GitHub API to get the teams from the repositories

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

# Step 4: ... map members to teams and remove needed members. 