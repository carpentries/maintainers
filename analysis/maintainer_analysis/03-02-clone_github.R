library(fs)
library(purrr)
library(here)
library(readr)

repo_urls <- readr::read_tsv(here("./data/processed/github/repo_path_urls.tsv"))

fs::dir_delete(here("./data/original/github/repos/"))
fs::dir_create(repo_urls$clone_pth)

# for testing
#repo_urls <- repo_urls[1:5, ]

pb <- progress_estimated(nrow(repo_urls)) # make progress bar

purrr::walk2(repo_urls$repo_url,
             repo_urls$clone_pth,
             function(x, y) {
               pb$tick()$print() # tick progress bar
               git2r::clone(url = x, local_path = y, progress = FALSE)
             })
