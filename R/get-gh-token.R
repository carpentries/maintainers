library("clipr")
library("askpass")

# step 1: create a throwaway github token ----------------------------------------
# 
# This opens a browser window with a token that allows you to read organisation information.
damage <- getOption("modify.maintainer.permissions", default = FALSE)
if (damage) {
  message("CAUTION: This token has write access to any organisations you have control of.")
}
scopes <- c("public_repo", "read:org", if (damage) "write:org" else NULL)
desc <- if (damage) "DANGER%20Write%20" else "Read%20"
url <- "https://github.com/settings/tokens/new?scopes=%s&description=%sCarpentries%%20Data"
browseURL(sprintf(url, paste(scopes, collapse = ","), desc))

## Step 2: save the token for use here ---------------------------------------------
#You should copy and paste the token in your prompt after running this command. 
Sys.setenv("GITHUB_TOKEN" = askpass::askpass("Copy and past your Github PAT:\n"))
message("clearing the token from your clipboard")
clipr::write_clip("[deleted]")

