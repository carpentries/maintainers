library(jsonlite)
library(googlesheets4)
library(readr)
library(here)
library(dplyr)


# The official list of lessons are in https://feeds.carpentries.org and formatted as JSON.
# The official lessons are in https://feeds.carpentries.org/lessons.json
# The incubator lessons are in https://feeds.carpentries.org/community_lessons.json

lessons_raw <- jsonlite::read_json("https://feeds.carpentries.org/lessons.json")
lessons_df <- jsonify::from_json( "https://feeds.carpentries.org/lessons.json" )

# From Daniel's googld drive, this data set is not public because of GDPR
maintainers <- googledrive::drive_get("maintainer_list") %>%
  googlesheets4::read_sheet()

tags <- lessons_df %>%
  unnest(github_topics) %>%
  tidyr::pivot_wider(names_from = github_topics, names_glue = "tag-{github_topics}", values_from = github_topics)

readr::write_tsv(tags, "./data/lessons.tsv")
readr::write_tsv(maintainers, "./data/maintainers.tsv")
