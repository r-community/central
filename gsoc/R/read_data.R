read_gsoc <- function() {
  gsoc_tsv <- "data/gsoc.tsv"

  vroom::vroom(gsoc_tsv)
}

read_summary <- function() {
  gsoc_summary <- "data/gsoc_summary.json"

  jsonlite::fromJSON(gsoc_summary)
}
