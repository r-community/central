readData <- function(filename) {
  data <- vroom::vroom(filename)

  unprepend_ids(rtweet::flatten(data))

}

unprepend_ids <- function(x) {
  ids <- grepl("\\_id$", names(x))
  x[ids] <- lapply(x[ids], unx_ids)
  x
}

unx_ids <- function(x) {
  if (is.recursive(x)) {
    x <- lapply(x, function(.)
      ifelse(length(.) == 0 || (length(.) == 1 && is.na(.)),
             list(NA_character_), list(gsub("x", "", .))))
    x <- lapply(x, unlist, recursive = FALSE)
  } else {
    x <- gsub("x", "", x)
  }
  x
}
