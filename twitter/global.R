library(magrittr)
library(htmltools)

get_tweet_embed <- function(user, status_id, popularity) {
  url <-
    stringr::str_glue(
      "https://publish.twitter.com/oembed?url=https://twitter.com/{user}/status/{status_id}&partner=&hide_thread=false"
    )
  
  response <- httr::GET(url) %>%
    httr::content()
  
  return(toString(tagList(
    div(h4(
      paste("Popularity:", popularity), class = "text-center"
    ), style = "display: inline-block;border: 1px solid rgba(0, 0, 0, 0.2);border-radius: 1em;",
    HTML(response$html))
  )[[1]]))
}

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