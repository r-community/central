#' Get number of unique occurrences tweet based on a column
#'
#' This is useful for creating value boxes.
#'
#' @param data
#' @param col
#'
#' @return
#' @export
#'
#' @examples
get_unique_value <- function(data, col) {
  col <- dplyr::enquo(col)
  data %>%
    dplyr::pull(!!col) %>%
    unique() %>%
    length()
}


#' Get code for embedding a tweet
#'
#' @param user User screename
#' @param status_id Tweet Status ID
#'
#' @return
#' @export
#'
#' @examples
get_tweet_embed <- function(user, status_id) {
  url <-
    stringr::str_glue(
      "https://publish.twitter.com/oembed?url=https://twitter.com/{user}/status/{status_id}&partner=&hide_thread=false"
    )


  response <- httr::GET(url) %>%
    httr::content()

  return(shiny::HTML(response$html))
}

#' Use GitHub Action to Update data/dashboard
#'
#' @return
#' @export
#'
#' @examples
use_gh_action <- function() {
  if (fs::dir_exists("./.github/workflows/") == FALSE) {
    fs::dir_create("./.github/workflows/", recurse = TRUE)
  }
  fs::file_copy("./R/gh_action.yml",
                "./.github/workflows/update.yml",
                overwrite = TRUE)
}

#' Title
#'
#' @param twitter_dataset Twitter dataset as read by the readData() function,
#' with the following columns: created_at, favorite_count and retweet_count
#'
#' @return A summarised by day dataframe
#' @export
#'
#' @examples
make_by_day_metrics <- function(twitter_dataset) {
  try(Sys.setlocale("LC_TIME", "C"), silent = TRUE)

  tweets_by_day <- twitter_dataset %>%
    rtweet::ts_data(by = "days") %>%
    dplyr::mutate(
      weekday = lubridate::wday(time, label = TRUE, abbr = TRUE),
      myear = format(time, format = "%b %Y"),
      week = format(time, format = "%U")
    ) %>%
    dplyr::rename(tweets = n)

  likes_by_day <- twitter_dataset %>%
    dplyr::select(created_at, favorite_count) %>%
    dplyr::group_by(lubridate::floor_date(created_at, "day")) %>%
    dplyr::summarise(n = sum(favorite_count, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    setNames(c("time", "likes")) %>%
    dplyr::filter(!is.na(time))

  rts_by_day <- twitter_dataset %>%
    dplyr::select(created_at, retweet_count) %>%
    dplyr::group_by(lubridate::floor_date(created_at, "day")) %>%
    dplyr::summarise(n = sum(retweet_count, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    setNames(c("time", "retweets")) %>%
    dplyr::filter(!is.na(time))

  tweets_by_day %>%
    dplyr::left_join(rts_by_day, by = "time") %>%
    dplyr::left_join(likes_by_day, by = "time") %>%
    tidyr::replace_na(replace = list(likes = 0, retweets = 0))
}
