get_community_stats <- function() {
  categories_data <- get_categories()

  total_users <-
    jsonlite::fromJSON(
      "https://community.rstudio.com/directory_items.json?period=all&order=posts_read"
    )[["meta"]][["total_rows_directory_items"]]

  total_tags <-
    jsonlite::fromJSON("https://community.rstudio.com/tags.json")$tags %>%
    dplyr::pull(n_distinct(id)) %>%
    length()

  list(
    total_posts = sum(categories_data$post_count),
    total_topics = sum(categories_data$topic_count),
    total_users = total_users,
    total_tags = total_tags
  )
}

get_categories <- function() {
  jsonlite::fromJSON("https://community.rstudio.com/categories.json")[[1]]$categories
}

get_top_users <- function(pages = 1, period = "all", base_url = "https://community.rstudio.com") {
  API_URL <-
    stringr::str_glue("{base_url}/directory_items.json?period={period}&order=posts_read&page=")

  n_pages <- seq(from = 0, to = pages)

  urls <- stringr::str_glue("{API_URL}{n_pages}")

  purrr::map_dfr(urls,
                 ~ jsonlite::fromJSON(.x, flatten = TRUE)$directory_items) %>%
    dplyr::select(
      id,
      user.username,
      user.name,
      user.avatar_template,
      user.title,
      likes_received,
      posts_read,
      post_count,
      days_visited,
      topics_entered
    ) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(user_avatar = paste0(
      base_url,
      stringr::str_replace_all(
        user.avatar_template,
        pattern = "\\{size\\}",
        replacement = "30"
      )
    )) %>%
    dplyr::rename_with(~ stringr::str_remove(.x, "user\\.")) %>%
    dplyr::select(-c(avatar_template)) %>%
    dplyr::mutate_all(list(~dplyr::na_if(., "")))
}

make_user_links <- function(data, base_url = "https://community.rstudio.com") {
  data %>%
    mutate(
      profile_url = str_glue("{base_url}/u/{username}"),
      username = str_glue('<a href="{profile_url}" target="_blank">{ifelse(is.na(name), username, name)}</a>'),
      profile =
        str_glue(
          '<img class="img-fluid img-circle mr-2" src="{user_avatar}" alt="{id}">'
        ),
      username = str_glue("{profile} {username}")
    ) %>%
    select(-c(name))
}
