make_user_reactable <- function(data) {
  data %>%
    reactable(
      .,
      pagination = TRUE,
      showPageSizeOptions = TRUE,
      highlight = TRUE,
      defaultSorted = "posts_read",
      defaultColDef = colDef(headerClass = "header", align = "left", minWidth = 50),
      columns = list(
        username = colDef(
          name = "User",
          defaultSortOrder = "desc",
          filterable = TRUE,
          html = TRUE
        ),
        title = colDef(
          name = "Title",
          filterable = TRUE,
          na = "–"
        ),
        likes_received = colDef(
          name = "Likes Received",
          defaultSortOrder = "desc",
          cell = function(value) {
            build_bar_col(value, data$likes_received, "#fc5185")
          }
        ),
        posts_read = colDef(
          name = "Posts Read",
          defaultSortOrder = "desc",
          cell = function(value) {
            build_bar_col(value, data$posts_read, "#3fc1c9")
          },
        ),
        post_count = colDef(
          name = "Post Count",
          defaultSortOrder = "desc",
          cell = function(value) {
            build_bar_col(value, data$post_count, "#fc5185")
          },
        ),
        days_visited = colDef(
          name = "Days Visited",
          defaultSortOrder = "desc",
          cell = function(value) {
            build_bar_col(value, data$days_visited, "#3fc1c9")
          },
        ),
        topics_entered = colDef(
          name = "Topics Entered",
          defaultSortOrder = "desc",
          cell = function(value) {
            build_bar_col(value, data$topics_entered, "#fc5185")
          },
        ),
        id = colDef(
          show = FALSE
        ),
        user_avatar = colDef(
          show = FALSE
        ),
        profile_url = colDef(
          show = FALSE
        ),
        profile = colDef(
          show = FALSE
        )
      ),
      compact = TRUE,
      class = "topusers-tbl"
    )
}

build_bar_col <- function(value, max_col, color) {
  width <-
    paste0(value * 100 / max(max_col), "%")

  value <- format(value, big.mark = ",")
  value <- format(value, width = 9, justify = "right")

  bar <- div(
    class = "bar-chart",
    style = list(marginRight = "6px"),
    div(
      class = "bar",
      style = list(width = width, backgroundColor = color)
    )
  )
  div(class = "bar-cell", span(class = "number", value), bar)
}
