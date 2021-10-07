#' Plot echarts timeseries of tweet volumne
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
plot_tweet_volume <- function(data) {
  this_month <- lubridate::floor_date(lubridate::today(), "month")
  earliest_day <- min(data$time)

  data %>%
    echarts4r::e_charts(time) %>%
    echarts4r::e_area(tweets, name = "Number of Tweets", stack = "grp") %>%
    echarts4r::e_area(retweets, name = "Number of Retweets", stack = "grp") %>%
    echarts4r::e_area(likes, name = "Number of Likes", stack = "grp") %>%
    echarts4r::e_x_axis(
      type = "time",
      formatter = htmlwidgets::JS(
        "function(value){
        let date = new Date(value);
        label = `${date.getDate()}-${(parseInt(date.getMonth()) + 1)}-${date.getFullYear()}`;
        return label;
      }"
      )
    ) %>%
    echarts4r::e_y_axis(
      splitArea = list(show = TRUE),
      axisPointer = list(
        show = FALSE,
        lineStyle = list(
          color = "#999999",
          width = 0.75,
          type = "dotted"
        )
      )
    ) %>%
    echarts4r::e_toolbox_feature(feature = "magicType",
                      type = list("area", "line", "bar")) %>%
    echarts4r::e_toolbox_feature("restore") %>%
    echarts4r::e_toolbox_feature(feature = "reset") %>%
    echarts4r::e_toolbox_feature("saveAsImage") %>%
    echarts4r::e_animation(duration = 1000) %>%
    echarts4r::e_tooltip(
      trigger = "axis"
    ) %>%
    echarts4r::e_datazoom(type = "slider") %>%
    echarts4r::e_zoom(dataZoomIndex = 0,
                      start = 70,
                      end = 100) %>%
    echarts4r::e_zoom(
      dataZoomIndex = 0,
      startValue = earliest_day,
      endValue = lubridate::today(),
      btn = "allTimeBtn"
    ) %>%
    echarts4r::e_zoom(
      dataZoomIndex = 0,
      startValue = lubridate::today() - 7,
      endValue = lubridate::today(),
      btn = "weekBtn"
    ) %>%
    echarts4r::e_zoom(
      dataZoomIndex = 0,
      startValue = this_month,
      endValue = lubridate::today(),
      btn = "monthBtn"
    ) %>%
    echarts4r::e_button(id = "allTimeBtn",
                        position = "top",
                        class = "btn btn-primary btn-sm",
                        "All Time") %>%
    echarts4r::e_button(id = "weekBtn",
                        position = "top",
                        class = "btn btn-primary btn-sm",
                        "This Week") %>%
    echarts4r::e_button(id = "monthBtn",
                        position = "top",
                        class = "btn btn-primary btn-sm",
                        "This Month")
}

#' Plot dataset accumulated by hour
#'
#' @param tweet_dataset Tweet dataset in the format of the rtweet package
#'
#' @return
#' @export
#'
#' @examples
plot_tweet_by_hour <- function(tweet_dataset) {
  tweet_dataset %>%
    dplyr::group_by(hour = lubridate::hour(created_at)) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    echarts4r::e_charts(hour) %>%
    echarts4r::e_area(count, name = "Tweets", legend = FALSE) %>%
    echarts4r::e_x_axis(
      min = 0,
      max = 23,
    ) %>%
    echarts4r::e_axis_labels(x = "Time of Day (UTC)", y = "Tweets") %>%
    echarts4r::e_theme("westeros") %>%
    echarts4r::e_tooltip(trigger = "axis", formatter = htmlwidgets::JS("
    function(params) {
      let title = `<strong>${params[0].value[0]}h</strong>`
      let num = `${params[0].value[1]} tweets`
      return(`${title}</br>${num}`);
    }"))
}