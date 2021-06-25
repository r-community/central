plot_top_destination <- function(data, region_name, top_n = 20) {
  top_cities <- data %>%
    dplyr::filter(region == region_name) %>%
    dplyr::slice_max(users, n = top_n, with_ties = FALSE) %>%
    dplyr::mutate(city = stringr::str_wrap(city, 9))

  subtitle_name <- paste("Top", top_n, "Cities")

  top_cities %>%
    dplyr::arrange(users) %>%
    echarts4r::e_charts(city) %>%
    echarts4r::e_bar(users, name = "Number of R Users", legend = FALSE) %>%
    echarts4r::e_tooltip(trigger = "axis") %>%
    echarts4r::e_x_axis(
      type = "category",
      boundaryGap = TRUE,
      axisLabel = list(interval = 0, fontSize = 10)
    ) %>%
    echarts4r::e_flip_coords() %>%
    echarts4r::e_title(
      region_name,
      subtitle_name,
    ) %>%
    echarts4r::e_color("#80bbd6") %>%
    echarts4r::e_group("top_regions")
}
