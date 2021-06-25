# Scrape Project data from GSoC
library(jsonlite)
library(dplyr)
library(purrr)

current_data <- vroom::vroom("data/gsoc.tsv", col_types = c(Year = "i"))

projects_url <-
  "https://summerofcode.withgoogle.com/api/program/current/project/?organization=5111129224773632"

project_list <- fromJSON(projects_url)$results

clean_data <- project_list %>%
  as_tibble() %>%
  select(program_year,
         title,
         subcategory,
         student,
         assignee_display_names) %>%
  mutate(
    student = student$display_name,
    mentors = map_chr(assignee_display_names, ~ paste(.x, collapse = ", "))
  ) %>%
  select(program_year, title, subcategory, student, mentors) %>%
  mutate(
    subcategory = case_when(
      stringr::str_detect(subcategory, "package") ~ "Package",
      subcategory %in% c("data cleaning", "statistics", "finance") ~ "Data",
      TRUE ~ stringr::str_to_title(subcategory)
    )
  ) %>%
  setNames(c("Year", "Project", "WorkProduct", "Student", "Mentors")) %>%
  bind_rows(current_data) %>%
  arrange(Year)

vroom::vroom_write(clean_data, "data/gsoc.tsv")
