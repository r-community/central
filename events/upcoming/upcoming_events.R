# Read dataset
upcoming_event <- readRDS("Data/upcoming_revents.rds")
upcoming_event$venue_country_name <- countrycode(upcoming_event$group_country, "iso2c", "country.name")

# ValueBox data
total_events <- length(upcoming_event$id)
total_rsvp <- sum(upcoming_event$yes_rsvp_count)
total_cities <- length(unique(upcoming_event$venue_city))
total_countries <- length(unique(upcoming_event$venue_country_name))


# R Events by Country
events_by_country <- upcoming_event %>%
  group_by(venue_country_name) %>%
  summarise(Events_freq = n())
events_by_country <- events_by_country[order(events_by_country$Events_freq, decreasing = FALSE), ]
events_by_country <- tail(events_by_country, 25)


# R Events Across Regions
temp_df_region <- upcoming_event
temp_df_region$group_region <- sapply(strsplit(temp_df_region$group_region, "/"), "[", 1)
event_by_region <- temp_df_region %>%
  group_by(group_region) %>%
  summarise(Events_freq = n())
regions <- c("Africa", "Latin America", "Asia", "Australia", "US/Canada", "Europe")
value1 <- c(event_by_region$Events_freq[1], event_by_region$Events_freq[2], event_by_region$Events_freq[3], 
            event_by_region$Events_freq[4]+event_by_region$Events_freq[7], event_by_region$Events_freq[5]+event_by_region$Events_freq[8], 
            event_by_region$Events_freq[6])
region_df <- data.frame(group_region=regions, Events_freq=value1)


# Top R Events by attendance
top_events <- upcoming_event %>%
  group_by(name) %>%
  summarise(attendees = sum(yes_rsvp_count))
top_events <- top_events[order(top_events$attendees, decreasing = TRUE), ]


# Event Type - Online vs In-person 
event_type <- upcoming_event %>%
  group_by(venue_name=="Online event") %>%
  summarise(Event_Count = n())
event_type <- event_type[complete.cases(event_type), ]
event_type <- data.frame(EventType=c("In-person", "Online"), Count=event_type$Event_Count)


# Data-table
upcoming_event <- upcoming_event %>%
  arrange(local_date)
display_df <- data.frame(Event_name = upcoming_event$name, Date=upcoming_event$time, 
                         Country=upcoming_event$venue_country_name, Venue_Name=upcoming_event$venue_name, 
                         RSVP_count=upcoming_event$yes_rsvp_count)
