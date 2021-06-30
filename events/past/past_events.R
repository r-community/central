# Read dataset
past_event <- readRDS("all_past_R_events.rds")
past_event <- past_event %>%
  distinct(id, .keep_all = TRUE)


# ValueBox data
total_events <- length(past_event$id)
total_rsvp <- sum(past_event$yes_rsvp_count)
total_cities <- length(unique(past_event$group_city))
total_countries <- length(unique(past_event$venue_country_name))

# Count of events per year
all_events <- past_event
all_eventsDT <- data.table::as.data.table(all_events)
all_eventsDT[, `:=`(
  round_year = lubridate::floor_date(local_date, "year" )) ]
all_eventsDT[, `:=`(
  round_year = lubridate::year(round_year))]
alleventsdf <- data.table::setDF(all_eventsDT)
event_group_year <- alleventsdf  %>% group_by(round_year) %>% summarise(Events_Count = n())


# Attendance (Yes-RSVP) per year
event_group_rsvp_year <- alleventsdf  %>% group_by(round_year) %>% summarise(Attendees = sum(yes_rsvp_count))


#  Count of events per month.
all_events <- past_event
all_events$month <- months(all_events$local_date,abbreviate = TRUE)
all_events$month <- toupper(all_events$month)
month <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG",'SEP','OCT','NOV','DEC');
tempevent <- all_events%>%group_by(month)%>%summarise(Events_Count = n())
months.df <- data.frame(
  Events_Count = vector(mode = 'numeric', length = 12)
)
for(i in 1:12){
  cur.month <- month[i];
  months.df$Events_Count[i] = as.numeric(tempevent[tempevent$month==cur.month,2])
}
months.df$month = month


# Attendance of events per month.
tempevent_rsvp <- all_events%>%group_by(month)%>%summarise(attendees = sum(yes_rsvp_count))
months.rsvpdf <- data.frame(
  Attendees = vector(mode = 'numeric', length = 12)
)
for(i in 1:12){
  cur.month <- month[i];
  months.rsvpdf$Attendees[i] = as.numeric(tempevent_rsvp[tempevent_rsvp$month==cur.month,2])
}
months.rsvpdf$month = month


# Global R Events Distribution
eventcountry <- past_event %>% group_by(group_country) %>% summarise(values = n())


# R Events Across Regions
temp_df_region <- past_event
temp_df_region$group_region <- sapply(strsplit(temp_df_region$group_region, "/"), "[", 1)
event_by_region <- temp_df_region %>%
  group_by(group_region) %>%
  summarise(Events_freq = n())
regions <- c("Africa", "Latin America", "Asia", "Australia", "US/Canada", "Europe")
value1 <- c(event_by_region$Events_freq[1], event_by_region$Events_freq[2], event_by_region$Events_freq[3], 
            event_by_region$Events_freq[4]+event_by_region$Events_freq[7], event_by_region$Events_freq[5]+event_by_region$Events_freq[8], 
            event_by_region$Events_freq[6])
region_df <- data.frame(group_region=regions, Events_Count=value1)


# R Event Attendance Across regions by rsvp
attendee_event_by_region <- temp_df_region %>%
  group_by(group_region) %>%
  summarise(attendees = sum(yes_rsvp_count))
value2 <- c(attendee_event_by_region$attendees[1], attendee_event_by_region$attendees[2], attendee_event_by_region$attendees[3], 
            attendee_event_by_region$attendees[4]+attendee_event_by_region$attendees[7], attendee_event_by_region$attendees[5]+attendee_event_by_region$attendees[8], 
            attendee_event_by_region$attendees[6])
region_df_rsvp <- data.frame(group_region=regions, Attendees=value2)

# Top 40 destinations for R events
top_dest_city <- past_event %>%
  group_by(group_city) %>%
  summarise(Events_freq = n())
top_dest_city <- top_dest_city[order(top_dest_city$Events_freq, decreasing = TRUE), ]
top_dest_city <- top_dest_city[complete.cases(top_dest_city), ]
top_dest_city <- head(top_dest_city, 40)


# Cumulative growth of R events
past_event <- arrange(past_event, time)

cumm_event_count <- past_event %>%
  group_by(date(time)) %>%
  summarise(Events = n())
cumm_event_count <- cumm_event_count %>% 
  mutate(csum = cumsum(Events))
colnames(cumm_event_count)[1] <- "Event_date"

# Top Destinations for R events | Event count per city per region 
name <- event_by_region$group_region

df1 <- temp_df_region[temp_df_region$group_region==name[1],]
df1 <- df1 %>%
  group_by(group_city) %>%
  summarise(Events_freq=n())
df1 <- df1[complete.cases(df1), ]
df1 <- df1[order(df1$Events_freq, decreasing = FALSE), ]

df2 <- temp_df_region[temp_df_region$group_region==name[2],]
df2 <- df2 %>%
  group_by(group_city) %>%
  summarise(Events_freq=n())
df2 <- df2[complete.cases(df2), ]
df2 <- df2[order(df2$Events_freq, decreasing = FALSE), ]

df3 <- temp_df_region[temp_df_region$group_region==name[3],]
df3 <- df3 %>%
  group_by(group_city) %>%
  summarise(Events_freq=n())
df3 <- df3[complete.cases(df3), ]
df3 <- df3[order(df3$Events_freq, decreasing = FALSE), ]

df4 <- temp_df_region[temp_df_region$group_region==name[4] | temp_df_region$group_region==name[7],]
df4 <- df4 %>%
  group_by(group_city) %>%
  summarise(Events_freq=n())
df4 <- df4[complete.cases(df4), ]
df4 <- df4[order(df4$Events_freq, decreasing = FALSE), ]

df5 <- temp_df_region[temp_df_region$group_region==name[5] | temp_df_region$group_region==name[8],]
df5 <- df5 %>%
  group_by(group_city) %>%
  summarise(Events_freq=n())
df5 <- df5[complete.cases(df5), ]
df5 <- df5[order(df5$Events_freq, decreasing = FALSE), ]

df6 <- temp_df_region[temp_df_region$group_region==name[6],]
df6 <- df6 %>%
  group_by(group_city) %>%
  summarise(Events_freq=n())
df6 <- df6[complete.cases(df6), ]
df6 <- df6[order(df6$Events_freq, decreasing = FALSE), ]


# Top Destinations for Event Attendance by rsvp by country
temp_df_country <- past_event
temp_df_country$venue_country_name <- countrycode(temp_df_country$group_country, "iso2c", "country.name")
td_by_country <- temp_df_country %>%
  group_by(venue_country_name) %>%
  summarise(Attendees = sum(yes_rsvp_count))
td_by_country <- td_by_country[order(td_by_country$Attendees, decreasing = FALSE), ]
top_dest_country <- tail(td_by_country, 20)


# Events by Country
events_by_country <- temp_df_country %>%
  group_by(venue_country_name) %>%
  summarise(Events_Frequency = n())
events_by_country <- events_by_country[order(events_by_country$Events_Frequency, decreasing = FALSE), ]
events_by_country <- tail(events_by_country, 20)


# Data-table
past_event$venue_country_name <- countrycode(past_event$group_country, "iso2c", "country.name")
past_event <- past_event %>%
  arrange(desc(local_date))
display_df <- data.frame(Event_name = past_event$name, Date=past_event$created, City=past_event$group_city, 
                         Country=past_event$venue_country_name, RSVP_count=past_event$yes_rsvp_count, 
                         Link = past_event$link)

