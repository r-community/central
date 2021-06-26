# library(data.table)
# library(purrr)
# library(lubridate)
# library(dplyr)

stack_data <- list.files(path = paste0("Query_Data/"), pattern = "*.csv", full.names = T) %>%
  map_df(~read.csv(.))
stack_data %>% modify_if(is.factor, as.character) -> stack_data


questions_count <- length(unique(stack_data$Id))

answers_count <- sum(stack_data$AnswerCount)

comment_count <- sum(stack_data$CommentCount)

pgviews_count <- sum(stack_data$ViewCount)

likes_count <- sum(stack_data$FavoriteCount, na.rm = T)

users_count <- length(unique(stack_data$OwnerUserId))


stdf <- data.frame(Date = stack_data$CreationDate, QId = stack_data$Id, ACount = stack_data$AnswerCount, 
                   CCount = stack_data$CommentCount, UId = stack_data$OwnerUserId, VCount = stack_data$ViewCount)
stdf <- stdf %>%
  mutate(Year = year(Date), Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(QCount = length(QId), ACount = sum(ACount), CCount = sum(CCount), 
            UCount = length(unique(UId)), VCount = sum(VCount))
stdf$Month <- month.abb[stdf$Month]


yearly_count_qac <- stdf %>%
  group_by(Year) %>%
  summarise(QCount = sum(QCount), ACount = sum(ACount), CCount = sum(CCount))

yearly_count_views <- stdf %>%
  group_by(Year) %>%
  summarise(VCount = sum(VCount))


tag_list <- strsplit(stack_data$Tags, split = "[<>]")
tag_vector <- unlist(tag_list)
tag_vector <- tag_vector[tag_vector!=""]
w <- table(tag_vector)
tag_df <- as.data.frame(w)
tag_df <- tag_df %>% arrange(desc(Freq)) 
tag_df <- head(tag_df, 21)


recent_top_ques <- data.frame(Question_Id = stack_data$Id, Title = stack_data$Title, Date = stack_data$CreationDate, 
                              Views = stack_data$ViewCount)
recent_top_ques$Date <- ymd_hms(recent_top_ques$Date)
recent_top_ques <- recent_top_ques[recent_top_ques$Date<=Sys.Date()-1 & recent_top_ques$Date>=Sys.Date()-31,]
recent_top_ques <- recent_top_ques %>% arrange(desc(Views))
recent_top_ques <- head(recent_top_ques, 50)


year_seq <- 2008:2021
year_top_ques <- data.frame(Question_Id = integer(), Title = character(), Date = numeric(), 
                            Views = integer())
i <- 1
repeat{
  
  if(i>length(year_seq)){
    break
  }
  df <- data.frame(Question_Id = stack_data$Id, Title = stack_data$Title, Date = stack_data$CreationDate, 
                   Views = stack_data$ViewCount)
  df$Date <- year(df$Date)
  df <- df[df$Date==year_seq[i],]
  df <- df %>% arrange(desc(Views))
  df <- head(df, 50)
  year_top_ques <- rbind(year_top_ques, df)
  i <- i+1
}
year_top_ques



