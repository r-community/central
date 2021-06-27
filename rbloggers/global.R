
bloglist_wbpg <- read_html("https://www.r-bloggers.com/blogs-list/")

blog_typename <- bloglist_wbpg %>%
  html_nodes(".post-16581 li a") %>%
  html_text()

contributing_blogs <- length(blog_typename)


wbpg <- read_html("https://www.r-bloggers.com/")

blog_text <- wbpg %>%
  html_nodes(xpath = "//*[@id='archives-dropdown-3']/option") %>%
  html_text() %>%
  stringr::str_squish()

blog_text <- blog_text[2:length(blog_text)]

# hyperlink extraction
blog_monthwise_hyperlink <- wbpg %>%
  html_nodes(xpath = "//*[@id='archives-dropdown-3']/option") %>%
  html_attr("value")

blog_monthwise_hyperlink <- blog_monthwise_hyperlink[2:length(blog_monthwise_hyperlink)]

# month extraction
month_names <- gsub("\\d|[[:punct:]]", " ", blog_text) %>%
  stringr::str_squish()

# year extraction
year_number <- str_extract(blog_text, "(?<=\\s)(.*)(?=\\s)")

# blogs count
Rcount <- as.numeric(gsub('.*\\((\\d+).*', '\\1', blog_text))


date <- paste(month_names, year_number, sep = ", ")

# creating data-frame for R Bloggers
Rbloggers_df <- data.frame(
  Hyperlink = blog_monthwise_hyperlink, Date = date, Month = month_names, Year = year_number, Blog_Count = Rcount
)
Rbloggers_df$Date <- factor(Rbloggers_df$Date, levels = unique(Rbloggers_df$Date))
yearly_blog_count <- tapply(Rbloggers_df$Blog_Count, Rbloggers_df$Year, FUN=sum)
yearly_df <- data.frame(Year=names(yearly_blog_count), Count=yearly_blog_count)

Rbloggers_df <- Rbloggers_df %>% arrange(desc(row_number()))


month_blogs <- function(year, month){
  
  ym_wbpg <- "https://www.r-bloggers.com/%d/%d/"
  ym_wbpg <- sprintf(ym_wbpg, year, month)
  temp_wbpg <- paste0(ym_wbpg, "page/2/")
  xpage <- read_html(temp_wbpg)
  pg_ym_max <- xpage %>%
    html_nodes(".dots+ .page-numbers") %>%
    html_text()
  pg_ym_max <- 1:as.numeric(gsub(",", "", pg_ym_max))
  remove(temp_wbpg)
  surf_wbpg <- paste0(ym_wbpg, "page/%d/")
  
  map_df(pg_ym_max, function(i){
    
    page <- read_html(sprintf(surf_wbpg, i))
    blog_title <- page %>%
      html_nodes(".loop-title a") %>%
      html_text()
    
    blog_author <- page %>%
      html_nodes(".fn") %>%
      html_text()
    
    blog_date <- page %>%
      html_nodes(".meta") %>%
      html_text()
    blog_date <- gsub(" \\|.*","",blog_date)
    
    author_blogs_hyperlink <- page %>%
      html_nodes("[class='fn']") %>%
      html_attr("href")
    
    data.frame(Title = blog_title,
               Date = blog_date,
               Author = blog_author,
               Author_Hyperlink = author_blogs_hyperlink)
  }) -> Month_Blog_Information
  Month_Blog_Information
}

# Note that year wise function is only valid from year 2009 till 2020
# Inorder to get the year before 2009 - use `month_blogs` function which will individually find monthly blogs
# For present year use `month_blogs` function too, as the year is not completed yet

# year wise function - combines full data into data-frame for that particular year as input
year_blogs <- function(year_date){
  
  map_df(1:12, function(i){
    
    month_blogs(year = year_date, month = i)
  }) -> Year_Blog_Information
  Year_Blog_Information
}

total_posts <- sum(yearly_blog_count)