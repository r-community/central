# ValueBox data
pkg_df <- CRAN_package_db()

total_pkgs <- nrow(pkg_df)

cran_pkg_authors <- pkg_df$Author
cran_pkg_authors <- gsub("\n","", cran_pkg_authors)
cran_pkg_authors <- gsub("\\[.*?\\]", "", cran_pkg_authors)
cran_pkg_authors <- gsub("\\(.*?\\)", "", cran_pkg_authors)
cran_pkg_authors <- paste(cran_pkg_authors, collapse = ", ")
cran_pkg_authors <- strsplit(cran_pkg_authors, ", ")[[1]]
cran_pkg_authors <- gsub("\\@.*?\\.", "", cran_pkg_authors)
cran_pkg_authors <- gsub("[[:punct:]].*?[[:punct:]]", "", cran_pkg_authors)
cran_pkg_authors <- str_trim(cran_pkg_authors)
total_authors <- length(unique(cran_pkg_authors))

url <- "https://cranlogs.r-pkg.org/downloads/daily/2012-01-01:"
date <- toString(Sys.Date()-2)
final_url <- paste0(url, date)
dailyPKG = fromJSON(final_url)
downloads <- dailyPKG$downloads[[1]]
total_dwnld <- sum(downloads$downloads)

total_cran_taskviews <- length(available.views())


# Charts - Yearly chart of total downloads
yearly_dwnld <- downloads %>%
  group_by(year(day)) %>%
  summarise(count = sum(downloads))
colnames(yearly_dwnld)[1] = "year"


# Reactable - Top 100 most depended-upon packages
tt <- pkg_df %>% unite("DependsUpon", c('Depends', 'Imports', 'LinkingTo', 'Suggests'), 
                       na.rm = TRUE, sep = ', ', remove = FALSE)

tt$DependsUpon <- gsub("\n"," ", tt$DependsUpon)
name <- tt$Package
depend <- tt$DependsUpon
count <- c()
i <- 1
repeat{
  if(i>nrow(pkg_df)){
    break
  }
  count[i] <- length(unlist(str_split(tt$DependsUpon[i], ", ")))
  i <- i+1
}
pkg_depend_df <- data.frame(Package = name, Count = count, Depends_Upon = depend)


# Reactable - Top 50 popular package keywords
CRAN_package_wbpg <- read_html("https://cran.r-project.org/web/packages/available_packages_by_name.html")
package_summary <- CRAN_package_wbpg %>%
  html_nodes("td+ td") %>%
  html_text()
package_summary <- gsub("[\r\n]", "", package_summary)

data <- toString(package_summary)
data <- gsub('[[:punct:] ]+',' ',data)

docs <- VCorpus(VectorSource(data))
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
keyword_df <- head(d, 50)
rownames(keyword_df) <- NULL


# Reactable - Unique count of all licenses ranked by their frequency
license_df <- pkg_df %>%
  group_by(License) %>%
  summarise(Frequency = n())


# Reactable - All Task views and number of packages
taskview_list <- available.views()
taskview_name <- c()
for(i in 1:length(taskview_list)){
  taskview_name[i]=taskview_list[[i]][["name"]]
}
taskview_pkg_count <- c()
for(i in 1:length(taskview_list)){
  taskview_pkg_count[i]=length(taskview_list[[i]][["packagelist"]][["name"]])
}
taskview_df <- data.frame(TaskViews = taskview_name, NumberOfPackages = taskview_pkg_count)

