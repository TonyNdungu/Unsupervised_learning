# web scrapping tv scripts
library(rvest)
# Which tv show, if you want another show, first check on the website which tv show url is used. 
tvshow <- "vikings"

# Creating download directory and change to it
directory = paste("~/Data Analysis/files/", tvshow, sep="")
dir.create(directory, recursive = TRUE, showWarnings = FALSE)
setwd(directory)

# Setting base url and complte url
baseurl <- "http://www.springfieldspringfield.co.uk/"
url <- paste(baseurl,"episode_scripts.php?tv-show=", tvshow, sep="")

# read the HTML page
scrape_url <- read_html(url)
# node selector
s_selector <- ".season-episode-title"

# scrape href nodes in .season-episode-title
all_urls_season <- html_nodes(scrape_url, s_selector) %>%
  html_attr("href")

str(all_urls_season)
head(all_urls_season)
tail(all_urls_season)

# Loop through all season urls 
for (i in all_urls_season) {
  uri <- read_html(paste(baseurl, i, sep="/"))
  # same thing here first check which node we need to select, so forst do a inspect of the site
  script_selector <- ".scrolling-script-container"
  # scrape all script text to a variable
  text <- html_nodes(uri, script_selector) %>% 
    html_text()
  
  # Get last five characters of all_urls_season as season for saving this to seperate text files
  substrRight <- function(x, n) {
    substr(x, nchar(x)-n+1, nchar(x))
  }
  seasons <- substrRight(i, 5)
  # Write each script to a seperate text file
  write.csv(text, file = paste(directory, "/", tvshow, "_", seasons, ".txt", sep=""), row.names = FALSE)
}


# TEXT MINING
library(tm)
# set filepath to scripts
cname <- file.path(directory)
# see if the filepath contains our scripts
(docname <- dir(cname))

# Crete a Corpus of the text files so we can do some analysis
docs <- Corpus(DirSource(cname), readerControl = list(id=docname))
# Show summary of the Corpus, we have 40 document in our Corpus
summary(docs)

# Inspect the first document, it has 12958 characters
inspect(docs[1])
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))

library(SnowballC)
docs <- tm_map(docs, stemDocument)

docs <- tm_map(docs, stripWhitespace)


inspect(docs[1])
docs <- tm_map(docs, PlainTextDocument)

# Create a tdm
tdm <- TermDocumentMatrix(docs)
# Add readable columnnames, in our case the document filename
docname <- gsub("vikings_", "",docname)
docname <- gsub(".txt", "",docname)
docname <- paste("s",docname, sep="")
colnames(tdm) <- docname
# Show and inspect the tdm
tdm
inspect(tdm[1:10,1:6])



dtm <- DocumentTermMatrix(docs)
rownames(dtm) <- docname
dtm

inspect(dtm[1:10,1:6])

# Term Frequency
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
head(freq,20)

# Add it to a data frame so that we can plot it to show the top 20
tf <- data.frame(term=names(freq), freq=freq)   
head(tf,20)

# Let's plot it
# descending sort of teh tf by freq
tf$term <- factor(tf$term, levels = tf$term[order(-tf$freq)])
library(ggplot2)

p <- ggplot(subset(tf, freq>200), aes(term, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p

# Remove spoarse terms
tdm.common = removeSparseTerms(tdm, sparse = 0.1)
tdm

tdm.common
dim(tdm)

dim(tdm.common)
inspect(tdm.common[1:10,1:6])

tdm.dense <- as.matrix(tdm.common)
dim(tdm.dense)

library(reshape2)
tdm.dense.m <- melt(tdm.dense, value.name = "count")
head(tdm.dense.m)

library(ggplot2)
ggplot(tdm.dense.m, aes(x = Docs, y = Terms, fill = log10(count))) +
  geom_tile(colour = "white") +
  scale_fill_gradient(high="steelblue" , low="white")+
  ylab("") +
  theme(panel.background = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

corr <- cor(tdm.dense)
library(corrplot)
corrplot(corr, method = "circle", type = "upper", tl.col="black", tl.cex=0.7)

tdm.dense.t <- t(tdm.dense)
corr.t <- cor(tdm.dense.t)
corrplot(corr.t,method = "circle", type = "upper", tl.col="black", tl.cex=0.7)
