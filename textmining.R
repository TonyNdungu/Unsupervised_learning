# Load libraries
library(twitteR)
# Twitter API Oauth process.
consumer_key <- 'T247avsV5cil8YUYuSYl25yCa'
consumer_secret <- 'W0HPnGJp2XqsLpyhImjOgem29f0dcxIrkDdzO0aicv3nMDbGdy'
access_token <- '367326351-nFSKL8lSzaFxoTKq2N0WYL8YgTimfReoVvBorNDv'
access_secret <- '9dabbWazeHYRz3mHPseTA1O9YBmaMakUJvLgrfKRDigDC'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# retrieve the first 200 tweets from the timeline of @Rdatamining
rdmTweets <- userTimeline("rdatamining", n=200)
(nDocs <- length(rdmTweets))

# Have a look at the five tweets numbered 11 to 15
rdmTweets[11:15]

# Wrapping the tweets to fit the width of paper
for (i in 11:15) {
  cat(paste("[[", i, "]] ", sep=""))
  writeLines(strwrap(rdmTweets[[i]]$getText(), width=73))
}

#convert tweets to a data frame
df <- do.call("rbind", lapply(rdmTweets, as.data.frame))
dim(df)

# build a corpus, and specify the source to be character vectors
library(tm)
myCorpus <- Corpus(VectorSource(df$text))

# convert to lower case
myCorpus <- tm_map(myCorpus, tolower)
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
# remove URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
myCorpus <- tm_map(myCorpus, removeURL)
# add two extra stop words: "available" and "via"
myStopwords <- c(stopwords('english'), "available", "via")
# remove "r" and "big" from stopwords
myStopwords <- setdiff(myStopwords, c("r", "big"))
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

library(SnowballC)
# keep a copy of corpus to use later as a dictionary for stem completion
myCorpusCopy <- myCorpus
# stem words
myCorpus <- tm_map(myCorpus, stemDocument)
# inspect documents (tweets) numbered 11 to 15
# inspect(myCorpus[11:15])
# The code below is used for to make text fit for paper width
for (i in 11:15) {
  cat(paste("[[", i, "]] ", sep=""))
  writeLines(strwrap(myCorpus[[i]], width=73))
}

# stem completion
myCorpus <- tm_map(myCorpus, stemCompletion, dictionary=myCorpusCopy)

#count frequency of "mining"
miningCases <- tm_map(myCorpusCopy, grep, pattern="\\<mining")
sum(unlist(miningCases))

# count frequency of "miners"
minerCases <- tm_map(myCorpusCopy, grep, pattern="\\<miners")
sum(unlist(minerCases))

# replace "miners" with "mining"
myCorpus <- tm_map(myCorpus, gsub, pattern="miners", replacement="mining")

# Build a term document matrix
myTdm <- TermDocumentMatrix(myCorpus, control=list(wordLengths=c(1,Inf)))

myTdm <- TermDocumentMatrix(myCorpus, control=list(minWordLength=1))

# inspect frequent words
findFreqTerms(myTdm, lowfreq=10)

termFrequency <- rowSums(as.matrix(myTdm))
termFrequency <- subset(termFrequency, termFrequency>=10)
library(ggplot2)
qplot(names(termFrequency), termFrequency, geom="bar", xlab="Terms") + coord_flip()
barplot(termFrequency, las=2)

# which words are associated with "r"?
findAssocs(myTdm, 'r', 0.25)
# which words are associated with "mining"?
findAssocs(myTdm, 'mining', 0.25)

library(wordcloud)
m <- as.matrix(myTdm)
# calculate the frequency of words and sort it descendingly by frequency
wordFreq <- sort(rowSums(m), decreasing=TRUE)
# word cloud
set.seed(375) # to make it reproducible
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3, random.order=F,
          colors=grayLevels)

# Clustering words
# remove sparse terms
myTdm2 <- removeSparseTerms(myTdm, sparse=0.95)
m2 <- as.matrix(myTdm2)
# cluster terms
  
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method="ward")
plot(fit)
# cut tree into 10 clusters
rect.hclust(fit, k=10)
(groups <- cutree(fit, k=10))

# change it to a Boolean matrix
termDocMatrix[termDocMatrix>=1] <- 1
# transform into a term-term adjacency matrix
termMatrix <- termDocMatrix %*% t(termDocMatrix)
# inspect terms numbered 5 to 10
termMatrix[5:10,5:10]

#============================================================

# transpose the matrix to cluster documents (tweets)
m3 <- t(m2)
# set a fixed random seed
set.seed(122)
# k-means clustering of tweets
k <- 8
kmeansResult <- kmeans(m3, k)
# cluster centers
round(kmeansResult$centers, digits=3)

#Check the top 3 words in every cluster
for (i in 1:k) {
   cat(paste("cluster ", i, ": ", sep=""))
   s <- sort(kmeansResult$centers[i,], decreasing=T)
   cat(names(s)[1:3], "\n")
# print the tweets of every cluster
# print(rdmTweets[which(kmeansResult$cluster==i)])
}

# Clustering tweets with k-medoid algorithm
library(fpc)
# partitioning around medoids with estimation of number of clusters
pamResult <- pamk(m3, metric="manhattan")
# number of clusters identified
(k <- pamResult$nc)

pamResult <- pamResult$pamobject
# print cluster medoids
for (i in 1:k) {
     cat(paste("cluster", i, ": "))
     cat(colnames(pamResult$medoids)[which(pamResult$medoids[i,]==1)], "\n")
# print tweets in cluster i
# print(rdmTweets[pamResult$clustering==i])
}

# plot clustering result
layout(matrix(c(1,2),2,1)) # set to two graphs per page
plot(pamResult, color=F, labels=4, lines=0, cex=.8, col.clus=1,
        col.p=pamResult$clustering)
layout(matrix(1)) # change back to one graph per page

pamResult2 <- pamk(m3, krange=2:8, metric="manhattan")

# LoadTerm document matrix
#
#Inspect part of the matrix
m2[5:10,1:20]
# change it to a Boolean matrix
m2[m2>=1] <- 1
# transform into a term-term adjacency matrix
m2 <- m2 %*% t(m2)
# inspect terms numbered 5 to 10
m2[5:10,5:10]

library(igraph)
# build a graph from the above matrix
g <- graph.adjacency(m2, weighted=T, mode="undirected")
# remove loops
g <- simplify(g)
# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)

# Plot the network with layout
# set seed to make the layout reproducible
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g)
plot(g, layout=layout1)

#Details about other layout options
plot(g, layout=layout.kamada.kawai)
tkplot(g, layout=layout.kamada.kawai)

V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree)+ .2
V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA
egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam
# plot the graph in layout1
plot(g, layout=layout1)

