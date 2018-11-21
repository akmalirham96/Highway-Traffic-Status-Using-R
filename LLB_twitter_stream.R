#Read file

highway <- read.csv(file.choose(),header = T)
str(highway)

#Build Corpus

library(tm)
library(stopwords)
corpus <- iconv(highway$text,to = 'UTF-8', sub = "byte")
corpus <- Corpus(VectorSource(corpus))

#Cleaning text
corpus <- tm_map(corpus, tolower)
inspect(corpus)

#Remove comar
corpus <- tm_map(corpus, removePunctuation)
inspect(corpus)

#remove number
corpus <- tm_map(corpus, removeNumbers)
inspect(corpus)


#Exception word
exceptions <- c('dari','dan','di','ke','masa','maklumat')

my_stopwords <- setdiff(stopwords("ms", source = "stopwords-iso"), exceptions)

#stopword
cleanset <- tm_map(corpus,removeWords,my_stopwords)
cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset)

#Remove url
removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(corpus)

#remove word
cleanset <- tm_map(cleanset, removeWords, c('am','pm','amp'))

#remove whitespace
cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset)

#insert filter tweet to data
data <- cleanset

#check wheather word are in the data
simpang <- regexpr("simpang",data[1:4])
substr(data,139,139 + 7 -1)

#checking data
if(simpang > 1){
  print("Traffic Busy")
}else{
  print("Traffic Clear")
}
