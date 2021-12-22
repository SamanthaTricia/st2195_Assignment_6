setwd("~/Desktop/SIM/ST2195/prac/st2195_Assignment_6")

library(dplyr)

fx <- read.csv("~/Desktop/SIM/ST2195/prac/st2195_Assignment_6/fx.csv", skip=6, header = TRUE, na.strings = "-")
#missing values stored as "-"

fx <- fx[1:2]
#subset the first 2 columns
colnames(fx) <- c("date", "exchange_rate")

str(fx)
nrow(fx)
length(unique(fx$date))
head(fx)

## Load speeches

speeches <- read.csv("~/Desktop/SIM/ST2195/prac/st2195_Assignment_6/speeches.csv", sep = '|', quote ="", encoding ="UTF-8")
speeches <- speeches[!is.na(speeches$contents),c('date', 'contents')]
#is.na --- filter out the null datas
str(speeches)
nrow(speeches)
length(unique(speeches$date)) #there can be more than one speeches on the same date
head(fx)

# Merge fx and speeches

# To merge correctly with fx data, we need to paste all contents for each date together
speeches <- speeches %>%
  group_by(date) %>%
  summarise(contents=paste(contents,collapse = " "))
str(speeches)
nrow(speeches)

# syntax:left.join(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)
df <- fx %>% left_join(speeches)
str(df)

## Change data types for data and exchange_rate
df$exchange_rate <- as.numeric(df$exchange_rate)
df$date <- as.Date(df$date)
str(df)
  
## REMOVE ENTRIES WITH OBVIOUS OUTLIERS OR MISTAKES

# Check for any obvious outliers or mistakes by plotting the data
plot(df$date, df$exchange_rate, type ='l', xlab="date",
     ylab = "EUR/USD reference exchange rate")

# Look at summary statistics
# Observation: no obvious outliers/mistakes, but there is missing data (NA)
summary(df)

## HANDLE MISSING DATA

#we can use fill from dplyr package(part of tidyverse) also use the 'na.locf()' from zoo
#syntax is fill(data, ...., .direction = c("down", "downup", "updown"))
#Use fill direction of "up" as date in descending order and we want prior order value

install.packages("tidyr")
library(tidyr)
df2 <- df
df2 <- df2 %>% fill(exchange_rate, .direction ="up")
summary(df2)

#use the 'na.locf()' from zoo
#locf = last observation carried forward
#fromlast = replacing each NA with the most recent non-NA prior to it

#install.packages("zoo")
#library(zoo)
#df3 <- df
#df3$exchange_rate <- na.locf(df3$exchange_rate, fromLast = TRUE)
#summary(df3)

#IF want to check whether both ways produce the same result 
#all.equal(df2,df3)

## EXCHANGE RATE RETURN

help(diff)

# Difference between two elements, with default lag=1
diff(df2$exchange_rate)

#as date is in descending order, multiply by -1?
#depends on perpective? EURO? USD?
diff(df2$exchange_rate)*(-1)

df2$exchange_rate
df2$exchange_rate[-1] #exclude the first element

#last element set to NA, as there aren't any further prior values for calculation
df2$return <- c( (diff(df2$exchange_rate)*(-1)) / df2$exchange_rate[-1], NA)

#tail(df$return)                
#tail(df2$return)

## EXTEND DATSET WITH VARIABLES FOR "GOOD NEWS" AND "BAD NEWS"

df2$good_news <- as.numeric(df2$return > 0.5/100)
df2$bad_news <- as.numeric(df2$return > -0.5/100)

## ASSOCIATE WORDS WITH "GOOD NEWS" AND "BAD NEWS"

library(tidyr)

#Remove rows with NA
df2 <- df2 %>% drop_na(contents)
str(df2)

#Get the contents related to "good news" and "bad news"
good_news_contents <- df2$contents[df2$good_news==1]
bad_news_contents <- df2$contents[df2$bad_news==1]

str(bad_news_contents)
#Load in stop words, which are those used to form a sentence but does not add much meaning.
stop_words <- read.csv("~/Desktop/SIM/ST2195/prac/stop_words_english.txt", header = FALSE)[,1]
stop_words

# Function below helps us to get the most common words (excluding stop_words) related to `good_news` and `bad_news`. Please read the comments to understand how it works:
install.packages("text2vec")
library(text2vec)
??text2vec

get_word_freq <- function(contents, stop_words, num_words) {
  
  # turn a paragraph to a vector of words
  words <- unlist(lapply(contents, word_tokenizer))  
  
  # turn all words to lowercase
  words <- tolower(words)  
  
  # find out the number of appearance of each word
  freq <- table(words)
  #length(freq)
  
  # remove the stop words
  freq <- freq[!(names(freq) %in% stop_words)]
  #length(freq)
  
  # sort the words from appearing most to least and return the result
  freq <- sort(freq, decreasing=TRUE)
  
  return(names(freq)[1:num_words])
  
}

# Use the function above to get the 20 most common words associated with 
# `good_news` and `bad_news`

good_indicators <- get_word_freq(good_news_contents, stop_words, num_words = 20)
good_indicators

bad_indicators <- get_word_freq(bad_news_contents, stop_words, num_words = 20)
bad_indicators

# Observation: Many terms appear in both, some overlapping


# Additional - Lemmatization
# Lemmatization can possibly narrow the terms further

install.packages("textstem")
library(textstem)

#add lemmatization step to the word counter function
get_word_freq_lemmatize <- function(contents, stop_words, num_words) {
  # turn a paragraph to a vector of words
  words <- unlist(lapply(contents, word_tokenizer))  
  # turn all words to lowercase
  words <- tolower(words)  
  # lemmatize words
  words <- lemmatize_words(words)
  # find out the number of appearance of each word
  freq <- table(words)
  # remove the stop words
  freq <- freq[!(names(freq) %in% stop_words)]
  # sort the words from appearing most to least and return result
  freq <- sort(freq, decreasing=TRUE)
  return(names(freq)[1:num_words])
  #names(freq[order(-freq)])[1:num_words] #another way to do this
}

good_indicators2 <- get_word_freq_lemmatize(good_news_contents, stop_words, num_words=20)
good_indicators2

bad_indicators2 <- get_word_freq_lemmatize(bad_news_contents, stop_words, num_words=20)
bad_indicators2

