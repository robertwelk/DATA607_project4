---
title: "iukgbjkuvjhvfhjc"
author: "Rob Welk"
date: "April 7, 2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#packages used
```{r}
library(tm)
library(tidytext)
library(tidyverse)
library(XML)
library(magrittr)
library(SnowballC)
#library(e1071)
```
# Project overview 
The goal of this project is classify email documents as spam(unsolicited) or ham (solicited) based on already classfied training documents.  Files for download were made available by spamassassine and are at https://spamassassin.apache.org/old/publiccorpus/

# Methodology 
The spam and ham training corpa are loaded into r, and basic tidying operatations are 
performed until the data is in 'tidytext' form as described in 'Text Mining in R' by Silge and Robinson (2016) andEach variable is a column Each observation is a row A term-document matrix is then developed 
```{r}
```

# Ham data preperation
## Loading Data into R
```{r}
## get directories
ham.dir <- "C:/Users/robbj/CUNY SPS/DATA607/spamham/easy_ham/"
spam.dir <- "C:/Users/robbj/CUNY SPS/DATA607/spamham/spam/"

## list of files in directory
ham.files <- list.files(ham.dir)
spam.files <- list.files(spam.dir)

```

## tidy the data sets
The loaded corpus contains a collection of email messages in a structured form.
Additional information is given, and only the body of the email is needed.
By examining several documents, it is seen that the body generally begins after the first blank line.
The results are stored in a dataframe, and will later be processed into a Document-Term Matrix.

```{r}
#Try to get only the content of the email message 
#blank dataframe to store 
ham.df <- data.frame(content=NA, doc.num=NA, stringsAsFactors = FALSE)

for(i in 1:length(ham.files)){
  input.file <- readLines(paste0(ham.dir,ham.files[i])) %>% 
                          data.frame(stringsAsFactors = F) 
  colnames(input.file) <- 'content'
  first.blank.row <- min(which(input.file$content==""))
  body <- slice(input.file, -1:-(first.blank.row))
  body <- str_c(body$content, collapse=" ")
  tmp.df <- data.frame(content=body, doc.num=ham.files[i])
  ham.df <- rbind(ham.df,tmp.df)
 }
ham.df <- ham.df %>% slice(-1)
```

```{r}
#Try to get only the content of the email message 
#blank dataframe to store 
spam.df <- data.frame(content=NA, doc.num=NA, stringsAsFactors = FALSE)

for(i in 1:length(spam.files)){
  input.file <- readLines(paste0(spam.dir, spam.files[i])) %>% 
                          data.frame(stringsAsFactors = F) 
  colnames(input.file) <- 'content'
  first.blank.row <- min(which(input.file$content==""))
  body <- slice(input.file, -1:-(first.blank.row))
  body <- str_c(body$content, collapse=" ")
  tmp.df <- data.frame(content=body, doc.num=spam.files[i])
  ham.df <- rbind(spam.df,tmp.df)
 }
spam.df <- spam.df %>% slice(-1)
```

## get into corpus form
```{r}
ham.corpus <- VCorpus(VectorSource(ham.df$content))
spam.corpus <- VCorpus(VectorSource(spam.df$content))
# add metadata
#add label in metadata
for(i in 1:length(ham.corpus)){
  meta(ham.corpus[[i]], "classification")<-'ham'
}
for(j in 1:length(spam.corpus)){
  meta(spam.corpus[[j]], "classification")<-'spam'
}
```

```{r}
### apply some tm_map transformations to remove unwanted characters
# tm_map functions can be applied to an entire corpus
ham.corpus <- ham.corpus %>% tm_map(content_transformer(PlainTextDocument))
ham.corpus <- ham.corpus %>% tm_map(content_transformer(removePunctuation))
ham.corpus <- ham.corpus %>% tm_map(content_transformer(removeNumbers))
ham.corpus <- tm_map(ham.corpus, removeWords, words = stopwords("en"))
ham.corpus <- tm_map(ham.corpus, content_transformer(tolower))
#ham.corpus <- tm_map(ham.corpus, stemDocument)

spam.corpus <- spam.corpus %>% tm_map(content_transformer(PlainTextDocument))
spam.corpus <- spam.corpus %>% tm_map(content_transformer(removePunctuation))
spam.corpus <- spam.corpus %>% tm_map(content_transformer(removeNumbers))
spam.corpus <- tm_map(spam.corpus, removeWords, words = stopwords("en"))
spam.corpus <- tm_map(spam.corpus, content_transformer(tolower))
#ham.corpus <- tm_map(ham.corpus, stemDocument)
corpus <- c(ham.corpus,spam.corpus)
```

## create term document matrix 
```{r}
tdm <- DocumentTermMatrix(corpus)
tdm <- removeSparseTerms(test.tdm, .99)
inspect(test.tdm)
head(test.dtm)
```

# split into training and test sets


```{r}
# Set Seed so that same sample can be reproduced in future also
set.seed(101)  

# Now Selecting 75% of data as sample from total 'n' rows of the data 
sample <- sample.int(n = nrow(tdm), size = floor(.75*nrow(tdm)), replace = F)
train <- tdm[sample, ]
test  <- tdm[-sample, ]
```

```{r}
```

```{r}
```