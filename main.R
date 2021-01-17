library(tidyr)
library(tidytext)
library(tm)

review <- readr::read_csv('skytrax.csv')



tibble::glimpse(r)

#Cleaning NAs
r<- review %>% na.omit()

#Check if there is left
row.has.na <- apply(r, 1, function(x){any(is.na(x))})

sum(row.has.na)



f<- data.frame(r$recommended,r$customer_review)
library(pander)

pander(table(f$r.customer_review))

panderOptions('table.split.cells', Inf)

pandoc.table(f[2:4,1:2], justify = c('left', 'left', 'center'))
#Setting up source and corpus

corpus <- VCorpus(VectorSource(r$customer_review))
corpus[[123]]$content
rm(corpus)
# Cleaning
corpus<- tm_map(corpus,stripWhitespace)

corpus <- tm_map(corpus,removePunctuation)

corpus <- tm_map(corpus,content_transformer(tolower))

corpus<- tm_map(corpus,removeWords,stopwords("english"))

#since it is a flight review it has lots of time info lets remove
corpus <- tm_map(corpus,removeNumbers)



#Custom Cleaning 
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))


#Since data scraped from website as png next to comments below we have removed it
corpus <- tm_map(corpus,toSpace, c("âœ… | Not Verified | âœ… verified | âœ… trip verified | Verified | trip verified|verified review"))


corpus <- tm_map(corpus,stripWhitespace)

dtm <- DocumentTermMatrix(corpus)
dtm2 <-as.matrix(dtm)

freq<- findFreqTerms(dtm,10)
head(freq)
inspect(dtm[1250:1260,])
corpus[[1536]]$content
corpus[[1459]]$content

library(dplyr)

tidy_corp <- tidy(corpus)
tidy_corp <- tidy_corp %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words)

## DTM and Wordcloud

review_dtm <- removeSparseTerms(dtm,0.99)

inspect(review_dtm[1:20,])


findFreqTerms(review_dtm,1000)


freq <- data.frame(sort(colSums(as.matrix(review_dtm)), decreasing=TRUE))
library(wordcloud)


wordcloud(rownames(freq), freq[,1], max.words=500, colors=brewer.pal(1, "Dark2"))


review_dtm_tf <- DocumentTermMatrix(corpus,control=c(weighting =weightTfIdf))
review_dtm_tf<- removeSparseTerms(review_dtm_tf,0.95)
inspect(review_dtm_tf)

freq1 <- data.frame(sort(colSums(as.matrix(review_dtm_tf)), decreasing=TRUE))
wordcloud(rownames(freq1), freq1[,1], max.words=500, colors=brewer.pal(1, "Dark2"))


##sentiment

tidy_r <- r %>% 
  unnest_tokens(word,customer_review) %>% 
  anti_join(stop_words)
tidy_r %>% 
  count(word) %>% 
  arrange(desc(n))

word2=fct_reorder(tidy_r$word,x=word2,y=n)
count_of_word <- tidy_r %>% 
  count(word) %>% 
  filter(n>1500) %>% 
  mutate(word2=fct_reorder(word,n))


ggplot(count_of_word, aes(x=word2, y=n)) + 
  geom_col() +
  coord_flip() +
  ggtitle("Review Word Counts")



airline <- tidy_r %>%
  count(word, airline) %>%
  group_by(airline) %>%
  top_n(10, n) %>%
  ungroup() %>% 
  mutate(word2 = fct_reorder(word,n))


### Filter ekle

ggplot(airline, aes(x=word2, y=n)) + 
  geom_col(show.legend=FALSE) +
  coord_flip() +
  ggtitle("Review Word Counts")
