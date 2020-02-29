library(stringr)
library(tidyverse)
library(quanteda)
library(tictoc)
library(fst)
library(data.table)

quanteda_options(threads= 7)

##--------------------------------------------------------------------------------------------------
##--Load Data
##--------------------------------------------------------------------------------------------------

##--Blogs Data
con <- file(file.path(here::here(), 'Capstone Project', 'data', 'en_US.blogs.txt'), "r")
en_us_blogs <- readLines(con, -1, encoding = 'UTF-8')
close(con)
rm(con)

##--News Data
con <- file(file.path(here::here(), 'Capstone Project', 'data', 'en_US.news.txt'), "r")
en_us_news <- readLines(con, -1, encoding = 'UTF-8')
close(con)
rm(con)

##--Twitter Data
con <- file(file.path(here::here(), 'Capstone Project', 'data', 'en_US.twitter.txt'), "r")
en_us_twitter <- readLines(con, -1, encoding = 'UTF-8')
close(con)
rm(con)

##--------------------------------------------------------------------------------------------------
##--Create Corpus
##--------------------------------------------------------------------------------------------------

##--Create Blogs Corpus
blogs_corpus <- quanteda::corpus(en_us_blogs)
summary(blogs_corpus)

##--Create News Corpus
news_corpus <- quanteda::corpus(en_us_news)
summary(news_corpus)

##--Create Twitter Corpus
twitter_corpus <- quanteda::corpus(en_us_twitter)
summary(twitter_corpus)

##--Combine into one corpus
final_corpus <- blogs_corpus + news_corpus + twitter_corpus

rm(en_us_blogs,
   en_us_news, 
   en_us_twitter, 
   blogs_corpus, 
   news_corpus, 
   twitter_corpus)

gc()

##--Save Corpus
saveRDS(final_corpus, file.path('Capstone Project', 'data', 'final_corpus.rds'))

##--------------------------------------------------------------------------------------------------
##--Create N-grams
##--------------------------------------------------------------------------------------------------

quick_load <- TRUE

if(quick_load) {
   
   final_corpus <- readRDS(file.path('Capstone Project', 'data', 'final_corpus.rds'))
   
}

##--N1
tic()
n1 <- tokens(final_corpus,
             what = "word",
             remove_symbols = TRUE,
             remove_numbers = TRUE,
             remove_punct = TRUE,
             remove_twitter = TRUE,
             remove_url = TRUE,
             remove_separators = TRUE,
             ngrams = 1,
             concatenator = " ")
toc()

tic()
n1 <- dfm(n1)
toc()

tic()
n1 <- dfm_trim(n1, min_termfreq = 100)
toc()

tic()
n1 <- sort(colSums(n1),decreasing=TRUE)
toc()

##--Save n1
saveRDS(n1, file.path('Capstone Project', 'data', 'n1_data.rds'))

rm(n1)
gc()

##--N2
tic()
n2 <- tokens(final_corpus,
             what = "word",
             remove_symbols = TRUE,
             remove_numbers = TRUE,
             remove_punct = TRUE,
             remove_twitter = TRUE,
             remove_url = TRUE,
             remove_separators = TRUE,
             ngrams = 2,
             concatenator = " ")
toc()

tic()
n2 <- dfm(n2)
toc()

tic()
n2 <- dfm_trim(n2, min_termfreq = 3)
toc()

tic()
n2 <- sort(colSums(n2),decreasing=TRUE)
toc()

##--Save n2
saveRDS(n2, file.path('Capstone Project', 'data', 'n2_data.rds'))

rm(n2)
gc()

##--N3
tic()
n3 <- tokens(final_corpus,
             what = "word",
             remove_symbols = TRUE,
             remove_numbers = TRUE,
             remove_punct = TRUE,
             remove_twitter = TRUE,
             remove_url = TRUE,
             remove_separators = TRUE,
             ngrams = 3,
             concatenator = " ")
toc()

tic()
n3 <- dfm(n3)
toc()

tic()
n3 <- dfm_trim(n3, min_termfreq = 3)
toc()

tic()
n3 <- sort(colSums(n3),decreasing=TRUE)
toc()

##--Save n3
saveRDS(n3, file.path('Capstone Project', 'data', 'n3_data.rds'))

rm(n3)
gc()

##--N4
tic()
n4 <- tokens(final_corpus,
             what = "word",
             remove_symbols = TRUE,
             remove_numbers = TRUE,
             remove_punct = TRUE,
             remove_twitter = TRUE,
             remove_url = TRUE,
             remove_separators = TRUE,
             ngrams = 4,
             concatenator = " ")
toc()

tic()
n4 <- dfm(n4)
toc()

tic()
n4 <- dfm_trim(n4, min_termfreq = 3)
toc()

tic()
n4 <- sort(colSums(n4),decreasing=TRUE)
toc()

##--Save n4
saveRDS(n4, file.path('Capstone Project', 'data', 'n4_data.rds'))

rm(n4)
gc()

##--N5
tic()
n5 <- tokens(final_corpus,
             what = "word",
             remove_symbols = TRUE,
             remove_numbers = TRUE,
             remove_punct = TRUE,
             remove_twitter = TRUE,
             remove_url = TRUE,
             remove_separators = TRUE,
             ngrams = 5,
             concatenator = " ")
toc()

tic()
n5 <- dfm(n5)
toc()

tic()
n5 <- dfm_trim(n5, min_termfreq = 3)
toc()

tic()
n5 <- sort(colSums(n5),decreasing=TRUE)
toc()

##--Save n5
saveRDS(n5, file.path('Capstone Project', 'data', 'n5_data.rds'))

rm(n5)
gc()

##--N6
tic()
n6 <- tokens(final_corpus,
             what = "word",
             remove_symbols = TRUE,
             remove_numbers = TRUE,
             remove_punct = TRUE,
             remove_twitter = TRUE,
             remove_url = TRUE,
             remove_separators = TRUE,
             ngrams = 6,
             concatenator = " ")
toc()

tic()
n6 <- dfm(n6)
toc()

tic()
n6 <- dfm_trim(n6, min_termfreq = 3)
toc()

tic()
n6 <- sort(colSums(n6),decreasing=TRUE)
toc()

##--Save n6
saveRDS(n6, file.path('Capstone Project', 'data', 'n6_data.rds'))

rm(n6,
   final_corpus)
gc()

##--------------------------------------------------------------------------------------------------
##--Load prepared N-grams
##--------------------------------------------------------------------------------------------------

##--Load n1
n1 <- readRDS(file.path('Capstone Project', 'data', 'n1_data.rds'))

##--Create Tibble
n1_tibble <- tibble(string = names(n1),
                    freq = n1,
                    ngram_label = 'n1') %>% 
   filter(freq >= 1000) %>% 
   mutate(lookup_words = stringr::word(string, start = 1, end = -2),
          next_word = stringr::word(string, start = -1, end = -1))

##--Save Tibble
write.fst(n1_tibble, file.path('Capstone Project', 'data', 'n1_tibble.fst'), 100)
rm(n1,
   n1_tibble)
gc()

##--Load n2
n2 <- readRDS(file.path('Capstone Project', 'data', 'n2_data.rds'))

##--Create Tibble
n2_tibble <- tibble(string = names(n2),
                    freq = n2,
                    ngram_label = 'n2') %>% 
   filter(freq >= 3) %>% 
   mutate(lookup_words = stringr::word(string, start = 1, end = -2),
          next_word = stringr::word(string, start = -1, end = -1))

##--Save Tibble
write.fst(n2_tibble, file.path('Capstone Project', 'data', 'n2_tibble.fst'), 100)
rm(n2,
   n2_tibble)
gc()

##--Load n3
n3 <- readRDS(file.path('Capstone Project', 'data', 'n3_data.rds'))

##--Create Tibble
n3_tibble <- tibble(string = names(n3),
                    freq = n3,
                    ngram_label = 'n3') %>% 
   filter(freq >= 3) %>%
   mutate(lookup_words = stringr::word(string, start = 1, end = -2),
          next_word = stringr::word(string, start = -1, end = -1))

##--Save Tibble
write.fst(n3_tibble, file.path('Capstone Project', 'data', 'n3_tibble.fst'), 100)
rm(n3,
   n3_tibble)
gc()

##--Load n4
n4 <- readRDS(file.path('Capstone Project', 'data', 'n4_data.rds'))

##--Create Tibble
n4_tibble <- tibble(string = names(n4),
                    freq = n4,
                    ngram_label = 'n4') %>% 
   filter(freq >= 3) %>%
   mutate(lookup_words = stringr::word(string, start = 1, end = -2),
          next_word = stringr::word(string, start = -1, end = -1))

##--Save Tibble
write.fst(n4_tibble, file.path('Capstone Project', 'data', 'n4_tibble.fst'), 100)
rm(n4,
   n4_tibble)
gc()

##--Load n5
n5 <- readRDS(file.path('Capstone Project', 'data', 'n5_data.rds'))

##--Create Tibble
n5_tibble <- tibble(string = names(n5),
                    freq = n5,
                    ngram_label = 'n5') %>% 
   filter(freq >= 3) %>%
   mutate(lookup_words = stringr::word(string, start = 1, end = -2),
          next_word = stringr::word(string, start = -1, end = -1))

##--Save Tibble
write.fst(n5_tibble, file.path('Capstone Project', 'data', 'n5_tibble.fst'), 100)
rm(n5,
   n5_tibble)
gc()

##--Load n6
n6 <- readRDS(file.path('Capstone Project', 'data', 'n6_data.rds'))

##--Create Tibble
n6_tibble <- tibble(string = names(n6),
                    freq = n6,
                    ngram_label = 'n6') %>% 
   filter(freq >= 3) %>%
   mutate(lookup_words = stringr::word(string, start = 1, end = -2),
          next_word = stringr::word(string, start = -1, end = -1))

##--Save Tibble
write.fst(n6_tibble, file.path('Capstone Project', 'data', 'n6_tibble.fst'), 100)
rm(n6)
gc()

##--------------------------------------------------------------------------------------------------
##--Create Word Prediction Lookup Table
##--------------------------------------------------------------------------------------------------

##--Load Tibbles
n1_tibble <- read.fst(file.path('Capstone Project', 'data', 'n1_tibble.fst'))
n2_tibble <- read.fst(file.path('Capstone Project', 'data', 'n2_tibble.fst'))
n3_tibble <- read.fst(file.path('Capstone Project', 'data', 'n3_tibble.fst'))
n4_tibble <- read.fst(file.path('Capstone Project', 'data', 'n4_tibble.fst'))
n5_tibble <- read.fst(file.path('Capstone Project', 'data', 'n5_tibble.fst'))
# n6_tibble <- read.fst(file.path('Capstone Project', 'data', 'n6_tibble.fst'))

##--Combine N-grams
word_frequency_data <- dplyr::bind_rows(n1_tibble,
                                        n2_tibble,
                                        n3_tibble,
                                        n4_tibble,
                                        n5_tibble,
                                        n6_tibble)

rm(n2_tibble,
   n3_tibble,
   n4_tibble,
   n5_tibble,
   n6_tibble)
gc()

##--Add Next word frequency
aug_word_frequency_data <- left_join(word_frequency_data,
                                     n1_tibble %>% 
                                        select(string,
                                               single_freq = freq),
                                     by = c('next_word' = 'string'))

rm(n1_tibble,
   word_frequency_data)
gc()

##--Replace NA Single Freq with 0
set(aug_word_frequency_data,which(is.na(aug_word_frequency_data[[6L]])),6L,0)

##--Rename
word_prediction_lookup_table <- aug_word_frequency_data %>% 
   arrange(desc(freq, single_freq)) %>% 
   group_by(lookup_words) %>% 
   mutate(lookup_freq = sum(freq)) %>% 
   ungroup() %>%
   mutate(prob = freq/lookup_freq*100) %>% 
   select(-string, -lookup_freq)

##--Save Lookup Table
write.fst(word_prediction_lookup_table,file.path('Capstone Project', 'data', 'word_prediction_lookup_table_final.fst'), 100)

##--Alternate save to reduce memory usage on shinyapps.io
# n1_lookup <- word_prediction_lookup_table %>% 
#    filter(ngram_label == 'n1') %>% 
#    select(-ngram_label, -lookup_words, -freq, -)
   

##--------------------------------------------------------------------------------------------------
##--Prediction Algorithm
##--------------------------------------------------------------------------------------------------

quick_load <- TRUE

if(quick_load) {
   
   word_prediction_lookup_table <- read.fst(file.path('Capstone Project', 'data', 'word_prediction_lookup_table_final.fst'))
   
}

##--Not in function
'%!in%' <- function(x,y)!('%in%'(x,y))

##--Prediction Function
predict_word <- function(string, lookup_table = word_prediction_lookup_table) {
   
   prediction <- NULL
   
   input_string <- gsub("\\s+", " ", stringr::str_trim(string) %>%
                           sub('.*\\.', '', .) %>%
                           sub('.*\\,', '', .) %>%
                           sub('.*\\?', '', .) %>%
                           sub('.*\\!', '', .) %>%
                           trimws(., "both") %>% 
                           stringr::str_to_lower())
   
   input_string_length <- str_count(input_string, ' ') + 1
   
   if(input_string_length >= 5) {
      
      ##--n6
      cat('\n')
      cat('Entering into n6 search')
      cat('\n')
      
      words_needed <- 5 - length(prediction)
      
      lookup_string <- stringr::word(input_string, start = -5, end = -1)
      
      input_string_length <- str_count(lookup_string, ' ') + 1
      
      next_words <- lookup_table %>% 
         filter(ngram_label == paste0('n', input_string_length + 1)) %>% 
         filter(lookup_words == lookup_string) %>% 
         head(words_needed) %>% 
         .$next_word
      
      cat(paste('   ', length(next_words),'results found'))    
      cat('\n')
      
      prediction <- c(prediction, next_words)
      
      ##--Add condition 
      if(length(prediction) < 5) {
         
         input_string_length = input_string_length - 1
         
      }
      
   }
   
   if(input_string_length == 4) {
      
      ##--n5
      cat('\n')
      cat('Entering into n5 search')
      cat('\n')
      
      words_needed <- 5 - length(prediction)
      
      lookup_string <- stringr::word(input_string, start = -4, end = -1)
      
      next_words <- lookup_table %>% 
         filter(ngram_label == paste0('n', input_string_length + 1)) %>% 
         filter(lookup_words == lookup_string) %>%
         filter(next_word %!in% prediction) %>% 
         head(words_needed) %>% 
         .$next_word
      
      cat(paste('   ', length(next_words),'results found'))    
      cat('\n')
      
      prediction <- c(prediction, next_words)
      
      ##--Add condition 
      if(length(prediction) < 5) {
         
         input_string_length = input_string_length - 1
         
      }
      
   }
   
   if(input_string_length == 3) {
      
      ##--n4
      cat('\n')
      cat('Entering into n4 search')
      cat('\n')
      
      words_needed <- 5 - length(prediction)
      
      lookup_string <- stringr::word(input_string, start = -3, end = -1)
      
      next_words <- lookup_table %>% 
         filter(ngram_label == paste0('n', input_string_length + 1)) %>% 
         filter(lookup_words == lookup_string) %>% 
         filter(next_word %!in% prediction) %>%
         head(words_needed) %>% 
         .$next_word
      
      cat(paste('   ', length(next_words),'results found'))    
      cat('\n')
      
      prediction <- c(prediction, next_words)
      
      ##--Add condition 
      if(length(prediction) < 5) {
         
         input_string_length = input_string_length - 1
         
      }
      
   }
   
   if(input_string_length == 2) {
      ##--n3
      cat('\n')
      cat('Entering into n3 search')
      cat('\n')
      
      words_needed <- 5 - length(prediction)
      
      lookup_string <- stringr::word(input_string, start = -2, end = -1)
      
      next_words <- lookup_table %>% 
         filter(ngram_label == paste0('n', input_string_length + 1)) %>% 
         filter(lookup_words == lookup_string) %>% 
         filter(next_word %!in% prediction) %>% 
         head(words_needed) %>% 
         .$next_word
      
      cat(paste('   ', length(next_words),'results found'))    
      cat('\n')
      
      prediction <- c(prediction, next_words)
      
      ##--Add condition 
      if(length(prediction) < 5) {
         
         input_string_length = input_string_length - 1
         
      }
      
   }
   
   if(input_string_length == 1) {
      ##--n2
      cat('\n')
      cat('Entering into n2 search')
      cat('\n')
      
      words_needed <- 5 - length(prediction)
      
      lookup_string <- stringr::word(input_string, start = -1, end = -1)
      
      next_words <- lookup_table %>% 
         filter(ngram_label == paste0('n', input_string_length + 1)) %>% 
         filter(lookup_words == lookup_string) %>% 
         filter(next_word %!in% prediction) %>% 
         head(words_needed) %>% 
         .$next_word
      
      cat(paste('   ', length(next_words),'results found'))    
      cat('\n')
      
      prediction <- c(prediction, next_words)
      # ##--Add condition 
      # if(length(prediction) < 5) {
      #     
      #     input_string_length = input_string_length - 1
      #     
      # }
      
   }
   
   
   ##--Check if 5 suggestions were found, if not add the missing suggestions from most freq single words
   if(length(prediction) < 5) {
      
      cat('\n')
      cat('Entering additional words search')
      cat('\n')
      
      words_to_add <- 5 - length(prediction)
      
      additional_words <- lookup_table %>% 
         filter(ngram_label == 'n1') %>% 
         head(words_to_add) %>% 
         .$next_word
      
      prediction <- c(prediction, additional_words)
      
      cat(paste('   ', length(additional_words),'words added'))    
      cat('\n')
      
   }
   
   cat('\n')
   cat(paste('Returning results'))    
   cat('\n')
   
   return(prediction)
   
}

##--------------------------------------------------------------------------------------------------
##--Test Algorithm
##--------------------------------------------------------------------------------------------------

##--Test
predict_word('the great')

predict_word('am  the person           who gets      out   ')

predict_word('what happens if   I')

predict_word('What if I type the word Afrikaans')

predict_word('The guy in front of me just bought a pound of bacon, a bouquet, and a case of')

predict_word("You're the reason why I smile everyday. Can you follow me please? It would mean the")

predict_word('Hey sunshine, can you follow me and make me the')

predict_word('Very early observations on the Bills game: Offense still struggling but the')

predict_word('Go on a romantic date at the')
