##--Function Module for Coursera Capstone Project - Next word prediction
##--Developed by Pieter Eksteen

##--------------------------------------------------------------------------------------------------
##--Functions
##--------------------------------------------------------------------------------------------------

options(scipen = 999,digits = 5)

##--Not in function
'%!in%' <- function(x,y)!('%in%'(x,y))

##-------------------------------------
##--Function to Predict Next Word
##-------------------------------------

##--Prediction Function
predict_word <- function(string, lookup_table = word_prediction_lookup_table) {
    
    prediction <- NULL
    
    ##--Prepare Input String
    input_string <- gsub("\\s+", " ", stringr::str_trim(string) %>%
                             sub('.*\\.', '', .) %>%
                             sub('.*\\,', '', .) %>%
                             sub('.*\\?', '', .) %>%
                             sub('.*\\!', '', .) %>%
                             trimws(., "both") %>% 
                             stringr::str_to_lower())
    
    ##--Get String Length
    input_string_length <- str_count(input_string, ' ') + 1
    
    ##--N6 Search
    if(input_string_length >= 5) {
        
        cat('\n')
        cat('Entering into n6 search')
        cat('\n')
        
        words_needed <- 10 - ifelse(is.null(prediction), length(prediction), nrow(prediction))
        
        lookup_string <- stringr::word(input_string, start = -5, end = -1)
        
        cat('\n')
        cat(paste('Looking for string:', lookup_string))
        cat('\n')
        
        input_string_length <- str_count(lookup_string, ' ') + 1
        
        next_words <- lookup_table %>% 
            filter(ngram_label == paste0('n', input_string_length + 1)) %>% 
            filter(lookup_words == lookup_string) %>% 
            head(words_needed) %>% 
            select(next_word, prob)
        
        cat(paste('   ', nrow(next_words),'results found'))    
        cat('\n')
        
        prediction <- dplyr::bind_rows(prediction,
                                       next_words)
        
        ##--Add condition 
        if(ifelse(is.null(prediction), length(prediction), nrow(prediction)) < 10) {
            
            input_string_length = input_string_length - 1
            
        }
        
    }
    
    ##--N5 Search
    if(input_string_length == 4) {
        
        cat('\n')
        cat('Entering into n5 search')
        cat('\n')
        
        words_needed <- 10 - ifelse(is.null(prediction), length(prediction), nrow(prediction))
        
        lookup_string <- stringr::word(input_string, start = -4, end = -1)
        
        cat('\n')
        cat(paste('Looking for string:', lookup_string))
        cat('\n')
        
        next_words <- lookup_table %>% 
            filter(ngram_label == paste0('n', input_string_length + 1)) %>% 
            filter(lookup_words == lookup_string) %>%
            filter(next_word %!in% prediction$next_word) %>% 
            head(words_needed) %>% 
            select(next_word, prob)

        cat(paste('   ', nrow(next_words),'results found'))    
        cat('\n')
        
        prediction <- dplyr::bind_rows(prediction,
                                       next_words)
        
        ##--Add condition 
        if(ifelse(is.null(prediction), length(prediction), nrow(prediction)) < 10) {
            
            input_string_length = input_string_length - 1
            
        }
        
    }

    ##--N4 Search    
    if(input_string_length == 3) {
        
        cat('\n')
        cat('Entering into n4 search')
        cat('\n')
        
        words_needed <- 10 - ifelse(is.null(prediction), length(prediction), nrow(prediction))
        
        lookup_string <- stringr::word(input_string, start = -3, end = -1)
        
        cat('\n')
        cat(paste('Looking for string:', lookup_string))
        cat('\n')
        
        next_words <- lookup_table %>% 
            filter(ngram_label == paste0('n', input_string_length + 1)) %>% 
            filter(lookup_words == lookup_string) %>% 
            filter(next_word %!in% prediction$next_word) %>%
            head(words_needed) %>% 
            select(next_word, prob)
        
        cat(paste('   ', nrow(next_words),'results found'))    
        cat('\n')
        
        prediction <- dplyr::bind_rows(prediction,
                                       next_words)
        
        ##--Add condition 
        if(ifelse(is.null(prediction), length(prediction), nrow(prediction)) < 10) {
            
            input_string_length = input_string_length - 1
            
        }
        
    }
    
    ##--N3 Search
    if(input_string_length == 2) {
        
        cat('\n')
        cat('Entering into n3 search')
        cat('\n')
        
        words_needed <- 10 - ifelse(is.null(prediction), length(prediction), nrow(prediction))
        
        lookup_string <- stringr::word(input_string, start = -2, end = -1)
        
        cat('\n')
        cat(paste('Looking for string:', lookup_string))
        cat('\n')
        
        next_words <- lookup_table %>% 
            filter(ngram_label == paste0('n', input_string_length + 1)) %>%
            filter(lookup_words == lookup_string) %>% 
            filter(next_word %!in% prediction$next_word) %>% 
            head(words_needed) %>% 
            select(next_word, prob)
        
        cat(paste('   ', nrow(next_words),'results found'))    
        cat('\n')
        
        prediction <- dplyr::bind_rows(prediction,
                                       next_words)
        
        ##--Add condition 
        if(ifelse(is.null(prediction), length(prediction), nrow(prediction)) < 10) {
            
            input_string_length = input_string_length - 1
            
        }
        
    }
    
    ##--N2 Search
    if(input_string_length == 1) {
        
        cat('\n')
        cat('Entering into n2 search')
        cat('\n')
        
        words_needed <- 10 - ifelse(is.null(prediction), length(prediction), nrow(prediction))
        
        lookup_string <- stringr::word(input_string, start = -1, end = -1)
        
        cat('\n')
        cat(paste('Looking for string:', lookup_string))
        cat('\n')
        
        next_words <- lookup_table %>% 
            filter(ngram_label == paste0('n', input_string_length + 1)) %>%
            filter(lookup_words == lookup_string) %>% 
            filter(next_word %!in% prediction$next_word) %>% 
            head(words_needed) %>% 
            select(next_word, prob)
        
        cat(paste('   ', nrow(next_words),'results found'))    
        cat('\n')
        
        prediction <- dplyr::bind_rows(prediction,
                                       next_words)
       
    }
    
    
    ##--Check if suggestions were found, if not add the missing suggestions from most freq single words
    if(ifelse(is.null(prediction), length(prediction), nrow(prediction)) < 10) {
        
        cat('\n')
        cat('Entering additional words search')
        cat('\n')
        
        words_to_add <- 10 - ifelse(is.null(prediction), length(prediction), nrow(prediction))
        
        additional_words <- lookup_table %>% 
            filter(ngram_label == 'n1') %>% 
            filter(words_to_add %!in% prediction$next_word) %>% 
            head(words_to_add) %>% 
            select(next_word, prob)
        
        prediction <- dplyr::bind_rows(prediction,
                                       additional_words)
        
        cat(paste('   ', nrow(additional_words),'words added'))    
        cat('\n')
        
    }
    
    cat('\n')
    cat(paste('Returning', nrow(prediction), 'results'))    
    cat('\n')
    
    return(prediction)
    
}