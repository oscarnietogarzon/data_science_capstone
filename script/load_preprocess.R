install.packages("ctv")
ctv::update.views("NaturalLanguageProcessing")

getwd()
list.files("Coursera-SwiftKey/final/en_US") -> files_US

#### Load sample data ####

## Conection with the local file
con <- file("Coursera-SwiftKey/final/en_US/en_US.twitter.txt", "r")

readLines(con) -> df_sample ## Sample of 5000 lines

## get the longest line
lapply(df_sample, function(x) nchar(x) ) -> num_char_list
sort(unlist(num_char_list), decreasing=TRUE)[1]

close(con)

#### Distribution of sample words per review ####
library(tokenizers)

lapply(df_sample, function(x) length( tokenize_words(x) ) ) -> words_per_line
lapply(df_sample, unlist(tokenize_words) ) -> words_per_line

hist(unlist(words_per_line, use.names = FALSE) )
mean(unlist(words_per_line, use.names = FALSE))

## Tidy approach
tibble(line = 1:2360148, text = df_sample) -> text_df
text_df %>% 
  filter( grepl("\\blove\\b", text) == TRUE ) %>% dim()
  
text_df %>% 
  filter( grepl("\\bhate\\b", text) == TRUE ) %>% dim()

text_df %>% 
  filter( grepl("\\bbiostats\\b", text) == TRUE )

text_df %>% 
  filter( grepl("\\bA computer once beat me at chess, but it was no match for me at kickboxing\\b", text) == TRUE )


#### Pre-processing ####
## 1. Converting words to lower case
library(tidyverse)
library(tidytext)

lapply(df_sample, function(x) tolower(x) ) -> df_sample_low

text_df %>%
  unnest_tokens(word, text)

## 2. Removing special characters
lapply(df_sample, unlist( tokenize_words )) -> df_words

# Tidy approach
text_df %>%
  unnest_tokens(word, text) -> tidy_df

## 3. Remove stop words -- catch profanity
data(stop_words)

`%ni%` <- Negate(`%in%`)

tidy_df %>%
  filter(word %ni% lexicon::profanity_banned & word %ni% lexicon::profanity_alvarez &
         word %ni% lexicon::profanity_arr_bad & word %ni% lexicon::profanity_racist &
           word %ni% lexicon::profanity_zac_anger) %>%
  filter(grepl("^[[:alpha:]]*[aeiouy]", word) == TRUE) %>%
  count(word, sort = TRUE) 
 


