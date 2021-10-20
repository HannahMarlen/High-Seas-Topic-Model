# Topic Model about the Future of the High Seas

library(quanteda)
library(tidyverse)
library(tidytext)
library(tm)
library(topicmodels)
library(readxl)

# Loading the Data 

Scopus_Titles_Abstracts <- read_excel("Scopus_Abstract_Data/Scopus_Titles_Abstracts.xlsx")
View(Scopus_Titles_Abstracts)

# Tidying the Data

unnest(Scopus_Titles_Abstracts, cols = c(1,2))

tidy_abstracts <- Scopus_Titles_Abstracts %>%
  unnest_tokens(word, 2) %>%
  anti_join(stop_words) %>%
  filter(word != "abstract")

tidy_abstracts %>%
  count(word, sort = TRUE)


# Calculating the Frequency of Terms (tdf_idf)

x <- tidy_abstracts %>%
  count(Title, word, sort = TRUE) %>%
  bind_tf_idf(word, Title, n) %>%
  arrange(-tf_idf) %>%
  group_by(Title) %>%
  top_n(3)


# Creating the Document Term Matrix

dtm <- tidy_abstracts %>% 
  select(Abstract, Title) %>%
  unique() %>% 
  unnest_tokens(word, Abstract) %>% 
  anti_join(stop_words) %>% 
  filter(!str_detect(word, "[:digit:]")) %>%
  mutate(word = textstem::lemmatize_words(word)) %>% 
  group_by(Title) %>%
  count(word, sort = TRUE) %>%
  cast_dtm(document = Title, term = word, value = n)
