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

save(dtm, file = "High-Seas-Topic-Model")

# Calculating the best fitting Algorithm

SEED <- 2010
k <- 10


tset.TM <- list (
  VEM0 = LDA(dtm, k=k, control = list ( seed = SEED)),
  VEM_fixed= LDA(dtm, k=k, control= list (estimate.alpha = F, seed = SEED)),
  Gibbs = LDA (dtm, k=k, method ="Gibbs", control = list (seed = SEED, burnin= 1000, thin = 100, iter= 1000)),
  CTM = CTM (dtm, k=k, control = list(seed = SEED, var= list (tol= 10^-4), em= list (tol = 10^-3))))

sapply (tset.TM[1:3], slot, "alpha")

#Finding number of topics

k <- c(5,10,25,50,100)

topicNumber.TM <- map(
  .x = k,
  .f = function(x) {
    LDA(dtm, k = x, control= list (seed = SEED), method = "Gibbs")
  })
save(tset.TM, topicNumber.TM, file = "High-Seas_Topic-Model")

