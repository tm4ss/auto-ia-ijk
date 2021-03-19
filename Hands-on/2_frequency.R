options(stringsAsFactors = FALSE)
require(quanteda)
library(dplyr)
library(tidyverse)

textdata <- read.csv("data/articles_coronaberichterstattung_coronabezug_v10.csv", sep = ";", encoding = "UTF-8")

# we add some more metadata columns to the data frame

textdata <- textdata %>% mutate(month = floor_date(as.Date(article_date, "%d.%m.%Y"), "month"))

# some cleaning
textdata$article_text <- stringi::stri_replace_all_regex(textdata$article_text, "(\\p{Ll}[.!?])(\\p{Lu})", "$1 $2")

corona_corpus <- corpus(textdata$article_text, docnames = textdata$id)

# Build a dictionary of lemmas
lemma_data <- read.csv("resources/word_baseform_mwu_fixed.txt", sep = ";", encoding = "UTF-8")

# Create a DTM
sw <- readLines("resources/stopwords_de.txt", encoding = "UTF-8")
sw_upper <- paste(toupper(substring(sw, 1,1)), substring(sw, 2), sep="")
sw_extended <- c(sw, sw_upper)
corpus_tokens <- corona_corpus %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_replace(lemma_data$V1, lemma_data$V2, valuetype = "fixed", case_insensitive = F) %>% 
  tokens_remove(pattern = sw_extended)

print(paste0("1: ", substr(paste(corpus_tokens[1],collapse = " "), 0, 400), '...'))

DTM <- corpus_tokens %>% 
  dfm(tolower = F) 

# check most frequent words
wordlist <- data.frame(
  word = colnames(DTM),
  freqs = colSums(DTM),
  row.names = NULL
)

wordlist <- wordlist %>% 
  arrange(desc(freqs)) %>% 
  mutate(rank = row_number())
head(wordlist, 25)


# problemfälle: 
# - SALZGITTERNatürlich
# - case insensitive token_replace

# re-order the wordlist by decreasing frequency &
# show the most frequent words

# plot word frequencies
ggplot(wordlist, aes(x = rank, y = freqs)) +
  geom_line() +
  ggtitle("Rank frequency plot") + 
  xlab("Rank") + 
  ylab("Frequency")


# plot with log scales
ggplot(wordlist, aes(x = rank, y = freqs)) +
  geom_line() +
  ggtitle("Rank frequency plot") +
  scale_x_log10() +
  scale_y_log10() + 
  xlab("log-Rank") + 
  ylab("log-Frequency")


# die collocations liste müsste manuell durchgesehen und selektiert werden. da ist zuviel nicht hilfreiches drin
# collocations <- textstat_collocations(corpus_tokens, min_count = 25)
# collocations <- collocations[1:250, ]
# corona.corpus.tokens <- tokens_compound(corpus_tokens, collocations)

vocabulary <- colnames(DTM)
vocabulary_covid <- vocabulary[grepl("(?i)covid", vocabulary, perl = T)]
vocabulary_corona <- vocabulary[grepl("(?i)corona", vocabulary, perl = T)]

# corona
top_corona <- names(sort(colSums(DTM[, vocabulary_corona]), decreasing = T))[1:5]
terms_to_observe <- top_corona
DTM_reduced <- as.matrix(DTM[, terms_to_observe])
counts_per_month <- aggregate(DTM_reduced, by = list(month = textdata$month), sum)

counts_df <- reshape2::melt(counts_per_month, id.vars = "month")

ggplot(counts_df, aes(x = month, y = value, group = variable)) +
  geom_line(aes(color = variable)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# covid
top_covid <- names(sort(colSums(DTM[, vocabulary_covid]), decreasing = T))[1:5]
DTM_reduced <- as.matrix(DTM[, top_covid])
counts_per_month <- aggregate(DTM_reduced, by = list(month = textdata$month), sum)
counts_df <- reshape2::melt(counts_per_month, id.vars = "month")
ggplot(counts_df, aes(x = month, y = value, group = variable)) +
  geom_line(aes(color = variable)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# verschwörung
vocabulary_conspiracy <- vocabulary[grepl("(?i)verschwörung", vocabulary, perl = T)]
top_conspiracy <- names(sort(colSums(DTM[, vocabulary_conspiracy]), decreasing = T))[1:5]
DTM_reduced <- as.matrix(DTM[, top_conspiracy])
counts_per_month <- aggregate(DTM_reduced, by = list(month = textdata$month), sum)
counts_df <- reshape2::melt(counts_per_month, id.vars = "month")
ggplot(counts_df, aes(x = month, y = value, group = variable)) +
  geom_line(aes(color = variable)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# SENTIMENT
# ---------

# install package
# devtools::install_github("kbenoit/quanteda.dictionaries") 

# We can use a sentiment dict by 
# Christian Rauh (2018) Validating a sentiment dictionary for German political 
# language—a workbench note, Journal of Information Technology & Politics, 
# 15:4, 319-343, DOI: 10.1080/19331681.2018.1485608  


# tokenize texts for Rauh dictionary
tokens_sentiment <- corona_corpus %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_replace(pattern = c("nicht", "nichts", "kein", "keine", "keinen"), replacement = rep("not", 5))
# compound bi-gram negation patterns
tokens_sentiment <- tokens_compound(tokens_sentiment, data_dictionary_Rauh, concatenator = " ")

# apply dictionary
dfm_sentiment <- quanteda::dfm(tokens_sentiment, dictionary = data_dictionary_Rauh)

# how many matches?
colSums(dfm_sentiment)

# how long are the single articles?
dfm_complete <- corona_corpus %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>% 
  dfm()
article_lengths <- rowSums(dfm_complete)

counts_per_medium <- aggregate(as.matrix(dfm_sentiment), by = list(medium = textdata$media_name), sum)
tokens_per_medium <- aggregate(article_lengths, by = list(medium = textdata$media_name), sum)
counts_per_medium[, 2:5] <- counts_per_medium[, 2:5] / tokens_per_medium$x
sentiment_df <- reshape2::melt(counts_per_medium)

ggplot(sentiment_df, aes(x = variable, y = value, group = medium)) +
  geom_bar(stat = "identity", aes(fill = medium), position = "dodge")


# what else to do with sentiments?
# - sentences: identify sentences mentioning (Jens) Spahn, sentiment in different media over time
# - sentiments per topic?



# My own dictionary
# -----------------

corona_dict <- dictionary(list(
  corona = vocabulary_corona,
  covid = vocabulary_covid,
  conspiracy = vocabulary_conspiracy
))

dfm_dict <- dfm(corpus_tokens, dictionary = corona_dict)

head(dfm_dict)

colSums(dfm_dict)

# Task: 
# - Create your own dictionary, e.g. to measure mentions of known scientists
# - measure their absolute frequency over time
# - create a normalized version of the frequency measure