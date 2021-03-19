options(stringsAsFactors = F)
# load & install required packages
pacman::p_load(readtext, tidyverse, janitor, quanteda)
# read csv corpus

corona_artikel <- read.csv2("data/articles_coronaberichterstattung_coronabezug_v10.csv", encoding = "UTF-8")

# print 1st article
corona_artikel[1, ]

library(dplyr)
library(janitor)
library(magrittr)
# how many articles per media type?
corona_artikel %>% 
  tabyl(media_type) %>% 
  arrange(desc(n))


# plot number of articles over time (e.g. line plot, or bar plot with days, weeks, or months)
# - hint: ggplot2 package + geom_bar/geom_line functions
library(lubridate)
library(ggplot2)

corona_artikel %<>% mutate(week = floor_date(as.Date(article_date,"%d.%m.%Y"), "week"))
week_count <- corona_artikel %>% group_by(week) %>% summarize(sum_article = n())

ggplot(week_count, aes(x = week, y = sum_article)) + 
  geom_smooth(method='glm',formula = y ~ poly(x, 4)) +
  geom_line() +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

require(lubridate)

corona_artikel <- read.csv2("data/articles_coronaberichterstattung_coronabezug_v10.csv", encoding = "UTF-8") %>% 
  mutate(article_date = ymd(dmy(article_date))) %>%
  mutate(week = floor_date(as.Date(article_date,"%d.%m.%Y"), "week"))

corona_artikel %>% 
  group_by(media_type, week) %>% 
  summarize(anzahl = n()) %>% 
  ggplot(aes(x = week, y = anzahl)) +
  geom_line(aes(color = media_type))


# plot a line plot version of the previous plot with different lines for each media type
# - hint: use the group or fill parameter of the ggplot function
week_count <- corona_artikel %>% group_by(week,media_name) %>% summarize(sum_article = n())

ggplot(week_count, aes(x = week, y = sum_article)) + 
  geom_smooth(method='glm',formula = y ~ poly(x, 4)) +
  geom_line() +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(vars(media_name), ncol = 6)



# optional: for which media do we have comments in the corpus
# optional: how many comments per media do we have and which is the most commented article?


# create a quanteda corpus object, preprocess tokens, and create a dfm
lemma_data <- read.csv("resources/baseform_german_web_2012.csv", encoding = "UTF-8", sep = ",",header = T)
lemma_data %<>% mutate(word = tolower(word)) %>% mutate(base_form = tolower(base_form))

corona.corpus <- corpus(corona_artikel$article_text, docvars = corona_artikel %>% select(-article_text))

corona.corpus.tokens <- corona.corpus %>% tokens(
  remove_separators = T, 
  remove_punct = T,
  remove_numbers = T, 
  remove_symbols = T,
  remove_twitter = T, 
  split_hyphens = T, 
  remove_url = T, 
  verbose=T) %>%  
  quanteda::tokens_tolower() %>%
  tokens_replace(lemma_data$word, lemma_data$base_form, valuetype = "fixed")

collocations <- textstat_collocations(corona.corpus.tokens, min_count = 25)
#collocations <- collocations[1:250, ]
#corona.corpus.tokens <- tokens_compound(corpus_tokens, collocations)

corona.corpus.tokens.dfm <- corona.corpus.tokens %>% 
  dfm(.,remove = stopwords("de")) %>%
  dfm_trim(
    min_termfreq = 1,
    max_termfreq = NULL,
    min_docfreq = 3,
    max_docfreq = NULL
  )

corona.corpus.tokens.dfm 

# how many words containing "covid" or "corona" do you count?
colnames(corona.corpus.tokens.dfm) %>% stringr::str_subset(pattern="(covid)|(corona)")

