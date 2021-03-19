options(stringsAsFactors = FALSE)
require(quanteda)
library(dplyr)

textdata <- read.csv("data/sotu.csv", sep = ";", encoding = "UTF-8")

# we add some more metadata columns to the data frame
textdata$year <- substr(textdata$date, 0, 4)
textdata$decade <- paste0(substr(textdata$date, 0, 3), "0")

sotu_corpus <- corpus(textdata$text, docnames = textdata$doc_id)

# Build a dictionary of lemmas
lemma_data <- read.csv("resources/baseform_en.tsv", encoding = "UTF-8")

# Create a DTM
corpus_tokens <- sotu_corpus %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed") %>% 
  tokens_remove(pattern = stopwords())

print(paste0("1: ", substr(paste(corpus_tokens[1],collapse = " "), 0, 400), '...'))

DTM <- corpus_tokens %>% 
  dfm() 

terms_to_observe <- c("nation", "war", "god", "terror", "security")

DTM_reduced <- as.matrix(DTM[, terms_to_observe])

counts_per_decade <- aggregate(DTM_reduced, by = list(decade = textdata$decade), sum)

# give x and y values beautiful names
decades <- counts_per_decade$decade
frequencies <- counts_per_decade[, terms_to_observe]

# plot multiple frequencies
matplot(decades, frequencies, type = "l")

# add legend to the plot
l <- length(terms_to_observe)
legend('topleft', legend = terms_to_observe, col=1:l, text.col = 1:l, lty = 1:l)  

#########################################

# English Opinion Word Lexicon by Hu et al. 2004
positive_terms_all <- readLines("data/positive-words.txt")
negative_terms_all <- readLines("data/negative-words.txt")

# AFINN sentiment lexicon by Nielsen 2011
afinn_terms <- read.csv("data/AFINN-111.txt", header = F, sep = "\t")
positive_terms_all <- afinn_terms$V1[afinn_terms$V2 > 0]
negative_terms_all <- afinn_terms$V1[afinn_terms$V2 < 0]

positive_terms_in_suto <- intersect(colnames(DTM), positive_terms_all)
counts_positive <- rowSums(DTM[, positive_terms_in_suto])

negative_terms_in_suto <- intersect(colnames(DTM), negative_terms_all)
counts_negative <- rowSums(DTM[, negative_terms_in_suto])

counts_all_terms <- rowSums(DTM)

relative_sentiment_frequencies <- data.frame(
  positive = counts_positive / counts_all_terms,
  negative = counts_negative / counts_all_terms
)

sentiments_per_president <- aggregate(relative_sentiment_frequencies, by = list(president = textdata$president), mean)

head(sentiments_per_president)

require(reshape2)

df <- melt(sentiments_per_president, id.vars = "president")

require(ggplot2)

ggplot(data = df, aes(x = president, y = value, fill = variable)) + 
  geom_bar(stat="identity", position=position_dodge()) + coord_flip()

# order by positive sentiments
ggplot(data = df, aes(x = reorder(president, value, head, 1), y = value, fill = variable)) +
geom_bar(stat="identity", position=position_dodge()) + coord_flip()

# order by negative sentiments
ggplot(data = df, aes(x = reorder(president, value, tail, 1), y = value, fill = variable)) + 
  geom_bar(stat="identity", position=position_dodge()) + coord_flip()

terms_to_observe <- c("war", "peace", "health", "terror", "islam", 
                      "threat", "security", "conflict", "job", 
                      "economy", "indian", "afghanistan", "muslim", 
                      "god", "world", "territory", "frontier", "north",
                      "south", "black", "racism", "slavery", "iran")

DTM_reduced <- as.matrix(DTM[, terms_to_observe])

rownames(DTM_reduced) <- ifelse(as.integer(textdata$year) %% 2 == 0, textdata$year, "")

heatmap(t(DTM_reduced), Colv=NA, col = rev(heat.colors(256)), keep.dendro= FALSE, margins = c(5, 10))


DTM.tibble <- convert(DTM[, terms_to_observe],to ="data.frame")

library(magrittr)
library(reshape2)
library(pals)

DTM.tibble %<>% mutate(decade = textdata$decade) %>%
  group_by(decade) %>% 
  summarise_if(is.numeric,sum) %>%
  mutate(decade =  as.Date(substr(decade, 0, 4),format="%Y"))


ggplot(melt(DTM.tibble,id.vars = "decade"), aes(x = decade, y = value, colour = variable)) + 
  geom_smooth(method='glm',formula = y ~ poly(x, 3)) +
  geom_point() +
  scale_color_manual(values=paste0(kelly()[-1], "FF")) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
