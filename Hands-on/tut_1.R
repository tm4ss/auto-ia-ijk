data_files <- list.files(path = "data/documents", full.names = T, recursive = T)

# View first file paths
head(data_files, 3)

require(readtext)

extracted_texts <- readtext(data_files, docvarsfrom = "filepaths", dvsep = "/")

# View first rows of the extracted texts
head(extracted_texts)

# View beginning of the second extracted text
cat(substr(extracted_texts$text[2] , 0, 300))

write.csv2(extracted_texts, file = "data/text_extracts.csv", fileEncoding = "UTF-8")

# Global options
options(stringsAsFactors = FALSE)

# read csv into a data.frame
textdata <- read.csv("data/sotu.csv", header = TRUE, sep = ";", encoding = "UTF-8")

# dimensions of the data frame
dim(textdata)


# column names of text and metadata
colnames(textdata)

table(textdata$president)

require(quanteda)

sotu_corpus <- corpus(textdata$text, docnames = textdata$doc_id, docvars = data.frame(year = textdata$date))

# have a look on the new corpus object
summary(sotu_corpus)

# getting a single text documents content
cat(texts(sotu_corpus[1]))

# Create a DTM (may take a while)
DTM <- dfm(sotu_corpus)
# Show some information
DTM

# sum columns for word counts
freqs <- colSums(DTM)

# get vocabulary vector
words <- colnames(DTM)

# combine words and their frequencies in a data frame
wordlist <- data.frame(words, freqs)

# re-order the wordlist by decreasing frequency
wordIndexes <- order(wordlist[, "freqs"], decreasing = TRUE)

wordlist <- wordlist[wordIndexes, ]

# show the most frequent words
head(wordlist, 25)

plot(wordlist$freqs , type = "l", lwd=2, main = "Rank frequency Plot", xlab="Rank", ylab ="Frequency")

plot(wordlist$freqs , type = "l", log="xy", lwd=2, main = "Rank-Frequency Plot", xlab="log-Rank", ylab ="log-Frequency")

plot(wordlist$freqs, type = "l", log="xy",lwd=2, main = "Rank-Frequency plot", xlab="Rank", ylab = "Frequency")

englishStopwords <- stopwords("en")

stopwords_idx <- which(wordlist$words %in% englishStopwords)

low_frequent_idx <- which(wordlist$freqs < 10)

insignificant_idx <- union(stopwords_idx, low_frequent_idx)

meaningful_range_idx <- setdiff(1:nrow(wordlist), insignificant_idx)

lines(meaningful_range_idx, wordlist$freqs[meaningful_range_idx], col = "green", lwd=2, type="p", pch=20)

#TIDYVERSE
library(dplyr)
library(tidyr)
library(ggplot2)

# re-order the wordlist by decreasing frequency &
# show the most frequent words
wordlist <- wordlist %>% 
  arrange(desc(freqs)) %>% 
  mutate(rank = row_number()) %>% 
  print(n = 25)


# plot word frequencies
ggplot(wordlist, aes(x = rank, y = freqs)) +
  geom_line() +
  ggtitle("Rank frequency plot") + 
  xlab("Rank") + 
  ylab("Frequency")


meaningful_range_idx <- which(wordlist$words %in% englishStopwords) %>%
  union(which(wordlist$freqs < 10)) %>% 
  setdiff(x = 1:nrow(wordlist), y = .)

# plot with log scales
ggplot(wordlist, aes(x = rank, y = freqs)) +
  geom_line() +
  geom_point(data = wordlist[meaningful_range_idx,] , size = 1.0, color = "green") + 
  ggtitle("Rank frequency plot") +
  scale_x_log10() +
  scale_y_log10() + 
  xlab("log-Rank") + 
  ylab("log-Frequency")

wordlist <- wordlist %>% 
  mutate(stopword = ifelse(is.element(words, englishStopwords), TRUE, FALSE),
         low_frequency = ifelse(freqs < 10, TRUE, FALSE),
         meaningful = ifelse(!stopword & !low_frequency, TRUE, FALSE))


ggplot(wordlist, aes(x = rank, y = freqs)) +
  geom_line() +
  geom_point(aes(color = meaningful)) +
  ggtitle("Rank frequency plot") +
  scale_x_log10() +
  scale_y_log10() + 
  xlab("log-Rank") + 
  ylab("log-Frequency") +
  scale_color_manual(values = c("black", "green")) +
  theme(legend.position = "none")


