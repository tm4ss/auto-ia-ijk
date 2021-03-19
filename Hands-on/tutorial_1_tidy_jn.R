# auto IA-workshop @IJK, Gregor Wiedemann & Andreas Niekler

# tidyverse version of tutorial 2
# 2021-03-19, JNL


# prerequisites -----------------------------------------------------------

# load & install required packages
pacman::p_load(readtext, tidyverse, janitor, quanteda)


# 1 Reading txt, pdf, html, docx, … ---------------------------------------

# make list of text documents in folder
data_files <- list.files(path = "data/documents", 
                         full.names = TRUE, 
                         recursive = TRUE)

# View first file paths
head(data_files, 3)

# load content of documents into R & make a tibble
extracted_texts <- readtext(data_files, 
                            docvarsfrom = "filepaths", 
                            dvsep = "/") %>% 
  tibble()
  
# View first rows of the extracted texts
head(extracted_texts)

# View beginning of the second extracted text
extracted_texts$text[2] %>% 
  str_sub(0, 300) %>% 
  cat()

# write csv
write_csv2(extracted_texts, file = "data/text_extracts.csv")


# 2 From text to a corpus object ------------------------------------------
options(stringsAsFactors = FALSE)

textdata2 <- read.csv("data/sotu.csv", header = TRUE, sep = ";", encoding = "UTF-8") 
textdata <- read_csv2("data/sotu.csv")

# explore data
dim(textdata)
names(textdata)

# frequency var president
textdata %>% 
  tabyl(president) %>% 
  arrange(desc(n))

# make corpus
sotu_corpus <- corpus(textdata$text, docnames = textdata$doc_id)

summary(sotu_corpus)

# getting a single text documents content
cat(texts(sotu_corpus[1]))


# 3 Text statistics -------------------------------------------------------

# Create a DTM (may take a while)
DTM <- dfm(sotu_corpus)
# Show some information
DTM

# Dimensionality of the DTM
dim(DTM)


# sum columns for word counts
freqs <- colSums(DTM)
# get vocabulary vector
words <- colnames(DTM)
# combine words and their frequencies in a data frame
wordlist <- tibble(words, freqs)


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


# plot with log scales
ggplot(wordlist, aes(x = rank, y = freqs)) +
  geom_line() +
  ggtitle("Rank frequency plot") +
  scale_x_log10() +
  scale_y_log10() + 
  xlab("log-Rank") + 
  ylab("log-Frequency")



englishStopwords <- stopwords("en")

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


# plot with log scales
ggplot(wordlist, aes(x = rank, y = freqs)) +
  geom_line() +
  geom_point(data = wordlist[meaningful_range_idx,], color = "green", size = 1.0) + 
  ggtitle("Rank frequency plot") +
  scale_x_log10() +
  scale_y_log10() + 
  xlab("log-Rank") + 
  ylab("log-Frequency") + 
  theme(legend.position = "none")


