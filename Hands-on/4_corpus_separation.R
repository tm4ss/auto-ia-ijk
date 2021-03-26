options(stringsAsFactors = F)
# load & install required packages
pacman::p_load(tidyverse, janitor, quanteda, topicmodels, LDAvis)
# read csv corpus

corona_artikel <- get(load(file = "data/corona_artikel.rda"))

# load dfm
load("data/dfm_50_iter_500.RData")

corona_artikel$quanteda_doc_id <- paste0("text", corona_artikel$id)
corona_artikel <- corona_artikel %>%
  filter(quanteda_doc_id %in% rownames(corona.corpus.tokens.dfm))

# load topic model with 50 K
load("data/topicmodel_50_iter_500.RData")
K <- 50

# extract results
tmResult <- posterior(topicModel)
phi <- tmResult$terms   # named beta in the tutorial
theta <- tmResult$topics


# ldavis
json <- createJSON(
  phi = phi, 
  theta = theta, 
  doc.length = rowSums(corona.corpus.tokens.dfm), 
  vocab = colnames(phi), 
  term.frequency = colSums(corona.corpus.tokens.dfm))
# Render JSON to html output (convert json data to UTF-8 explicitly, sonst Umlautchaos!)
serVis(iconv(json, to="UTF-8"), out.dir = paste0("./ldaviz_K", K), open.browser = T)

# topicnames
topic_names <- read.csv("data/topicmodel_50_iter_500_labels.tsv", sep = "\t", fileEncoding = "UTF-8")

# get topic order from ldavis
# topic proportions
doc.length <- rowSums(corona.corpus.tokens.dfm)
topic.frequency <- colSums(theta * doc.length)
topic.proportion <- topic.frequency/sum(topic.frequency)
o <- order(topic.proportion, decreasing = TRUE)

# reorder topics
phi <- phi[o, ]
rownames(phi) <- topic_names$label
theta <- theta[, o]
colnames(theta) <- topic_names$label

# Nächste Aufgaben
# ----------------

# Dokumentselektion
# A per threshold (z.B. Themenanteil > 30%)
# B per rank 1 (höchstwahrscheinliches Topic)
# Zeitreihenvisualisierung
# - unterschieden sich die Verlaufskurven für A und B?
# Korrelationsanalysen
# - Welche Topics laufen parallel / konträr / abwechselnd?





# Zeitreihe
threshold <- 0.2
bin_theta <- theta >= threshold
months <- as.character(corona_artikel$month)
topic_sums <- aggregate(as.matrix(bin_theta), by = list(month = months), sum)
n_total <- table(as.character(corona_artikel$month))
topic_sums[, 2:ncol(topic_sums)] <- topic_sums[, 2:ncol(topic_sums)] / n_total
counts_df_melted <- reshape2::melt(topic_sums, id.vars = "month")

topic_selection <- topic_names$label[1:5]
counts_df <- counts_df_melted[counts_df_melted$variable %in% topic_selection, ]

ggplot(counts_df, aes(x = month, y = value, group = variable)) +
  geom_line(aes(color = variable)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




# Auswahl zur Darstellung

months_int <- factor(unique(months))

allSlopes <- apply(topic_sums[, 2:ncol(topic_sums)], 2, FUN = function(x) lm(x ~ as.integer(months_int))$coefficients[[2]])

top_decreasing <- sort(allSlopes, decreasing = F)[1:10]
top_increasing <- sort(allSlopes, decreasing = T)[1:10]

topic_selection <- names(top_decreasing)
counts_df <- counts_df_melted[counts_df_melted$variable %in% topic_selection, ]

ggplot(counts_df, aes(x = month, y = value, group = variable)) +
  geom_line(aes(color = variable)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




# RANK 1
corona_artikel$primaryTopic <- NA
for (i in 1:nrow(corona.corpus.tokens.dfm)) {
  topicsPerDoc <- theta[i, ] # select topic distribution for document i
  # get first element position from ordered list
  primaryTopic <- topic_names$label[order(topicsPerDoc, decreasing = TRUE)[1]]
  corona_artikel$primaryTopic[i] <- primaryTopic
}



# Vergleich nach Medientypen

df <- corona_artikel %>%
  select(month, media_type, primaryTopic) %>%
  group_by(month, media_type, primaryTopic) %>%
  summarise(anzahl = n())
ggplot(df, aes(x = month, y = anzahl, group = media_type)) +
  geom_line(aes(color = media_type)) +
  facet_wrap(~primaryTopic) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

