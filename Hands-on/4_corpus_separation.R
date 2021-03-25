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





# Vergleich nach Medientypen
threshold <- 0.3
bin_theta <- theta < 


