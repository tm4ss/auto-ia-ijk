options(stringsAsFactors = F)
# load & install required packages
pacman::p_load(tidyverse, janitor, quanteda)
# read csv corpus

# corona_artikel <- read.csv2("data/articles_coronaberichterstattung_coronabezug_v10.csv", encoding = "UTF-8")

load(file = "data/corona_artikel.rda")

View(data)

library(lubridate)
library(ggplot2)

corona_artikel <- data

# corona_artikel <- data %>% mutate(month = floor_date(as.Date(article_date,"%d.%m.%Y"), "month"))
# create a quanteda corpus object, preprocess tokens, and create a dfm
lemma_data <- read.csv("resources/word_baseform_mwu_fixed.txt", encoding = "UTF-8", sep = ";",header = T)
lemma_data <- lemma_data %>% mutate(word = tolower(V1)) %>% mutate(base_form = tolower(V2))

corona.corpus <- corpus(corona_artikel$article_tt, docvars = corona_artikel %>% select(-article_tt))

corona.corpus.tokens <- corona.corpus %>% tokens(
  remove_separators = T, 
  remove_punct = T,
  remove_numbers = T, 
  remove_symbols = T,
  split_hyphens = F, 
  remove_url = T, 
  verbose=T) %>%  
  quanteda::tokens_tolower() %>%
  tokens_replace(lemma_data$word, lemma_data$base_form, valuetype = "fixed")

corona.corpus.tokens.dfm <- corona.corpus.tokens %>%
  dfm(remove = c(stopwords("de"), LETTERS, letters)) %>%
  dfm_trim(
    min_docfreq = 0.005,
    docfreq_type = "prop",
    
  )
dim(corona.corpus.tokens.dfm)

textstat_frequency(corona.corpus.tokens.dfm)


# remove empty docs
sel_idx <- rowSums(corona.corpus.tokens.dfm) > 0
corona.corpus.tokens.dfm <- corona.corpus.tokens.dfm[sel_idx, ]
corona_artikel <- corona_artikel[sel_idx, ]



# load package topicmodels
require(topicmodels)

# install.packages("ldatuning")

K_to_test <- list(
  "group1" = c(20, 30),
  "group2" = 40,
  "group3" = 50
)
alpha_to_test <- c(0.5, 0.2, 0.1, 0.05)

# number of topics
K <- 20
# compute the LDA model, inference via n iterations of Gibbs sampling
topicModel <- LDA(corona.corpus.tokens.dfm, K, method="Gibbs", 
                  control=list(iter = 500, seed = 1, verbose = 25, alpha = 0.2, delta = 1 / K ))

# load(paste0("data/topicmodel_", K, ".RData"))

# have a look a some of the results (posterior distributions)
tmResult <- posterior(topicModel)

phi <- tmResult$terms   # named beta in the tutorial
theta <- tmResult$topics

require(LDAvis)
json <- createJSON(
  phi = phi, 
  theta = theta, 
  doc.length = rowSums(corona.corpus.tokens.dfm), 
  vocab = colnames(phi), 
  term.frequency = colSums(corona.corpus.tokens.dfm))
# Render JSON to html output (convert json data to UTF-8 explicitly, sonst Umlautchaos!)
serVis(iconv(json, to="UTF-8"), out.dir = paste0("./ldaviz_K", K), open.browser = F)


# save(topicModel, corona.corpus.tokens.dfm, file = paste0("data/topicmodel_", K, ".RData"))


# Tasks:
# - run the topic model for different K
# - interpret the resulting topics with your colleagues
#   - look at the top terms of each topic (vary the lambda parameter in ldavis)
#   - select and read most representative documents per topic
# - write topic labels into csv (topic_id,topic_label)



# get topic order from ldavis
# topic proportions
doc.length <- rowSums(corona.corpus.tokens.dfm)
topic.frequency <- colSums(theta * doc.length)
topic.proportion <- topic.frequency/sum(topic.frequency)
o <- order(topic.proportion, decreasing = TRUE)

# show top docs per topic
top_n <- 10
top_docs <- data.frame()
reordered_k <- o
for (i in 1:K) {
  k <- reordered_k[i]
  selected_docs <- order(theta[, k], decreasing = T)[1:top_n]
  top_docs_per_k <- data.frame(
    k = i,
    id = selected_docs,
    prob = theta[selected_docs, k],
    titel = corona_artikel$article_title[selected_docs],
    text = corona_artikel$article_tt[selected_docs]
  )
  top_docs <- rbind(top_docs, top_docs_per_k)
}
View(top_docs)
