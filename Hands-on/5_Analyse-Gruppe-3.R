# install.packages("udpipe")

library(dplyr)
library(udpipe)

# udmodel <- udpipe_download_model(language = "german")
udmodel <- udpipe_load_model(file = udmodel$file_model)

text <- "Peter Müller war früher einmal Ministerpresident des Saarlandes. Ein sehr kleines Bundesland. Saksia Esken ist auch bei Peter Müller dabei."
text <- iconv(text, from = "latin1", to = "UTF-8")

x <- udpipe_annotate(udmodel, x = text)
x <- as.data.frame(x, detailed = TRUE)
View(x)

# PROPN-PROPN patterns
x %>%
  mutate(prev_pos = lag(upos), prev_token = lag(token)) %>%
  filter(upos %in% c("PROPN"), prev_pos %in% c("PROPN")) %>%
  group_by(prev_token, token) %>%
  count()


# ------------------------------------------

corona_artikel <- get(load(file = "data/corona_artikel.rda"))

# load dfm
load("data/dfm_50_iter_500.RData")

corona_artikel$quanteda_doc_id <- paste0("text", corona_artikel$id)
corona_artikel <- corona_artikel %>%
  filter(quanteda_doc_id %in% rownames(corona.corpus.tokens.dfm))

x <- udpipe_annotate(udmodel, x = sample(corona_artikel$article_tt, 2000))
x <- as.data.frame(x, detailed = TRUE)

# PROPN-PROPN patterns
person_counts <- x %>%
  mutate(prev_pos = lag(upos), prev_token = lag(token)) %>%
  filter(upos %in% c("PROPN"), prev_pos %in% c("PROPN")) %>%
  group_by(prev_token, token) %>%
  count() %>%
  arrange(desc(n))
