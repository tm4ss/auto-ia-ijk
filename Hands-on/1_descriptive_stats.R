options(stringsAsFactors = F)

# read csv corpus

corona_artikel <- read.csv2("data/articles_coronaberichterstattung_coronabezug_v10.csv", encoding = "UTF-8")

# print 1st article
corona_artikel[1, ]

# how many articles per media?
table(corona_artikel$media_name)

# how many articles per media type?


# plot number of articles over time (e.g. line plot, or bar plot with days, weeks, or months)
# - hint: ggplot2 package + geom_bar/geom_line functions

# plot a line plot version of the previous plot with different lines for each media type
# - hint: use the group or fill parameter of the ggplot function


# optional: for which media do we have comments in the corpus
# optional: how many comments per media do we have and which is the most commented article?


# create a quanteda corpus object, preprocess tokens, and create a dfm


# how many words containing "covid" or "corona" do you count?