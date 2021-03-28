library(knitr)
library(ggplot2)
library(dplyr)
library(magrittr)
library(tidytext)
library(tidyr)
library(corrplot)
library(class)
library(caret)
library(stringr)

# import data
beer_reviews <- read.csv("data/breweries_reviews_IE.csv", stringsAsFactors = FALSE)
unique(beer_reviews$city)
# Irish beer has more than 300 reviews and breweries
beer_reviews %>% 
  group_by(brewery_name, beer_name) %>%
  summarise(review_num = n()) %>%
  filter(review_num >= 300) %>% 
  ggplot(aes(x = reorder(beer_name, -review_num), y = review_num, col=brewery_name)) +
  geom_point() +
  geom_text(aes(beer_name, review_num, label = review_num), nudge_x = 0.85, nudge_y = 0.5, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


#clean breweries: formatting city column
pattern <- c("^County\\s+|^Co.\\s+")
beer_reviews$city <- str_replace_all(beer_reviews$city, pattern, "") %>%
  str_trim(., side = "both")

# find K
beer_reviews_knn <- select(beer_reviews, c(style, city))
head(beer_reviews_knn)
fit <- train(city ~ .,  method = "knn", 
             tuneGrid = data.frame(k = seq(1, 15, 2)), 
             data = beer_reviews_knn)

# 
# for the category refer this site
#https://www.beeradvocate.com/beer/styles/





