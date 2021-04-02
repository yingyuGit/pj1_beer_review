library(dplyr)
library(tidytext)
library(tidyr)
library(stringr)

# import data
beers <- read.csv("data/beers.csv", stringsAsFactors = FALSE)
reviews <- read.csv("data/reviews.csv", stringsAsFactors = FALSE)
breweries <- read.csv("data/breweries.csv", stringsAsFactors = FALSE)

#filter beers from Ireland only remove notes column
beers_from_IE <- beers %>% 
  filter(country == "IE") %>%
  mutate(beer_id = id) %>%
  subset(., select = -c(id, notes, state, country, retired))

#clean beer data: remove extra space in availability column and remove notes column
beers_from_IE$availability <- str_trim(beers_from_IE$availability) %>% tolower()

#filter breweries in Ireland only and remove notes and state column
breweries_in_IE <- breweries %>% 
  filter(country == "IE") %>% 
  mutate(brewery_id = id) %>%
  subset(., select = -c(id, state,country,notes))

# clean breweries: detect and assign missing values
breweries_in_IE[which(breweries_in_IE$city == ""), ] #find the id of the missing value is 41692
breweries_in_IE$city[breweries_in_IE$id == 41692] <- "Meath"

#clean breweries: formatting city column
pattern <- c("^County\\s+|^Co.\\s+")
breweries_in_IE$city <- str_replace_all(breweries_in_IE$city, pattern, "") %>% 
  str_trim(., side = "both")

# join tables
beer_reviews_IE <- left_join(beers_from_IE, reviews, by='beer_id') %>%
  left_join(., breweries_in_IE, by='brewery_id') %>%
  rename(beer_name = name.x, brewery_name = name.y) %>%
  drop_na()
  

#export cleaned data to a csv file
write.csv(beer_reviews_IE, "data/breweries_reviews_IE.csv")

