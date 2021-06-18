# Preliminaries
library(lubridate)
library(dplyr)
library(stringi)
library(rstudioapi)  # This is a external library of functions
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())
cat("\014")

getwd()

mov <- read.csv("data/IMDb movies.csv")
View(mov)

rat <- read.csv("data/IMDb ratings.csv")
View(rat)

summary(mov)

colSums(is.na(mov))

nchar(mov[mov$imdb_title_id == "tt0001892", ])

imdb_merged <- merge(mov, rat, by="imdb_title_id")
View(imdb_merged)

#dropping description because of large text values & metascore because of 72K+ NA values
#dropping original title as there is a lot of redundant data
imdb_merged <- subset(imdb_merged, select = -c(original_title, description, metascore))

imdb_merged$date_published <- as.Date(imdb_merged$date_published)
imdb_merged$release_weekday <- wday(imdb_merged$date_published, label=TRUE)

summary(imdb_merged)

#Now, a few columns for age0 have 50K+ NA values, in such cases we will discard the columns associated with this age category
#columns to be subsetted are -
#allgenders_0age_avg_vote, allgenders_0age_votes, males_0age_avg_vote, males_0age_votes, females_0age_avg_vote, females_0age_votes 
imdb_merged <- subset(imdb_merged, select = -c(allgenders_0age_avg_vote, allgenders_0age_votes, males_0age_avg_vote, males_0age_votes, females_0age_avg_vote, females_0age_votes))

View(imdb_merged)

#Also, a lot of values in columns are blank strings ("") instead of NAs, let's replace these values to keep it consistent
imdb_merged$language[imdb_merged$language == ""] <- NA
imdb_merged$budget[imdb_merged$budget == ""] <- NA
imdb_merged$usa_gross_income[imdb_merged$usa_gross_income == ""] <- NA
imdb_merged$worlwide_gross_income[imdb_merged$worlwide_gross_income == ""] <- NA

#also renaming a column that has a typo in it - worlwide_gross_income to worldwide_gross_income
names(imdb_merged)[names(imdb_merged) == "worlwide_gross_income"] <- "worldwide_gross_income"

summary(imdb_merged)

imdb_merged <- subset(imdb_merged, select = -c(budget, usa_gross_income, worldwide_gross_income))

for(i in 1:length(imdb_merged$actors)){
  temp <- imdb_merged$actors[i]
  imdb_merged$actor1[i]<-unlist(strsplit(temp, ","))[1]
  imdb_merged$actor2[i]<-unlist(strsplit(temp, ","))[2]
}
View(imdb_merged[,ncol(imdb_merged)-2])

imdb_merged <- subset(imdb_merged, select = -c(actors))

View(imdb_merged)

imdb_merged$language[imdb_merged$language == "None"] <- NA

imdb_merged$title <- iconv(imdb_merged$title,from="UTF-8",to="ASCII//TRANSLIT")
imdb_merged$director <- iconv(imdb_merged$director,from="UTF-8",to="ASCII//TRANSLIT")
imdb_merged$writer <- iconv(imdb_merged$writer,from="UTF-8",to="ASCII//TRANSLIT")
imdb_merged$production_company <- iconv(imdb_merged$production_company,from="UTF-8",to="ASCII//TRANSLIT")


for(i in 1:length(imdb_merged$director)){
  temp <- imdb_merged$director[i]
  imdb_merged$director[i]<-unlist(strsplit(temp, ","))[1]
}

for(i in 1:length(imdb_merged$writer)){
  temp <- imdb_merged$writer[i]
  imdb_merged$writer[i]<-unlist(strsplit(temp, ","))[1]
}

View(imdb_merged)

write.csv(imdb_merged, "cleaned-imdb.csv", row.names = FALSE)
