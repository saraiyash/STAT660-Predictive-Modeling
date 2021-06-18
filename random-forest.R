###############################################################################
#Bagging
#install.packages("randomForest")
library(randomForest)
library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())
cat("\014")

par(mfrow=c(1,2))

imdb = read.csv('cleaned-imdb-2.csv')
imdb = subset(imdb, select = -c(1, 2, 4, 11, 15:37, 44, 45, 60))
imdb$actor1 = as.factor(imdb$actor1)
imdb$director = as.factor(imdb$director)
imdb$writer = as.factor(imdb$writer)
imdb = na.omit(imdb)

set.seed(1)
train = sample(1:nrow(imdb), nrow(imdb)*0.75)

imdb = subset(imdb, select = -c(director, writer, actor1))

bag.imdb=randomForest(avg_vote~.,data=imdb,subset=train,mtry=27,ntree=25,importance=TRUE)
bag.imdb

yhat.bag = predict(bag.imdb,newdata=imdb[-train,])

imdb.test=imdb[-train,"avg_vote"]
plot(yhat.bag, imdb.test)
abline(0,1, col="red")

mean((yhat.bag-imdb.test)^2)


###############################################################################
#Boosting
#install.packages("gbm")
library(gbm)
library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())
cat("\014")

imdb = read.csv('cleaned-imdb-2.csv')
imdb = subset(imdb, select = -c(1, 2, 4, 11, 15:37, 44, 45, 60))

imdb$year = as.numeric(imdb$year)
imdb$genre = as.factor(imdb$genre)
imdb$country = as.factor(imdb$country)
imdb$language = as.factor(imdb$language)
imdb$actor1 = as.factor(imdb$actor1)
imdb$director = as.factor(imdb$director)
imdb$writer = as.factor(imdb$writer)
imdb$release_weekday = as.factor(imdb$release_weekday)
imdb = na.omit(imdb)

set.seed(1)
train = sample(1:nrow(imdb), nrow(imdb)*0.75)

imdb = subset(imdb, select = -c(director, writer, actor1))

imdb = subset(imdb, select = -c(genre, country, language))

boost.imdb=gbm(avg_vote~.,data=imdb[train,],distribution="gaussian",n.trees=1000,interaction.depth=4,shrinkage=0.1)

imdb.test=imdb[-train,"avg_vote"]

yhat.boost=predict(boost.imdb,newdata=imdb[-train,],n.trees=1000)

plot(yhat.boost, imdb.test)
abline(0,1, col="red")

mean((yhat.boost-imdb.test)^2)
