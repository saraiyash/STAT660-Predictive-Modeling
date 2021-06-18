###############################################################################
# Decision Tree
par(mfrow = c(1, 1))

library(tree)
library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())
cat("\014")

imdb = read.csv('cleaned-imdb-2.csv')
imdb = subset(imdb, select = -c(1, 2, 4, 11, 15:37, 44, 45, 60))
imdb$actor1 = as.factor(imdb$actor1)
imdb$director = as.factor(imdb$director)
imdb$writer = as.factor(imdb$writer)
imdb = subset(imdb, select = -c(director, writer, actor1))
imdb = na.omit(imdb)

set.seed(1)
train = sample(1:nrow(imdb), nrow(imdb)*0.75)

tree.imdb=tree(avg_vote~.,imdb,subset=train)
tree.imdb

plot(tree.imdb)
text(tree.imdb)

yhat=predict(tree.imdb,newdata=imdb[-train,])
imdb.test=imdb[-train,"avg_vote"]

mean((yhat-imdb.test)^2)

###############################################################################
#Tree Pruning
set.seed(1)
cv.imdb=cv.tree(tree.imdb)

cv.imdb
plot(cv.imdb$size,cv.imdb$dev,type='b')

prune.imdb=prune.tree(tree.imdb,best=8) #or 8
plot(prune.imdb)
text(prune.imdb)

yhat=predict(prune.imdb,newdata=imdb[-train,])
imdb.test=imdb[-train,"avg_vote"]

mean((yhat-imdb.test)^2)
