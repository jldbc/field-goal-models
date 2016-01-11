#random forest models 
library(ggplot2)
library(randomForest)
dat = read.csv("imputed_kicker_data.csv")

# 80/20 train/test split
set.seed(3456)
trainIndex <- createDataPartition(dat$X, p = .8,
                                  list = FALSE,
                                  times = 1)

nflTrain <- dat[ trainIndex,]
nflTest  <- dat[-trainIndex,]


#testing w/ various numbers of trees
rf1 <- randomForest(good ~ temp + wspd + dist + iced + turf + precip, nflTrain, ntree=10000)
rf2 <- randomForest(good ~ cold + windy + dist + iced + turf + precip, nflTrain, ntree=10000)
rf3 <- randomForest(good ~ temp + wspd + dist + iced + turf + precip, nflTrain, ntree=5000)
rf4 <- randomForest(good ~ cold + windy + dist + iced + turf + precip, nflTrain, ntree=5000)
rf5 <- randomForest(good ~ temp + wspd + dist + iced + turf + precip, nflTrain, ntree=1000)
rf6 <- randomForest(good ~ cold + windy + dist + iced + turf + precip, nflTrain, ntree=1000)
rf7 <- randomForest(good ~ temp + wspd + dist + iced + turf + precip, nflTrain, ntree=500)
rf8 <- randomForest(good ~ cold + windy + dist + iced + turf + precip, nflTrain, ntree=500)

preds1 <- predict(rf7, nflTest)
preds2 <- predict(rf2, nflTrain)

qplot(x=nflTest$dist, y=preds1, color="red") + guides(colour=FALSE) +
  scale_x_continuous(breaks = round(seq(min(15), max(nflTest$dist), by = 5),1)) +
  scale_y_continuous(breaks=round(seq(min(0), max(1), by=.05), 1))


qplot(x=nflTrain$dist, y=preds1, color="red")
qplot(x=nflTrain$dist, y=preds2, color="red")


nflTrain$preds1 <- preds1
nflTrain$preds2 <- preds2
nflTrain$resids1 <- (nflTrain$good - nflTrain$preds1)^2
nflTrain$resids2 <- (nflTrain$good - nflTrain$preds2)^2

SSR1 <- sum(nflTrain$resids1)
SSR2 <- sum(nflTrain$resids2)


#use w/ test data 
preds1 <- predict(rf1, nflTest)
preds2 <- predict(rf2, nflTest)

nflTest$preds1 <- preds1
nflTest$preds2 <- preds2
nflTest$resids1 <- (nflTest$good - nflTest$preds1)^2
nflTest$resids2 <- (nflTest$good - nflTest$preds2)^2

SSR1 <- sum(nflTest$resids1)
SSR2 <- sum(nflTest$resids2)

#5,000 trees:
preds3 <- predict(rf3, nflTest)
preds4 <- predict(rf4, nflTest)

nflTest$preds3 <- preds3
nflTest$preds4 <- preds4
nflTest$resids3 <- (nflTest$good - nflTest$preds3)^2
nflTest$resids4 <- (nflTest$good - nflTest$preds4)^2

SSR3 <- sum(nflTest$resids3)
SSR4 <- sum(nflTest$resids4)

#1,000 trees:
preds5 <- predict(rf5, nflTest)
preds6 <- predict(rf6, nflTest)

nflTest$preds5 <- preds5
nflTest$preds6 <- preds6
nflTest$resids5 <- (nflTest$good - nflTest$preds5)^2
nflTest$resids6 <- (nflTest$good - nflTest$preds6)^2

SSR5 <- sum(nflTest$resids5)
SSR6 <- sum(nflTest$resids6)

#500 trees: 
preds7 <- predict(rf7, nflTest)
preds8 <- predict(rf8, nflTest)

nflTest$preds7 <- preds7
nflTest$preds8 <- preds8
nflTest$resids7 <- (nflTest$good - nflTest$preds7)^2
nflTest$resids8 <- (nflTest$good - nflTest$preds8)^2

SSR7 <- sum(nflTest$resids7)
SSR8 <- sum(nflTest$resids8)

#show results: number of trees did not seem to matter a great deal 
SSR1
SSR2
SSR3
SSR4
SSR5
SSR6
SSR7
SSR8

#feature importances and other details
importance(rf7, type=2)
varImpPlot(rf7)
plot(rf7, log="y")