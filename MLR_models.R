#MLR score: SSR 373.7, 371.02 with log included
dat <- read.csv("imputed_kicker_data.csv")

set.seed(3456)
trainIndex <- createDataPartition(dat$X, p = .8,
                                  list = FALSE,
                                  times = 1)

nflTrain <- dat[ trainIndex,]
nflTest  <- dat[-trainIndex,]


fit <- lm(good ~ temp + wspd + dist + iced + turf + precip, data=nflTrain)
summary(fit) 


preds <- predict(fit, nflTest)
qplot(x=nflTest$dist, y=preds, color="red")

nflTest$preds <- preds
nflTest$resids1 <- (nflTest$good - nflTest$preds)^2

SSR1 <- sum(nflTest$resids)

#take log of distance, keep the rest linear
nflTrain$ldist <- log(nflTrain$dist)
nflTest$ldist <- log(nflTest$dist)

fit <- lm(good ~ temp + windy + dist + ldist + iced + turf + precip, data=nflTrain)
summary(fit) 

preds <- predict(fit, nflTest)

nflTest$preds <- preds

#fix impossible answers
nflTest$preds[nflTest$preds > .999 ] <- .99
nflTest$preds[nflTest$preds < .05 ] <- .05

qplot(x=nflTest$dist, y=nflTest$preds, color="red")

nflTest$resids1 <- (nflTest$good - nflTest$preds)^2

SSR1 <- sum(nflTest$resids1)

