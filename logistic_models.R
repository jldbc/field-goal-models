dat <- read.csv("imputed_kicker_data.csv")

#80/20 train/test split
set.seed(3456)
trainIndex <- createDataPartition(dat$X, p = .8,
                                  list = FALSE,
                                  times = 1)

nflTrain <- dat[ trainIndex,]
nflTest  <- dat[-trainIndex,]

#logistic regression model. First with icing of kicker, second the Clark replication
fit1 <- glm(good ~ dist + cold + precip + windy + iced + turf + mileHigh , data=nflTrain, family = "binomial")
fit2 <- glm(good ~ dist + cold + precip + windy + turf + mileHigh , data=nflTrain, family = "binomial")

summary(fit1)
summary(fit1)$coefficients

#show model's expected probabilities of kicks in data set 
preds = predict(fit1, nflTrain, type="response") 
qplot(x=nflTrain$dist, y=preds, color="red") + guides(colour=FALSE) +
  scale_x_continuous(breaks = round(seq(min(15), max(nflTrain$dist), by = 5),1)) +
  scale_y_continuous(breaks=round(seq(min(0), max(1), by=.05), 1))


#measuring on test data: SSR 370.34
preds1 <- predict(fit1, nflTest, type="response") 

nflTest$preds1 <- preds1
nflTest$resids1 <- (nflTest$good - nflTest$preds1)^2

SSR <- sum(nflTest$resids1)







#