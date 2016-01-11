dat <- read.csv("imputed_kicker_data.csv")

#80/20 train/test split
set.seed(3456)
trainIndex <- createDataPartition(dat$X, p = .8,
                                  list = FALSE,
                                  times = 1)

nflTrain <- dat[ trainIndex,]
nflTest  <- dat[-trainIndex,]

#FIRST:LOGISTIC
fit1 <- glm(good ~ dist + cold + precip + windy + iced + turf + mileHigh , data=nflTrain, family = "binomial")

#measuring on test data: SSR 370.34
preds1 <- predict(fit1, nflTest, type="response") 
nflTest$preds1 <- preds1

#NEXT: NEURAL NET
library(neuralnet)

keeps <- c("temp", "dist", "qtr", "good", "iced", "away", "humd", "mileHigh", "turf", "precip", "windy")
train <- nflTrain[keeps]

#scale the data 

maxs <- apply(train, 2, max) 
mins <- apply(train, 2, min)

scaled <- as.data.frame(scale(train, center = mins, scale = maxs - mins))
train_ <- scaled

scaled <- as.data.frame(scale(train, center = mins, scale = maxs - mins))
train_ <- scaled
nn <- neuralnet(good ~ dist + iced + temp + windy + mileHigh + turf + precip, data=train_, hidden = 3, lifesign = "full", 
                linear.output = TRUE, act.fct = "logistic", threshold = 0.04, likelihood=TRUE)   
plot(nn, rep = "best")

test <- nflTest[keeps]
train <- nflTrain[keeps]
maxs <- apply(train, 2, max) 
mins <- apply(train, 2, min)

scaled <- as.data.frame(scale(test, center = mins, scale = maxs - mins))
test_ <- scaled

test <- subset(test_, select = c("dist", "iced", "temp", "windy", "mileHigh", "turf", "precip"))

nn.results <- compute(nn, test)
results = nn.results[1]

#get rid or nonsensical answers
nflTest$pred <- nn.results$net.result
nflTest$pred[nflTest$pred > .999 ] <- .99
nflTest$pred[nflTest$pred < .05 ] <- .05

plot(nflTest$dist, nflTest$pred)

nflTest$resids <- (nflTest$good - nflTest$pred)^2

SSR <- sum(nflTest$resids)
SSR

#COMBINE THE TWO MOST SUCCESSFUL MODELS  -- 369.78
nflTest$combinedPred <- (nflTest$pred + nflTest$preds1)/2
nflTest$resids <- (nflTest$good - nflTest$combinedPred)^2
SSR <- sum(nflTest$resids)

qplot(x=nflTest$dist, y=nflTest$combinedPred, color="red") + guides(colour=FALSE) +
  scale_x_continuous(breaks = round(seq(min(15), max(nflTest$dist), by = 5),1)) +
  scale_y_continuous(breaks=round(seq(min(0), max(1), by=.05), 1))

#adding random forest and/or MLR model made this predict worse


