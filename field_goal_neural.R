library(neuralnet)
library(deepnet)
dat <- read.csv("imputed_kicker_data.csv")

#80/20 train/test split
set.seed(3456)
trainIndex <- createDataPartition(dat$X, p = .8,
                                  list = FALSE,
                                  times = 1)

nflTrain <- dat[ trainIndex,]
nflTest  <- dat[-trainIndex,]


keeps <- c("temp", "dist", "qtr", "good", "iced", "away", "humd", "mileHigh", "turf", "precip", "windy")
train <- nflTrain[keeps]

#scale the data 
maxs <- apply(train, 2, max) 
mins <- apply(train, 2, min)

scaled <- as.data.frame(scale(train, center = mins, scale = maxs - mins))
train_ <- scaled


## build the neural network 
nn <- neuralnet(good ~ dist + iced + temp + wspd + mileHigh + turf + precip, data=train_, hidden = (5), lifesign = "minimal", 
                       linear.output = TRUE, threshold = 0.04)   

## plot NN architecture and weights 
plot(nn, rep = "best")

#run predictions on test data
test <- subset(train_, select = c("dist", "iced", "temp", "windy", "mileHigh", "turf", "precip"))

nn.results <- compute(nn, test)
results = nn.results[1]

plot(train$dist, nn.results$net.result)



#now for the test data    
test <- nflTest[keeps]
train <- nflTrain[keeps]
maxs <- apply(train, 2, max) 
mins <- apply(train, 2, min)

scaled <- as.data.frame(scale(test, center = mins, scale = maxs - mins))
test_ <- scaled

test <- subset(test_, select = c("dist", "iced", "temp", "windy", "mileHigh", "turf", "precip"))

nn.results <- compute(nn, test)
results = nn.results[1]

plot(nflTest$dist, nn.results$net.result)

#get rid or nonsensical answers
nflTest$pred <- nn.results$net.result
nflTest$pred[nflTest$pred > .999 ] <- .99
nflTest$pred[nflTest$pred < .05 ] <- .05

plot(nflTest$dist, nflTest$pred)

#now evaluate: SSR 371.8
nflTest$resids <- (nflTest$good - nflTest$pred)^2

SSR <- sum(nflTest$resids)


#take two: this time, with a logistic smoothing function :  ssr 372.95 (worse)
#logistic plus linear plus likelihood=TRUE => 371.24 SSR
#cut to three hidden nodes, SSR now 370.53
#switched to generalized 'windy' var - 370.19
scaled <- as.data.frame(scale(train, center = mins, scale = maxs - mins))
train_ <- scaled
nn <- neuralnet(good ~ dist + iced + temp + windy + mileHigh + turf + precip, data=train_, hidden = 3, lifesign = "full", 
                linear.output = TRUE, act.fct = "logistic", threshold = 0.01, likelihood=TRUE)   
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

nflTest$pred <- nn.results$net.result
nflTest$pred[nflTest$pred > .999 ] <- .99
nflTest$pred[nflTest$pred < .05 ] <- .05

plot(nflTest$dist, nflTest$pred)
qplot(x=nflTest$dist, y=nflTest$pred, color="red") + guides(colour=FALSE) +
  scale_x_continuous(breaks = round(seq(min(15), max(nflTest$dist), by = 5),1)) +
  scale_y_continuous(breaks=round(seq(min(0), max(1), by=.05), 1))

nflTest$resids <- (nflTest$good - nflTest$pred)^2

SSR <- sum(nflTest$resids)
SSR


#a little bit deeper now. . . 
#(didn't get deeper networks to outperform the simple one in the end)
nn <- neuralnet(good ~ dist + iced + temp + windy + mileHigh + turf + precip, data=train_, hidden = c(4,3,2), lifesign = "full", 
                linear.output = TRUE, act.fct = "logistic", threshold = 0.03, likelihood=TRUE)   
plot(nn, rep = "best")


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