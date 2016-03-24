library(neuralnet)
library(caret)
library(deepnet)

dat <- read.csv("Documents/field_goal_models/imputed_kicker_data.csv")

#kicker dummies
for(t in unique(dat$kicker)) {
     dat[paste("k_",t,sep="")] <- ifelse(dat$kicker==t,1,0)
   }
dat$kicker = NULL
#remove dashes from var names to allow for legal formula 
for(i in 1:length(names(dat))) {
  names(dat)[i] = gsub("-", "", names(dat)[i])
  #print(names(dat)[i])
}
dat$k_WW0200 <- NULL
dat$k_MS0600 <- NULL
dat$Thunderstorms <- NULL
dat$k_BG1200 <- NULL
dat$away <- NULL
n <- names(dat)

f <- as.formula(paste("good ~", paste(n[!n %in% "good"], collapse = " + ")))
f

#80/20 train/test split
set.seed(3456)
trainIndex <- createDataPartition(dat$X, p = .8,
                                  list = FALSE,
                                  times = 1)

nflTrain <- dat[ trainIndex,]
nflTest  <- dat[-trainIndex,]


#original simple model
keeps <- c("temp", "dist", "qtr", "good", "iced", "away", "humd", "mileHigh", "turf", "precip", "windy")
#train <- nflTrain[keeps]
train <- nflTrain

#scale the data 
train$kicker = NULL
maxs <- apply(train, 2, max) 
mins <- apply(train, 2, min)

scaled <- as.data.frame(scale(train, center = mins, scale = maxs - mins))
train_ <- scaled

## build the neural network 
nn <- neuralnet(good ~ dist + iced + temp + windy + mileHigh + turf + precip, data=train_, hidden = (5), lifesign = "full", 
                       linear.output = TRUE, threshold = 0.04)   


nn <- nn.train(f, data=train_, hidden = (c(100, 50, 25)), lifesign = "full", 
                linear.output = TRUE, threshold = 0.04)   


#above example didn't train fast enough. trying deepnet package instead. 
trainx = train_
trainx$good <- NULL
trainy = train_$good
nn <- nn.train(trainx, trainy, hidden = c(100, 50, 25))



## plot NN architecture and weights 
plot(nn, rep = "best")

#run predictions on test data
test <- subset(train_, select = c("dist", "iced", "temp", "windy", "mileHigh", "turf", "precip"))

nn.results <- compute(nn, test)
results = nn.results[1]

plot(train$dist, nn.results$net.result)



#now for the test data    
#test <- nflTest[keeps]
test <- nflTest
train <- nflTrain[keeps]
maxs <- apply(train, 2, max) 
mins <- apply(train, 2, min)

scaled <- as.data.frame(scale(test, center = mins, scale = maxs - mins))
test_ <- scaled
test_$k_WW0200 <- NULL

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