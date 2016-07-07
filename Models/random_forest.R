#random forest models 
library(ggplot2)
library(randomForest)
library(caret)

#load data
dat <- read.csv("/Users/jamesledoux/Desktop/imputed_kicker_data.csv")

#subset then drop NAs
dat <- subset( dat, select = c(good, temp, windy, wspd, dist, iced, turf, precip) )
dat <- dat[complete.cases(dat),]

#Randomly shuffle the data
dat<-dat[sample(nrow(dat)),]

#hide final test data (80/20 split)
set.seed(3456)
trainIndex <- createDataPartition(dat$X, p = .8,
                                  list = FALSE,
                                  times = 1)

dat <- dat[ trainIndex,]  #training data
test  <- dat[-trainIndex,]  #test data for final evaluation


#Create 5 folds
folds <- cut(seq(1,nrow(dat)),breaks=5,labels=FALSE)


#generate function
#dat$Thunderstorms <- NULL
#dat$away <- NULL
#n <- names(dat)
#f1 <- as.formula(paste("good ~", paste(n[!n %in% "good"], collapse = " + ")))
f1 <- as.formula("good ~ temp + wspd + dist + iced + turf + precip") #continuous wind speed
f2 <- as.formula("good ~ temp + windy + dist + iced + turf + precip")  #categorical wind speed


#parameters to be tested: ntree (no. of trees in forest), model (trying two different feature sets)
ntree_list = list(100, 250, 500, 750)
model_list = list(f1, f2)

MSE_Store = list() #store model mse stores here
#Perform 5 fold cross validation
counter = 0
for(i in 1:length(ntree_list)){
  for(j in 1:length(model_list)){
    model_mse_store = list()  #reset this one for each model tested
    for(k in 1:5){
      #Segement your data by fold using the which() function 
      testIndexes <- which(folds==k,arr.ind=TRUE)
      testData <- dat[testIndexes, ]
      trainData <- dat[-testIndexes, ]
      
      #train model with this set of (ntree, model, fold k)
      rf <- randomForest(model_list[[j]], trainData, ntree=ntree_list[[i]])  

      #measuring on test data: SSR 370.34
      preds <- predict(rf, testData)
      
      testData$preds <- preds
      testData$resids <- (testData$good - testData$preds)^2
      
      #store score of each fold's model
      SSR <- sum(testData$resids)
      MSE <- SSR/nrow(testData)
      model_mse_store[[k]] <- MSE
    }
    #get CVmse for this model, append to score tracker
    model_mse_store <- mean(sapply(model_mse_store,mean))
    counter = counter + 1
    MSE_Store[[counter]] <- model_mse_store
  }
}

#continuous wind results
mses1 = list(0.1355714, 0.1357699, 0.1357104, 0.1355051)
#categorical wind results
mses2 = list(0.1343988, 0.1343337, 0.1343367, 0.1343401)

#best model: function 2, ntree = 500
rf <- randomForest(f2, dat, ntree=500)  

preds <- predict(rf, test)


qplot(x=test$dist, y=preds, color="red") + guides(colour=FALSE) +
  scale_x_continuous(breaks = round(seq(min(15), max(test$dist), by = 5),1)) +
  scale_y_continuous(breaks=round(seq(min(0), max(1), by=.05), 1))


test$preds1 <- preds
test$resids1 <- (test$good - test$preds)^2

SSR <- sum(test$resids1)
MSE = SSR/nrow(test)


#feature importances and other details
importance(rf, type=2)
varImpPlot(rf)
plot(rf, log="y")
