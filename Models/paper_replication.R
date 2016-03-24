################## pre-analysis: imputation of missing values using mice package ######################
#dat = read.csv("kicker_data_updated2.csv")
#dat2 <- read.csv("desktop/kicker_data_shortened.csv")
#dat$kicker <- dat2$fkicker
#library(mice)
#imputed <- mice(dat,m=5,maxit=50,meth='pmm')
#imputed <- complete(imputed, 1)
#dat <- imputed
#######################################################################################################
#defining key variables

dat <- read.csv("Documents/Field_Goal_Models/imputed_kicker_data.csv")

dat$cold <- ifelse(dat$temp > 50,0, 1)
dat$windy <- ifelse(dat$wspd >= 10,1, 0)

dat$precip <- ifelse(dat$Rain ==1,1, 0)
dat$precip[dat$Snow == 1] <- 1

dat$mileHigh <- ifelse(dat$Sports.Authority.Field.at.Mile.High ==1, 1, 0)
dat$mileHigh[dat$Mile.High.Stadium == 1] <- 1

#######################################################################################################
library(ggplot2)
library(plyr)

#logistic regression models
#my favorite model (replication + iced)
fit1 <- glm(good ~ dist + cold + precip + windy + iced + turf + mileHigh , data=dat, family = "binomial")
summary(fit1)
summary(fit1)$coefficients


#exact replication (no iced)
fit2 <- glm(good ~ dist + cold + precip + windy + turf + mileHigh , data=dat, family = "binomial")
summary(fit2)
summary(fit2)$coefficients


#checking stadium significance
fitStadiums <- glm(good ~ dist + cold + precip + windy + iced + turf + mileHigh + Adelphia.Coliseum + Giants.Stadium  + Network.Associates.Coliseum + Paul.Brown.Stadium  + Ralph.Wilson.Stadium  +  Texas.Stadium + The.Meadowlands  + Veterans.Stadium , data=dat, family = "binomial")
summary(fitStadiums)
summary(fitStadiums)$coefficients


#show model's expected probabilities of kicks in data set 
probability = predict(fit2, dat, type="response") 
distance = dat$dist
qplot(x=distance, y=probability, color="red") + guides(colour=FALSE) +
  scale_x_continuous(breaks = round(seq(min(15), max(dat$dist), by = 5),1)) +
  scale_y_continuous(breaks=round(seq(min(0), max(1), by=.05), 1)) +
  theme(axis.text.x = element_text(face="bold",
  size=14),  axis.text.y = element_text(face="bold", size=14), 
  axis.title.x = element_text(size=16,face="bold"),
  axis.title.y = element_text(size=16,face="bold"))                                                                                                 


#situational comparison 1 (from MIT paper): Patriots 2002 in NE vs in OAK
game1 <- data.frame(cold=1, precip=1, windy=1, iced=1, turf=0, mileHigh=0)
game1 <- game1[rep(seq_len(nrow(game1)), each=65),]
game1$dist<-seq.int(nrow(game1))

game2 <- data.frame(cold=0, precip=0, windy=0, iced=1, turf=0, mileHigh=0)
game2 <- game2[rep(seq_len(nrow(game2)), each=65),]
game2$dist<-seq.int(nrow(game2))

#non-iced model
predsa = predict(fit2, game1, type="response") 
predsb = predict(fit2, game2, type="response") 
game1$pred <- predsa
game2$pred <- predsb

predsaa <- data.frame(game1$dist)
predsaa$preda <- predsa
predsaa <- rename(predsaa, c("game1.dist"="dist"))

predsbb <- data.frame(game2$dist)
predsbb$predb <- predsb
predsbb <- rename(predsbb, c("game2.dist"="dist"))

predsaa$predb <- predsbb$predb
predsaa <- melt(predsaa, id="dist")

ggplot(data=predsaa,
       aes(x=dist, y=value, colour=variable)) +
  geom_line() + scale_x_continuous(limits = c(15, 65),breaks = round(seq(min(15), max(predsaa$dist), by = 5),1)) + 
  scale_y_continuous(breaks=round(seq(min(0), max(predsaa$value), by=.05), 1)) + 
  scale_colour_manual(
    values = c("predb" = "red", "preda" = "blue"), labels = c("Game at OAK", "Game at NE"), 
    breaks=c("predb","preda")) +
  theme(axis.text.x = element_text(face="bold",
  size=14),  axis.text.y = element_text(face="bold", size=14), 
  axis.title.x = element_text(size=16,face="bold"),
  axis.title.y = element_text(size=16,face="bold"))            


#now, if he were in New England, but was iced
game3 <- data.frame(cold=1, precip=1, windy=1, iced=0, turf=0, mileHigh=0)
game3 <- game3[rep(seq_len(nrow(game3)), each=65),]
game3$dist<-seq.int(nrow(game3))
game4 <- game1
game4$pred <- NULL

predsc = predict(fit1, game3, type="response")
predsd = predict(fit1, game4, type="response")
game3$pred <- predsc
game4$pred <- predsd

predscc <- data.frame(game3$dist)
predscc$predc <- predsc
predscc <- rename(predscc, c("game3.dist"="dist"))
predsdd <- data.frame(game4$dist)
predsdd$predd <- predsd
predsdd <- rename(predsdd, c("game4.dist"="dist"))

predscc$predd <- predsdd$predd
predscc <- melt(predscc, id="dist")

ggplot(data=predscc,
       aes(x=dist, y=value, colour=variable)) +
  geom_line() + scale_x_continuous(limits = c(15, 65),breaks = round(seq(min(15), max(predscc$dist), by = 5),1)) + 
  scale_y_continuous(breaks=round(seq(min(0), max(predscc$value), by=.05), 1)) + 
  scale_colour_manual(
    values = c("predd" = "blue", "predc" = "red"), labels = c("Kicker was not Iced", "Kicker was Iced"), 
    breaks=c("predc","predd"))



#########################################################################################
# part 2:  who are the best kickers of the modern era?
#########################################################################################

#determine points added metric for each kick
#function: 3 points x (outcome (1 for make, 0 for miss) - predicted outcome) 
dat$added_pts_per_attempt <- -999
dat$preds <- predict(fit1, dat, type="response") 

#points added/lost for each attempt:
dat$added_pts_single_attempt <- (dat$good - dat$preds) * 3

#loop through unique values in the kicker column, getting avg points added per kicker
for(i in unique(dat$kicker)){
  df = dat[dat$kicker == i,]
  score <- mean(df$added_pts_single_attempt)
  dat$added_pts_per_attempt[dat$kicker == i] = score
}

#create df: kicker, avg pts added per attempt, and number of attempts 2000-2013
kicker = (unique(dat$kicker))
df <- data.frame(kicker)
df$score <- -999
df$count = 0

for(i in unique(dat$kicker)){
  kickerdf <- dat[dat$kicker == i,]
  count = length(kickerdf$dist)
  df$score[df$kicker == i] = mean(dat$added_pts_per_attempt[dat$kicker == i])
  df$count[df$kicker==i] = count
  df$totmade[df$kicker==i] <- sum(kickerdf$good)  
}

df$pctgood <- df$totmade/df$count

#table ranking by average points added score, min. 30 attempts in this time period
sorted <- df[df$count >= 30,]
sorted <- sorted[order(-sorted$score),] 
rownames(sorted) <- 1:nrow(sorted)
sorted$ScoreRank <- 1:nrow(sorted)

#now table ranking by make percent
sorted1 <- sorted[order(-sorted$pctgood),] 
sorted1$PctRank <- 1:nrow(sorted1)
sorted1$DegreeUnderrated <- sorted1$PctRank - sorted1$ScoreRank
sorted1[order(-sorted1$DegreeUnderrated),] 

#results:
sorted1[order(-sorted1$score),]  #best all time by points added per attempt
sorted1[order(-sorted1$pctgood),]  #best all time by make percent
sorted1[order(-sorted1$DegreeUnderrated),]  #most underrated all time


#best seasons of all time
kicker = unique(dat[c("kicker", "seas")])

dat$season_score = -999
dat$season_count = 0
for(i in unique(kicker$kicker)){
  for(j in kicker$seas[kicker$kicker==i]){
    df = subset(dat , kicker == i & seas == j)
    count = length(df$dist)
    score <- sum(df$added_pts_single_attempt)
    dat$season_score[dat$kicker == i & dat$seas == j] = score
    dat$season_count[dat$kicker == i & dat$seas == j] = count
  }
}

sorted = dat[order(-dat$season_score),] 
sorted = unique(sorted[c("kicker", "seas", "season_score", "season_count")])
sorted <- sorted[sorted$season_count >= 15,]
sorted #results

#################################################################################
#   results summary:

#models:
summary(fit1)  #general model with icing
summary(fit2)  #general model used in MIT paper
summary(fit3)  #model with most significant stadium coeffients

#comparative career metrics:
sorted1

#comparative single-season scores:
sorted
#################################################################################