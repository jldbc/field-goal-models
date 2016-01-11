#regresssion models

library(caret)
library(ggplot2)
dat = read.csv("kicker_data_updated2.csv")
dat = read.csv("imputed_kicker_data.csv")


dat$cold <- ifelse(dat$temp > 40,0, 1)
dat$warm <- ifelse(dat$temp >= 70,1, 0)

dat$windy <- ifelse(dat$wspd >= 12,1, 0)

dat$precip <- ifelse(dat$Rain ==1,1, 0)
dat$precip[dat$Snow == 1] <- 1

dat$mileHigh <- ifelse(dat$Sports.Authority.Field.at.Mile.High ==1, 1, 0)
dat$mileHigh[dat$Mile.High.Stadium == 1] <- 1

dat$humid <- ifelse(dat$humd >= 80,1, 0)


set.seed(3456)
trainIndex <- createDataPartition(dat$X, p = .8,
                                  list = FALSE,
                                  times = 1)

nflTrain <- dat[ trainIndex,]
nflTest  <- dat[-trainIndex,]


ggplot(dat[dat$cold == 1, ]) +
  geom_smooth(aes(x=dist, y=good), size = 1) 
ggplot(dat[dat$warm == 1, ]) +
  geom_smooth(aes(x=dist, y=good), size = 1) 

#cold vs warm success rates 
ggplot(dat) + geom_smooth(aes(x=dist, y=good), size = 1, data = dat[dat$cold == 1, ], ) + 
  geom_smooth(aes(x=dist, y=good), size = 1, data = dat[dat$cold == 0, ], colour="red") + xlim(15, 65) + ylim(0,1)

#the Mile High effect
ggplot(dat) + geom_smooth(aes(x=dist, y=good), size = 1, data = dat[dat$mileHigh ==1,], ) + 
  geom_smooth(aes(x=dist, y=good), size = 1, data = dat, colour="red") + xlim(15, 65) + ylim(0,1)

#icing the kicker
ggplot(dat) + geom_smooth(aes(x=dist, y=good), size = 1, data = dat, ) + 
  geom_smooth(aes(x=dist, y=good), size = 1, data = dat[dat$iced == 1, ], colour="red") + xlim(15, 65) + ylim(0,1)

#humidity
ggplot(dat) + geom_smooth(aes(x=dist, y=good), size = 1, data = dat, ) + 
  geom_smooth(aes(x=dist, y=good), size = 1, data = dat[dat$humid == 1, ], colour="red") + xlim(15, 65) + ylim(0,1)

#snow
ggplot(dat) + geom_smooth(aes(x=dist, y=good), size = 1, data = dat, ) + 
  geom_smooth(aes(x=dist, y=good), size = 1, data = dat[dat$Snow == 1, ], colour="red") + xlim(15, 65) + ylim(0,1)

#cloudy
ggplot(dat) + geom_smooth(aes(x=dist, y=good), size = 1, data = dat, ) + 
  geom_smooth(aes(x=dist, y=good), size = 1, data = dat[dat$Cloudy == 1, ], colour="red") + xlim(15, 65) + ylim(0,1)

#rain 
ggplot(dat) + geom_smooth(aes(x=dist, y=good), size = 1, data = dat, ) + 
  geom_smooth(aes(x=dist, y=good), size = 1, data = dat[dat$Rain == 1, ], colour="red")+ xlim(15, 65) + ylim(0,1)


#rain 
ggplot(dat) + geom_smooth(aes(x=dist, y=good), size = 1, dat[dat$mileHigh == 0, ], ) + 
  geom_smooth(aes(x=dist, y=good), size = 1, data = dat[dat$mileHigh == 1, ], colour="red")+ xlim(15, 65) + ylim(0,1)

#precipitation (snow and/or rain)
ggplot(dat) + geom_smooth(aes(x=dist, y=good), size = 1, data = dat[dat$precip == 0, ], ) + 
  geom_smooth(aes(x=dist, y=good), size = 1, data = dat[dat$precip == 1, ], colour="red")+ xlim(15, 65) + ylim(0,1)

#sunny
ggplot(dat) + geom_smooth(aes(x=dist, y=good), size = 1, data = dat, ) + 
  geom_smooth(aes(x=dist, y=good), size = 1, data = dat[dat$Sunny == 1, ], colour="red") + xlim(15, 65) + ylim(0,1)

#dome vs outdoor
ggplot(dat) + geom_smooth(aes(x=dist, y=good), size = 1, data = dat[dat$Dome == 0, ], ) + 
  geom_smooth(aes(x=dist, y=good), size = 1, data = dat[dat$Dome == 1, ], colour="red")+ xlim(15, 65) + ylim(0,1)

#turf
ggplot(dat) + geom_smooth(aes(x=dist, y=good), size = 1, data = dat[dat$turf == 0, ], ) + 
  geom_smooth(aes(x=dist, y=good), size = 1, data = dat[dat$turf == 1, ], colour="red")+ xlim(15, 65) + ylim(0,1)

#windspeed
ggplot(dat) + geom_smooth(aes(x=dist, y=good), size = 1, data = dat[dat$windy==0, ], ) + 
  geom_smooth(aes(x=dist, y=good), size = 1, data = dat[dat$windy ==1, ], colour="red")+ xlim(15, 65) + ylim(0,1)

#all
ggplot(dat) + geom_smooth(aes(x=dist, y=good), size = 1, data = dat, ) +
  scale_x_continuous(breaks = round(seq(min(15), max(65), by = 5),1)) +
  scale_y_continuous(breaks=round(seq(min(0), max(1), by=.05), 1)) + xlim(15, 65)





#linear
fit <- lm(good ~ dist + cold + precip + windy + iced + humd + turf + mileHigh , data=nflTrain)
summary(fit) # show results

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)


#logistic
#first, the kitchen sink:
fit3 <- glm(good ~ dist + cold + precip + windy + iced + humd + turf + mileHigh + week + ou + sprv + ptdiff + X3COM.Park + Adelphia.Coliseum + Alltel.Stadium + Arrowhead.Stadium + Azteca.Stadium + Bank.of.America.Stadium + Candlestick.Park + CenturyLink.Field + Cleveland.Browns.Stadium + Cowboys.Stadium + Dolphins.Stadium + Ericsson.Stadium + Everbank.Field + FedEx.Field + FirstEnergy.Stadium + Foxboro.Stadium + Giants.Stadium + Gillette.Stadium + Heinz.Field + Husky.Stadium + Jacksonville.Municipal.Stadium + LP.Field + Lambeau.Field + Land.Shark.Stadium + Lincoln.Financial.Field + Louisiana.Superdome + Lucas.Oil.Stadium + M.T.Bank.Stadium + Memorial.Stadium + MetLife.Stadium + Monster.Park + Network.Associates.Coliseum + O.co.Coliseum + Oakland.Alameda.County.Coliseum + PSINet.Stadium + Paul.Brown.Stadium + Pro.Player.Stadium + Qualcomm.Stadium + Qwest.Field + Ralph.Wilson.Stadium + Ravens.Stadium + Raymond.James.Stadium + Reliant.Stadium + Seahawks.Stadium + Snapdragon.Stadium + Soldier.Field + Sun.Devil.Stadium + SunLife.Stadium + TCF.Bank.Stadium + Texas.Stadium + The.Meadowlands + Three.Rivers.Stadium + University.of.Phoenix.Stadium + Veterans.Stadium , data=nflTrain, family = "binomial")
summary(fit3)

#now only the significant stadiums
fit4 <- glm(good ~ dist + cold + precip + windy + iced + humd + turf + mileHigh + week + ou + sprv + ptdiff + X3COM.Park + Adelphia.Coliseum + Alltel.Stadium + Arrowhead.Stadium + Azteca.Stadium + Candlestick.Park + Cleveland.Browns.Stadium + Ericsson.Stadium + FedEx.Field + Foxboro.Stadium + Giants.Stadium + Gillette.Stadium + Heinz.Field + Husky.Stadium + Lambeau.Field + MetLife.Stadium + Network.Associates.Coliseum + PSINet.Stadium + Paul.Brown.Stadium + Qualcomm.Stadium + Qwest.Field + Ralph.Wilson.Stadium + Seahawks.Stadium + Snapdragon.Stadium + Soldier.Field + Sun.Devil.Stadium + Texas.Stadium + The.Meadowlands + Three.Rivers.Stadium + Veterans.Stadium , data=nflTrain, family = "binomial")
summary(fit4)

fit5 <- glm(good ~ dist + cold + precip + windy + iced + humd + turf + mileHigh + week + ou + sprv + ptdiff + X3COM.Park + Adelphia.Coliseum  + FedEx.Field + Giants.Stadium + Gillette.Stadium + Heinz.Field + Lambeau.Field  + Network.Associates.Coliseum + Paul.Brown.Stadium  + Qwest.Field + Ralph.Wilson.Stadium  + Sun.Devil.Stadium + Texas.Stadium + The.Meadowlands  + Veterans.Stadium , data=nflTrain, family = "binomial")
summary(fit5)

fit6 <- glm(good ~ dist + cold + precip + windy + iced + humd + turf + mileHigh + week + ou + sprv + ptdiff  + Adelphia.Coliseum + Giants.Stadium + Gillette.Stadium + Heinz.Field  + Network.Associates.Coliseum + Paul.Brown.Stadium  + Qwest.Field + Ralph.Wilson.Stadium  + Sun.Devil.Stadium + Texas.Stadium + The.Meadowlands  + Veterans.Stadium , data=nflTrain, family = "binomial")
summary(fit6)

fit7 <- glm(good ~ dist + cold + precip + windy + iced + turf + mileHigh + week + ou + Adelphia.Coliseum + Giants.Stadium + Heinz.Field  + Network.Associates.Coliseum + Paul.Brown.Stadium  + Qwest.Field + Ralph.Wilson.Stadium  + Sun.Devil.Stadium + Texas.Stadium + The.Meadowlands  + Veterans.Stadium , data=nflTrain, family = "binomial")
summary(fit7)



fit2 <- glm(good ~ dist + cold + precip + windy + iced + humd + turf + mileHigh , data=nflTrain, family = "binomial")
summary(fit2)

#drop foxboro, husky,Seahawks Stad, snapdragon, three rivers, 

preds = predict(fit2, nflTrain, type="response") 
preds2 = predict(fit7, nflTrain, type="response") 
preds3 <- data.frame(preds2)

qplot(x=nflTrain$dist, y=preds, color="red")
plot(nflTrain$dist, preds)
plot(nflTrain$dist, preds2)
ggplot(nflTrain) + geom_smooth(aes(x=dist, y=preds2), size = 1, data = nflTrain, ) 


#compare overall success rate in model vs reality 
preds1 = na.omit(preds)
mean(preds1)
mean(nflTrain$good)

#now to test on unseen data
preds = predict(fit2, nflTest, type="response") 
qplot(x=nflTest$dist, y=preds, color="red")


#comparison 
preds1 = na.omit(preds)
mean(preds1)
mean(nflTest$good)







