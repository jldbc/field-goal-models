library(caret)
library(ggplot2)
#dat = read.csv("kicker_data_updated2.csv")
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
