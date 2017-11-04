

mydata <- read.csv("bbref23teams.csv", header = T)
plot(mydata$WL., mydata$Breakeven.Game)
cor(mydata$WL., mydata$Breakeven.Game)



library(ggplot2)
library(GGally)

# load and check the data
hitters2015 <- read.csv("hitters2015.csv", stringsAsFactors=FALSE)
str(hitters2015)

hitters2016 <- read.csv("hitters2016.csv", stringsAsFactors=FALSE)
str(hitters2016)
#hitters2016$exit_velocity <- as.numeric(hitters2016$exit_velocity)
#hitters2016$launch_angle <- as.numeric(hitters2016$launch_angle)

hitters <- read.csv("hitters2015-2016.csv", stringsAsFactors=FALSE)
str(hitters)

hitters2015$LAeff12 <- hitters2015$exit_velocity/(abs(hitters2015$launch_angle - 12)+1)
hitters2015$LAeff28 <- hitters2015$exit_velocity/(abs(hitters2015$launch_angle - 28)+1)

hitters2016$LAeff12 <- hitters2016$exit_velocity/(abs(hitters2016$launch_angle - 12)+1)
hitters2016$LAeff28 <- hitters2016$exit_velocity/(abs(hitters2016$launch_angle - 28)+1)

hitters$LAeff12 <- hitters$exit_velocity/(abs(hitters$launch_angle - 12)+1)
hitters$LAeff28 <- hitters$exit_velocity/(abs(hitters$launch_angle - 28)+1)

#x.mean <- apply(hitters2015, 2, mean)
#x.sd <- apply(hitters2015, 2, sd)
#hitters2015.std <- t((t(hitters2015)-x.mean)/x.sd) # standardize to have zero mean and unit sd
#apply(hitters2015.std, 2, mean) # check zero mean
#apply(hitters2015.std, 2, sd) # check unit sd

df <- merge(hitters2015, hitters2016, by="player_id")
plot(df$LAeff12.x, df$LAeff12.y, data=df)
plot(df$LAeff28.x, df$LAeff28.y, data=df)

#2015
plot(hitters2015$LAeff28, hitters2015$slg, data=hitters2015)
cor(hitters2015$LAeff28, hitters2015$slg)
plot(hitters2015$LAeff28, hitters2015$iso, data=hitters2015)
cor(hitters2015$LAeff28, hitters2015$iso)
plot(hitters2015$LAeff12, hitters2015$ba, data=hitters2015)
cor(hitters2015$LAeff12, hitters2015$ba)
plot(hitters2015$LAeff12, hitters2015$babip, data=hitters2015)
cor(hitters2015$LAeff12, hitters2015$babip)

ggcorr(hitters2015[,c(6,8,7,9,12:13,22:23)])

#2016
plot(hitters2016$LAeff28, hitters2016$slg, data=hitters2016)
cor(hitters2016$LAeff28, hitters2016$slg)
plot(hitters2016$LAeff28, hitters2016$iso, data=hitters2016)
cor(hitters2016$LAeff28, hitters2016$iso)
plot(hitters2016$LAeff12, hitters2016$ba, data=hitters2016)
cor(hitters2016$LAeff12, hitters2016$ba)
plot(hitters2016$LAeff12, hitters2016$babip, data=hitters2016)
cor(hitters2016$LAeff12, hitters2016$babip)

ggcorr(hitters2016[,c(6,8,7,9,12:13,22:23)])

#total
plot(hitters$LAeff28, hitters$slg, data=hitters)
cor(hitters$LAeff28, hitters$slg)
plot(hitters$LAeff28, hitters$iso, data=hitters)
cor(hitters$LAeff28, hitters$iso)
plot(hitters$LAeff12, hitters$ba, data=hitters)
cor(hitters$LAeff12, hitters$ba)
plot(hitters$LAeff12, hitters$babip, data=hitters)
cor(hitters$LAeff12, hitters$babip)

ggcorr(hitters[,c(6,8,7,9,12:13,22:23)])




