#require(baseballr)
#scrape_statcast_savant_pitcher_all(start_date, end_date)

alldata <- read.csv("savant_data_10-1.csv", header = T, sep = ",")
mydata <- subset(alldata, select=c("game_date","game_type","home_team","away_team","player_name","pitcher","SP_RP","batter","at_bat_number",
                                   "pitch_number","release_speed","effective_speed","events","description","des","type","bb_type","launch_speed",
                                   "launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom",
                                   "babip_value","iso_value","launch_speed_angle"))
mydata <- data.frame(mydata[mydata$woba_denom == 1,])

mydata$game_date <- as.Date(mydata$game_date, "%m/%d/%Y")
mydata$woba_value <- as.numeric(as.character(mydata$woba_value))
mydata$woba_denom <- as.numeric(as.character(mydata$woba_denom))
mydata$babip_value <- as.numeric(as.character(mydata$babip_value))
mydata$iso_value <- as.numeric(as.character(mydata$iso_value))

#mydata$key <- paste(as.character(mydata$pitcher), as.character(mydata$batter), as.character(mydata$game_date), as.character(mydata$at_bat_number))
mydata$key <- paste(as.character(mydata$pitcher), as.character(mydata$batter))
mydata$key <- as.factor(mydata$key)

library(dplyr)
mydata <- mydata %>%
            group_by(key) %>%
            mutate(my_ranks = order(game_date, at_bat_number, decreasing = FALSE))

mydata <- mydata[order(mydata$key, mydata$my_ranks, decreasing = FALSE),]
df1 <- aggregate(cbind(estimated_ba_using_speedangle, estimated_woba_using_speedangle, woba_value, babip_value, iso_value) ~ my_ranks, data = mydata, mean)
df2 <- aggregate(cbind(woba_denom) ~ my_ranks, data = mydata, sum)
all_df <- merge(x = df1, y = df2)

#cleWSdata <- data.frame(mydata[mydata$game_type == "W",])
#cleWSdata <- data.frame(cleWSdata[cleWSdata$game_type == "W",])

SPdata <- data.frame(mydata[mydata$SP_RP == "SP",])
df1 <- aggregate(cbind(estimated_ba_using_speedangle, estimated_woba_using_speedangle, woba_value, babip_value, iso_value) ~ my_ranks, data = SPdata, mean)
df2 <- aggregate(cbind(woba_denom) ~ my_ranks, data = SPdata, sum)
SP_df <- merge(x = df1, y = df2)

RPdata <- data.frame(mydata[mydata$SP_RP == "RP",])
df1 <- aggregate(cbind(estimated_ba_using_speedangle, estimated_woba_using_speedangle, woba_value, babip_value, iso_value) ~ my_ranks, data = RPdata, mean)
df2 <- aggregate(cbind(woba_denom) ~ my_ranks, data = RPdata, sum)
RP_df <- merge(x = df1, y = df2)

round(all_df,3)
round(SP_df,3)
round(RP_df,3)

#did some stuff in Excel...
df <- read.csv("WOBAvals.csv", header = T, sep = ",")
colnames(df)
colnames(df) <- c( "PitcherType","1","2","3","4","5","6","7","8" )

library("reshape2")
mdf <- melt(df, id.vars="PitcherType", value.name="WOBA", variable.name="MatchupCount")

library(ggplot2)
#ggplot(all_df) + geom_bar(aes(x = my_ranks, y = estimated_ba_using_speedangle), position = "dodge", stat="identity")
ggplot(data=mdf, aes(x=MatchupCount, y=WOBA, group = PitcherType, colour = PitcherType)) + geom_hline(yintercept = 0.320, linetype="dotted") + 
  geom_line(size=1.5) + geom_point(size=3, shape=21, fill="white") + ggtitle("WOBA as Matchup Count increases")




