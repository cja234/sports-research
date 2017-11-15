### Rebuttal to Bullpenning: Part 2 - Chris Anderson 11/14/2017 ###

# Downloaded starting pitcher data from Baseball Savant
# Player Type: Pitcher/ Season: 2017/ Season Type: Playoffs/ Position: SP

SPdata <- read.csv("savant_data_2017SP.csv", header = T, sep = ",")
SPdata$SP_RP <- "SP"
mydata <- subset(SPdata, select=c("game_date","game_type","home_team","away_team","player_name","pitcher","SP_RP","batter","at_bat_number",
                                   "pitch_number","release_speed","effective_speed","events","description","des","type","bb_type","launch_speed",
                                   "launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom",
                                   "babip_value","iso_value","launch_speed_angle"))

# Extract one row per AB - the outcome record + some data manipulations
mydata <- data.frame(mydata[mydata$woba_denom == 1,])
mydata$game_date <- as.Date(mydata$game_date, "%Y-%m-%d")
mydata$woba_value <- as.numeric(as.character(mydata$woba_value))
mydata$woba_denom <- as.numeric(as.character(mydata$woba_denom))
mydata$babip_value <- as.numeric(as.character(mydata$babip_value))
mydata$iso_value <- as.numeric(as.character(mydata$iso_value))

# Create key to ID each unique pitcher-batter matchup
mydata$key <- paste(as.character(mydata$pitcher), as.character(mydata$batter))
mydata$key <- as.factor(mydata$key)

# Add time through order across entire playoffs (my_ranks column)
library(dplyr)
mydata <- mydata %>%
  group_by(key) %>%
  mutate(my_ranks = order(game_date, at_bat_number, decreasing = FALSE))

mydata <- mydata[order(mydata$key, mydata$my_ranks, decreasing = FALSE),]
df1 <- aggregate(cbind(estimated_ba_using_speedangle, estimated_woba_using_speedangle, woba_value, babip_value, iso_value) ~ my_ranks, data = mydata, mean)
df2 <- aggregate(cbind(woba_denom) ~ my_ranks, data = mydata, sum)
SP_df <- merge(x = df1, y = df2)
SP_table <- round(SP_df,3)
# write.csv(SP_table, file = "SP_table.csv")


# Downloaded relief pitcher data from Baseball Savant
# Player Type: Pitcher/ Season: 2017/ Season Type: Playoffs/ Position: RP

RPdata <- read.csv("savant_data_2017RP.csv", header = T, sep = ",")
RPdata$SP_RP <- "RP"
mydata <- subset(RPdata, select=c("game_date","game_type","home_team","away_team","player_name","pitcher","SP_RP","batter","at_bat_number",
                                  "pitch_number","release_speed","effective_speed","events","description","des","type","bb_type","launch_speed",
                                  "launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom",
                                  "babip_value","iso_value","launch_speed_angle"))

# Extract one row per AB - the outcome record + some data manipulations
mydata <- data.frame(mydata[mydata$woba_denom == 1,])
mydata$game_date <- as.Date(mydata$game_date, "%Y-%m-%d")
mydata$woba_value <- as.numeric(as.character(mydata$woba_value))
mydata$woba_denom <- as.numeric(as.character(mydata$woba_denom))
mydata$babip_value <- as.numeric(as.character(mydata$babip_value))
mydata$iso_value <- as.numeric(as.character(mydata$iso_value))

# Create key to ID each unique pitcher-batter matchup
mydata$key <- paste(as.character(mydata$pitcher), as.character(mydata$batter))
mydata$key <- as.factor(mydata$key)

library(dplyr)
mydata <- mydata %>%
  group_by(key) %>%
  mutate(my_ranks = order(game_date, at_bat_number, decreasing = FALSE))

mydata <- mydata[order(mydata$key, mydata$my_ranks, decreasing = FALSE),]
df1 <- aggregate(cbind(estimated_ba_using_speedangle, estimated_woba_using_speedangle, woba_value, babip_value, iso_value) ~ my_ranks, data = mydata, mean)
df2 <- aggregate(cbind(woba_denom) ~ my_ranks, data = mydata, sum)
RP_df <- merge(x = df1, y = df2)
RP_table <- round(SP_df,3)
# write.csv(SP_table, file = "SP_table.csv")

SP_table
RP_table



# Repeat for all data

alldata <- rbind(SPdata, RPdata)
