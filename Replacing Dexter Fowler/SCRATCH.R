temp <- read.csv("FanGraphs Leaderboard.csv", header = T, sep = ",")
temp$pitch_per_PA <- temp$Pitches / temp$PA
temp$pull_oppo_ratio <- temp$Pull. / temp$Oppo.
temp <- data.frame(temp$Name, temp$pitch_per_PA, temp$BB.K, temp$OBP, temp$ISO, temp$wOBA, temp$O.Swing., 
                   temp$Contact., temp$GB.FB, temp$pull_oppo_ratio, temp$Soft., temp$Med., temp$Hard.)
colnames(temp) <- c("Name", "Pitches/PA", "BB/K", "OBP", "ISO", "wOBA", "O-Swing%", "Contact%", "GB/FB", 
                    "Pull/Oppo", "Soft%", "Med%", "Hard%")
dex <- subset(temp, Name == "Dexter Fowler")
dex <- rbind(dex, dex, dex, dex, dex, dex, dex, dex, dex, dex, dex, dex, dex, dex, dex)
df <- (temp - dex)/ dex
df$Name <- NULL
df <- data.frame(temp$Name, abs(df))

write.csv(temp, file = "temp.csv")
write.csv(df, file = "df.csv")

temp0 <- read.csv("j-hey.csv", header = T, sep = ",")
temp0$pitch_per_PA <- temp0$Pitches / temp0$PA
temp0$pull_oppo_ratio <- temp0$Pull. / temp0$Oppo.
temp0 <- data.frame(temp0$Name, temp0$pitch_per_PA, temp0$BB.K, temp0$OBP, temp0$ISO, temp0$wOBA, temp0$O.Swing., 
                   temp0$Contact., temp0$GB.FB, temp0$pull_oppo_ratio, temp0$Soft., temp0$Med., temp0$Hard.)
colnames(temp) <- c("Name", "Pitches/PA", "BB/K", "OBP", "ISO", "wOBA", "O-Swing%", "Contact%", "GB/FB", 
                    "Pull/Oppo", "Soft%", "Med%", "Hard%")

df0 <- (temp0 - dex)/ dex
df0$temp0.Name <- NULL
df0 <- abs(df0)
write.csv(df0, file = "df0.csv")


temp2 <- read.csv("FanGraphs Leaderboard KS.csv", header = T, sep = ",")
temp2$pitch_per_PA <- temp2$Pitches / temp2$PA
temp2$pull_oppo_ratio <- temp2$Pull. / temp2$Oppo.
temp2 <- data.frame(temp2$Name, temp2$pitch_per_PA, temp2$BB.K, temp2$OBP, temp2$ISO, temp2$wOBA, temp2$O.Swing., 
                   temp2$Contact., temp2$GB.FB, temp2$pull_oppo_ratio, temp2$Soft., temp2$Med., temp2$Hard.)
colnames(temp2) <- c("Name", "Pitches/PA", "BB/K", "OBP", "ISO", "wOBA", "O-Swing%", "Contact%", "GB/FB", 
                    "Pull/Oppo", "Soft%", "Med%", "Hard%")

df2 <- (temp2 - dex)/ dex
df2$Name <- NULL
df2 <- data.frame(temp2$Name, abs(df2))

write.csv(df2, file = "KS.csv")
