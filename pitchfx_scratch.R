library(moments)
library(ggplot2)
library(gridExtra)
library(pitchRx)
library(dplyr)
library(Hmisc)
library(RSQLite)
library(XML)

#db <- src_sqlite("pitchfx.sqlite3", create = T)
#scrape(start = "2008-01-01", end = Sys.Date(), connect = db$con)

#files <- c("miniscoreboard.xml", "players.xml", "inning/inning_hit.xml")
#scrape(start = "2008-01-01", end = Sys.Date(), suffix = files, connect = db$con)

dbtest <- src_sqlite("pitchfx.sqlite3", create = T)
scrape(start = "2008-04-07", end = "2008-04-07", suffix = "inning/inning_all.xml", connect = dbtest$con)



dbListTables(dbtest$con)
#df <- dbReadTable(dbtest$con, "action")
#df <- dbReadTable(dbtest$con, "atbat")
#df <- dbReadTable(dbtest$con, "coach")
#df <- dbReadTable(dbtest$con, "game")
#df <- dbReadTable(dbtest$con, "hip")
#df <- dbReadTable(dbtest$con, "media")
#df2 <- dbReadTable(dbtest$con, "pitch")
#df <- dbReadTable(dbtest$con, "player")
#df <- dbReadTable(dbtest$con, "po")
#df <- dbReadTable(dbtest$con, "runner")
#df <- dbReadTable(dbtest$con, "umpire")
#dfall <- data.frame(df1, df2)
#head(df)

test <- dbGetQuery(dbtest$con, "SELECT * FROM atbat INNER JOIN pitch ON
                   (atbat.num = pitch.num AND atbat.url = pitch.url)")

db <- src_sqlite("pitchfx.sqlite3", create = T)
scrape(start = "2008-01-01", end = "2008-04-31", connect = db$con)
files <- c("miniscoreboard.xml", "players.xml", "inning/inning_hit.xml")
scrape(start = "2008-01-01", end = "2008-04-31", suffix = files, connect = db$con)
dbListTables(db$con)
dbGetQuery(db$con,'select * from action' )

rm(list = ls())
write.csv(df, file = "test.csv")



# Create SQLite database, then collect and store data in that database
my_db <- src_sqlite("Gameday.sqlite3")
scrape(start = "2013-08-01", end = "2013-08-01", connect = my_db$con)

# Collect other data complementary to PITCHf/x and store in database
files <- c("inning/inning_hit.xml", "miniscoreboard.xml", "players.xml")
scrape(start = "2013-08-01", end = "2013-08-01", connect=my_db$con, suffix = files)

# Simple example to demonstrate database query using dplyr
# Note that 'num' and 'gameday_link' together make a key that allows us to join these tables
locations <- select(tbl(my_db, "pitch"), px, pz, des, num, gameday_link)
names <- select(tbl(my_db, "atbat"), pitcher_name, batter_name, num, gameday_link)
que <- inner_join(locations, filter(names, batter_name == "Paul Goldschmidt"),
                  by = c("num", "gameday_link"))
que$query #refine sql query if you'd like
pitchfx <- collect(que) #submit query and bring data into R



atbats <- tbl(db, 'atbat')
pitches <- tbl(db, 'pitch')
mydata <- inner_join(pitches, atbats, by = c('num', 'gameday_link'))
mydata <- group_by(mydata, pitch_type)
mydata %>% count(pitch_type)
head(mydata)




atbats16 <- tbl(db, 'atbat') %>%
  filter(date >= '2016_01_01' & date <= '2017_01_01') 
mydata <- inner_join(pitches, atbats16, by = c('num', 'gameday_link'))


# https://baseballwithr.wordpress.com/2014/04/13/modifying-and-querying-a-pitchfx-database-with-dplyr/


df <- dbGetQuery(db$con, 'SELECT gameday_link, game_type FROM game WHERE home_team_name = "Cubs" AND game_type <> "E" AND game_type <> "S"')
df <- dbGetQuery(db$con, 'SELECT * FROM atbat WHERE date >= "2016_01_01" AND date <= "2017_01_01"')

atbat <- dbGetQuery(db$con, 'SELECT * FROM atbat')
df <- dbGetQuery(db$con, 'SELECT * FROM pitch WHERE pitch_type = "AB"')
pitchAB <- inner_join(df, atbat, by = c('num', 'gameday_link'))



# Put histograms on the diagonal
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

# put (absolute) correlations on the upper panels, with size proportional to the correlations.
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(mydata[2:16], lower.panel = panel.smooth, upper.panel = panel.cor, cex = 1.5, pch = 20, bg = "steel blue",
      diag.panel = panel.hist, cex.labels = NULL, font.labels = 1, main = "Simple Scatterplot Matrix", na.action = na.omit)

pairs(mydata[2:16], pch = 21, lower.panel = panel.smooth, upper.panel = panel.cor, diag.panel = panel.hist)


db <- src_sqlite("pitchfx.sqlite3")
files <- c("miniscoreboard.xml", "players.xml", "inning/inning_hit.xml")

# PITCHf/x data was downloaded from the MLB website using the below lines of code.
# It was done through Northwestern's SSCC connection and broken up into many pieces to manage the volume of data.
# The resulting database was transferred back to the local PC using Cyberduck and was read into R from there.
# Code:
#scrape(start = "2008-01-01", end = "2016-08-21", connect = db$con)
#scrape(start = "2008-01-01", end = "2016-08-21", suffix = files, connect = db$con)

ids <- db %>% tbl("game") %>% 
  select(game_pk, gameday_link, game_type) %>% 
  filter(game_type != "E" & game_type != "S") %>% collect()

devtools::source_gist("0adc410ef37e521c4f47")


grab_bb <- function(id) {
  feed <- sprintf("http://statsapi.mlb.com/api/v1/game/%s/feed/color.json", id)
  t <- jsonlite::fromJSON(feed, simplifyVector = FALSE)
  get_des <- function(x) {
    des <- x$data$description
    if (is.null(des)) "" else des
  }
  get_id <- function(x) {
    id <- x$id
    if (is.null(id)) "" else id
  }
  des <- unlist(lapply(t$items, get_des))
  # keep just the descriptions of play results
  # (this should hopefully match the number of at-bats)
  des <- des[unlist(lapply(t$items, get_id)) == "playResult"]
  
  idx <- grepl("[0-9]{2,3} mph", des)
  exit <- as.numeric(sub(".* ([0-9]{2,3}) mph.*", "\\1", des[idx]))
  
  idx2 <- grepl("[0-9]{1,3} feet", des)
  dis <- as.numeric(sub(".* ([0-9]{1,3}) feet.*", "\\1", des[idx2]))
  
  # start a matrix of missing values with one column for exit velocities,
  # one column for distance traveled (and one row for each play)
  m <- matrix(rep(NA, 2 * length(des)), ncol = 2)
  m[idx, 1] <- exit
  m[idx2, 2] <- dis
  df <- setNames(data.frame(m), c("exit", "distance"))
  N <- nrow(df)
  if (N > 0) {
    df$game_pk <- id
    df$num <- seq_len(N)
  }
  df
}

bbs <- plyr::ldply(ids[, "game_pk"], grab_bb)
head(bbs)


#  Check data
db
# src:  sqlite 3.8.6 [pitchfx.sqlite3]
# tbls: action, atbat, coach, game, hip, media, pitch, player, po, runner, umpire
dbListFields(db$con, "action")
dbListFields(db$con, "atbat")
dbListFields(db$con, "coach")
dbListFields(db$con, "game")
dbListFields(db$con, "hip")
dbListFields(db$con, "media")
dbListFields(db$con, "pitch")
dbListFields(db$con, "player")
dbListFields(db$con, "po")
dbListFields(db$con, "runner")
dbListFields(db$con, "umpire")


atbats <- tbl(db, 'atbat')
pitches <- tbl(db, 'pitch') %>%
  filter(pitch_type != "<NA>") %>% collect()
mydata <- inner_join(pitches, atbats, by = c('num', 'gameday_link'))
mydata <- group_by(mydata, pitch_type)




test <- summarise(mydata, G = mean(G), n = n())


test <- mydata %>% count(pitch_type)
test <- data.frame(test)
head(mydata)


n(mydata)

mean_velo <- aggregate(start_speed ~ pitch_type, data = mydata, mean, nfrequency = -1)
mean_velo <- mean_velo[order(mean_velo$start_speed),]
plot <- ggplot(data=mean_velo ,aes(x=pitch_type, y=start_speed, group=pitch_type, colour =
                                     pitch_type))+geom_line()+geom_point(size=4)+
  ggtitle("Plot ")

tapply(mydata$start_speed, mydata$pitch_type, mean)

head(mean_velo$start_speed)


set.seed(121688)
sample(mydata, 10, replace=FALSE)


