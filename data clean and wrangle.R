library(tidyr)
library(tidyverse)
library(dplyr)

injuries <- read.csv("injuries_2010-2020.csv",header = T)
playerstats <- read.csv("all_seasons.csv", header = T)
detail_playerstats <- read.csv("detail_player_data.csv")

nba_detail_playerstats <- subset(detail_playerstats, League == "NBA")
rs_detail_playerstats <- subset(nba_detail_playerstats, Stage == "Regular_Season")
po_detail_playerstats <- subset(nba_detail_playerstats, Stage == "Playoffs")



lbj <- subset(playerstats, player_name == "LeBron James")
kb <- subset(playerstats, player_name == "Kobe Bryant")

relinquished <- subset(injuries, select = c(Date,Team, Relinquished, Notes))
relinquished_lbj <- subset(relinquished, Relinquished == "LeBron James")
relinquished_kb <- subset(relinquished, Relinquished == "Kobe Bryant")

acquired <- subset(injuries, select = c(Date,Team, Acquired, Notes))
    