library(tidyr)
library(tidyverse)
library(dplyr)
library(viridis)
library(hrbrthemes)

injuries <- read.csv("injuries_2010-2020.csv",header = T)
playerstats <- read.csv("all_seasons.csv", header = T)
detail_playerstats <- read.csv("detail_player_data.csv", header = T)

nba_detail_playerstats <- subset(detail_playerstats, League == "NBA")
nbars_detail_playerstats <- subset(nba_detail_playerstats, Stage == "Regular_Season")
nbaps_detail_playerstats <- subset(nba_detail_playerstats, Stage == "Playoffs")

cba_detail_playerstats <- subset(detail_playerstats, League == "Chinese-CBA")
aunbl_detail_playerstats <- subset(detail_playerstats, League == "Australian-NBL")


relinquished <- subset(injuries, select = c(Date,Team, Relinquished, Notes))
acquired <- subset(injuries, select = c(Date,Team, Acquired, Notes))

nba_rs_9900 <- subset(nbars_detail_playerstats, nbars_detail_playerstats$Season == "1999 - 2000")
nba_rs_0001 <- subset(nbars_detail_playerstats, nbars_detail_playerstats$Season == "2000 - 2001")

three_point_attempt_plot <- function(league){
  ggplot(league, aes(x = Season, y = X3PA, fill = Season)) + 
  geom_violin(size=0.5)+
  geom_boxplot(width=0.5, alpha=0.6)+
  scale_fill_viridis(discrete = TRUE, alpha=0.2, option = "D")+
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  )
  #facet_wrap(~Season)
  #geom_violin(width=2, size=0.2)+
  #coord_flip()
  #geom_boxplot(width=0.1, color="grey", alpha=0.2)

}


three_point_attempt_plot(nbars_detail_playerstats)


free_throw_attempt_plot <- function(league){
  ggplot(league, aes(x = Season, y = FTA, fill = Season)) + 
    geom_violin(size=0.5)+
    geom_boxplot(width=0.5, alpha=0.6)+
    scale_fill_viridis(discrete = TRUE, alpha=0.2, option = "D")+
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=14)
    )
  #facet_wrap(~Season)
  #geom_violin(width=2, size=0.2)+
  #coord_flip()
  #geom_boxplot(width=0.1, color="grey", alpha=0.2)
  
}


free_throw_attempt_plot(nbars_detail_playerstats)
