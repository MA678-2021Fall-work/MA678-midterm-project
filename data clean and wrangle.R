library(tidyr)
library(tidyverse)
library(dplyr)
library(viridis)
library(hrbrthemes)

injuries <- read.csv("injuries_2010-2020.csv",header = T)
playerstats <- read.csv("all_seasons.csv", header = T)
detail_playerstats <- read.csv("detail_player_data.csv", header = T)



nba_detail_playerstats <- subset(detail_playerstats, League == "NBA")

nba_detail_playerstats <- nba_detail_playerstats %>% 
  mutate(PER = (FGM * 85.910 + STL * 53.897 + X3PM * 51.757 + FTM * 46.845 + BLK * 39.190 + ORB * 39.190 + AST * 34.677 + DRB * 14.707 - PF * 17.174 - (FTA - FTM) * 20.091 - (FGA - FGM) * 39.190 - TOV * 53.897) * (1/MIN))


nbars_detail_playerstats <- subset(nba_detail_playerstats, Stage == "Regular_Season")
nbaps_detail_playerstats <- subset(nba_detail_playerstats, Stage == "Playoffs")



cba_detail_playerstats <- subset(detail_playerstats, League == "Chinese-CBA")
aunbl_detail_playerstats <- subset(detail_playerstats, League == "Australian-NBL")


relinquished <- subset(injuries, select = c(Date,Team, Relinquished, Notes))
acquired <- subset(injuries, select = c(Date,Team, Acquired, Notes))

average_nba_detail_playerstats <- nba_detail_playerstats %>%
  mutate(MPG = MIN/GP,
         PPG = PTS/GP,
         RPG = REB/GP,
         APG = AST/GP,
         TOVPG = TOV/GP,
         PFPG = PF/GP,
         STLPG = STL/GP,
         BLKPG = BLK/GP,
         FGMPG = FGM/GP,
         FGAPG = FGA/GP,
         SP = FGM/FGA,
         X3PMPG = X3PM/GP,
         X3PAPG = X3PA/GP,
         X3PSP = X3PM/X3PA,
         FTMPG = FTM/GP,
         FTAPG = FTA/GP,
         FTSP = FTM/FTA
         ) %>% 
  select(League, 
         Season, 
         Stage, 
         Player, 
         Team,
         PER,
         GP,
         MPG, 
         PPG, 
         RPG, 
         APG,
         STLPG, 
         BLKPG, 
         TOVPG, 
         PFPG,
         FGMPG,
         FGAPG,
         SP,
         X3PMPG, 
         X3PAPG, 
         X3PSP, 
         FTMPG, 
         FTAPG, 
         FTSP,
         height,
         height_cm,
         weight,
         nationality)

average_nba_detail_playerstats$weight <- as.numeric(average_nba_detail_playerstats$weight)

for (i in 1:nrow(average_nba_detail_playerstats)) {
  if (is.nan(average_nba_detail_playerstats$X3PSP[i])) {
    average_nba_detail_playerstats$X3PSP[i] = 0
  }
  if (average_nba_detail_playerstats$Season[i] == "1999 - 2000" | average_nba_detail_playerstats$Season[i] == "2000 - 2001" | average_nba_detail_playerstats$Season[i] == "2001 - 2002" | average_nba_detail_playerstats$Season[i] == "2002 - 2003" | average_nba_detail_playerstats$Season[i] == "2003 - 2004" | average_nba_detail_playerstats$Season[i] == "2004 - 2005" | average_nba_detail_playerstats$Season[i] == "2005 - 2006") {
    average_nba_detail_playerstats$Era[i] = "1999-2006"
  }
  if (average_nba_detail_playerstats$Season[i] == "2006 - 2007" | average_nba_detail_playerstats$Season[i] == "2007 - 2008" | average_nba_detail_playerstats$Season[i] == "2008 - 2009" | average_nba_detail_playerstats$Season[i] == "2009 - 2010" | average_nba_detail_playerstats$Season[i] == "2010 - 2011" | average_nba_detail_playerstats$Season[i] == "2011 - 2012" |average_nba_detail_playerstats$Season[i] == "2012 - 2013") {
    average_nba_detail_playerstats$Era[i] = "2006-2013"
  }
  if (average_nba_detail_playerstats$Season[i] == "2013 - 2014" | average_nba_detail_playerstats$Season[i] == "2014 - 2015" | average_nba_detail_playerstats$Season[i] == "2015 - 2016" | average_nba_detail_playerstats$Season[i] == "2016 - 2017" | average_nba_detail_playerstats$Season[i] == "2017 - 2018" |average_nba_detail_playerstats$Season[i] == "2018 - 2019" |average_nba_detail_playerstats$Season[i] == "2019 - 2020") {
    average_nba_detail_playerstats$Era[i] = "2013-2020"
  }
}

average_nbars_detail_playerstats <- subset(average_nba_detail_playerstats, average_nba_detail_playerstats$Stage == "Regular_Season")
average_nbaps_detail_playerstats <- subset(average_nba_detail_playerstats, average_nba_detail_playerstats$Stage == "Playoffs")



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


#free_throw_attempt_plot(nbars_detail_playerstats)



ggplot(data = nbars_detail_playerstats, aes(x = height_cm, fill = Season))+
  geom_bar(stat = "count", position = "dodge2")+
  facet_wrap(~Season)


ggplot(average_nbars_detail_playerstats, aes(x = Era, y = X3PAPG, fill = Era)) + 
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


ggplot(average_nbars_detail_playerstats, aes(x = Era, y = height_cm, fill = Era)) + 
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

ggplot(average_nbars_detail_playerstats, aes(x = Era, y = weight, fill = Era)) + 
  geom_violin(size=0.5)+
  geom_boxplot(width=0.5, alpha=0.6)+
  scale_fill_viridis(discrete = TRUE, alpha=0.2, option = "D")+
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  )


ggplot(data = nbars_detail_playerstats, aes(x = round(PER, digits =0), fill = Season))+
 # geom_bar(stat = "count", position = "dodge")+
  geom_area(stat = "count", size=.5, colour="white") +
  scale_fill_viridis(discrete = T) +
  theme_ipsum()+
  #geom_area(stat = "count")+
  facet_wrap(~Season)


ggplot(data = average_nbars_detail_playerstats, aes(x = Season, y = PER, fill = Season))+
  geom_jitter(size = 0.5)+
  theme(legend.position = "none")
  # geom_bar(stat = "identity", position = "dodge")
  # geom_area(size=.5, colour="white") +
  # scale_fill_viridis(discrete = T) +
  # theme_ipsum()+
  # #geom_area(stat = "count")+
  #facet_wrap(~Season)


ggplot(data = average_nbars_detail_playerstats, aes(x = Season, y = height_cm, fill = Season))+
  geom_jitter(size = 0.5)+
  theme(legend.position = "none")



ggplot(data = average_nbars_detail_playerstats, aes(x = height_cm, fill = Era))+
  geom_area(stat = "count", alpha=0.3 , size=.5, color="white", position = "dodge")+
  #geom_bar(stat = "count",position = "identity", alpha = 0.2,width = 4)+
  scale_fill_viridis(discrete = T) + 
  theme_ipsum()
  #geom_vline(aes(xintercept = ))
  #facet_wrap(~Season)
  # theme(legend.position = "none")


ggplot(data = average_nbars_detail_playerstats, aes(x = weight, fill = Era))+
  #geom_area(stat = "count", alpha=0.3 , size=.5, color="white")+
  geom_bar(stat = "count",position = "dodge", alpha = 0.3,width = 4)+
  scale_fill_viridis(discrete = T) + 
  theme_ipsum()
  #geom_vline(aes(xintercept = ))+
  #facet_wrap(~Era)
  # theme(legend.position = "none")

hist(average_nbars_detail_playerstats$height_cm)

injury_freq <- table(injuries$Relinquished) %>% data.frame()




