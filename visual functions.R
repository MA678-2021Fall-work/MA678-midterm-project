library(tidyverse)
library(tidyr)
library(ggplot2)
library(viridis)
library(hrbrthemes)


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






