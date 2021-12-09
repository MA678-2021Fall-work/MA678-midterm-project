library(tidyverse)
library(tidyr)
library(ggplot2)
library(viridis)
library(hrbrthemes)


#eda for analyzing the basketball style changing trend:

three_point_attempt_plot <- function(df){
  ggplot(df, aes(x = Season, y = X3PAPG, fill = Season)) + 
    geom_violin(size=0.5)+
    geom_boxplot(width=0.5, alpha=0.5)+
    scale_fill_viridis(discrete = TRUE, alpha=0.2, option = "D")+
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=15)
    )
}


#three_point_attempt_plot(df_rs_aver_final) + ggtitle("Number of 3 Points attempt per game distribution from season 2010-2011 to season 2019-2020")



ggplot(df_rs_aver_final, aes(x = height_cm, fill = Season))+
  geom_bar(stat = "count", position = "dodge", alpha = 0.6, width = 3)+
  facet_wrap(~Season)
  
ggplot(df_rs_aver_final, aes(x = weight_kg, fill = Season))+
  geom_area(stat = "count", position = "dodge", alpha = 0.6, width = 2)+
  facet_wrap(~Season)


free_throw_attempt_plot <- function(league){
  ggplot(league, aes(x = Season, y = PFPG, fill = Season)) + 
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

free_throw_attempt_plot(df_rs_aver_final)

# Age vs. injury_level:
ggplot(df_rs_aver_final, aes(x = Age, y = injury_level, color = Season))+
  geom_point()+
  geom_smooth(se = F)+
  scale_fill_viridis(discrete = TRUE, alpha=0.2, option = "D")+
  theme_ipsum()


# MPG vs. injury_level:
ggplot(df_rs_aver_final, aes(x = MPG , y = injury_level, color = Season))+
  geom_point()+
  geom_smooth(se = F)+
  scale_fill_viridis(discrete = TRUE, alpha=0.2, option = "D")+
  theme_ipsum()

# GP vs. injury_level:
ggplot(df_rs_aver_final, aes(x = GP , y = injury_level, color = Season))+
  geom_point()+
  geom_smooth(se = F)+
  scale_fill_viridis(discrete = TRUE, alpha=0.2, option = "D")+
  theme_ipsum()

# X3PAPG vs. injury_level:
ggplot(df_rs_aver_final, aes(x = X3PAPG , y = injury_level, color = Season))+
  geom_point()+
  geom_smooth(se = F)+
  scale_fill_viridis(discrete = TRUE, alpha=0.2, option = "D")+
  theme_ipsum()

# PFPG vs. injury_level:

ggplot(df_rs_aver_final, aes(x = PFPG , y = injury_level, color = Season))+
  geom_point()+
  geom_smooth(se = F)+
  scale_fill_viridis(discrete = TRUE, alpha=0.2, option = "D")+
  theme_ipsum()

# PER vs. injury_level:

ggplot(df_rs_aver_final, aes(x = PER , y = injury_level, color = Season))+
  geom_point()+
  geom_smooth(se = F)+
  scale_fill_viridis(discrete = TRUE, alpha=0.2, option = "D")+
  theme_ipsum()

# height_cm vs. injury_level:

ggplot(df_rs_aver_final, aes(x = height_cm , y = injury_level, color = Season))+
  geom_point()+
  geom_smooth(se = F)+
  scale_fill_viridis(discrete = TRUE, alpha=0.2, option = "D")+
  theme_ipsum()

# weight_kg vs. injury_level:

ggplot(df_rs_aver_final, aes(x = weight_kg , y = injury_level, color = Season))+
  geom_point()+
  geom_smooth(se = F)+
  scale_fill_viridis(discrete = TRUE, alpha=0.2, option = "D")+
  theme_ipsum()


