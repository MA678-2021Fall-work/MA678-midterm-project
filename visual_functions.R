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



# ggplot(df_rs_aver_final, aes(x = height_cm, fill = Season))+
#   geom_bar(stat = "count", position = "dodge", alpha = 0.6, width = 3)+
#   facet_wrap(~Season)
#   
# ggplot(df_rs_aver_final, aes(x = weight_kg, fill = Season))+
#   geom_area(stat = "count", position = "dodge", alpha = 0.6, width = 2)+
#   facet_wrap(~Season)
# 
# 
# free_throw_attempt_plot <- function(league){
#   ggplot(league, aes(x = Season, y = PFPG, fill = Season)) + 
#     geom_violin(size=0.5)+
#     geom_boxplot(width=0.5, alpha=0.6)+
#     scale_fill_viridis(discrete = TRUE, alpha=0.2, option = "D")+
#     theme_ipsum() +
#     theme(
#       legend.position="none",
#       plot.title = element_text(size=14)
#     )
  #facet_wrap(~Season)
  #geom_violin(width=2, size=0.2)+
  #coord_flip()
  #geom_boxplot(width=0.1, color="grey", alpha=0.2)
  
# }

#free_throw_attempt_plot(df_rs_aver_final)

# Age vs. injury_level:
Age_il <- ggplot(df_rs_aver_final, aes(x = Age, y = injury_level, color = Season))+
  geom_point(alpha = 0.3)+
  geom_smooth(se = F)+
  scale_fill_viridis(discrete = TRUE, alpha=0.2, option = "D")+
  theme_ipsum()+
  ggtitle("Players' Age vs. Injury Level")+
  xlab("Players' Age")+
  ylab("Injury Level")+
  theme(legend.key.size = unit(1, 'cm'), legend.text = element_text(size=15,face="bold"),axis.text=element_text(size=15,face="bold"),
        axis.title=element_text(size=15,face="bold"), axis.text.x = element_text(angle = 45, hjust=1))
  


# MPG vs. injury_level:
mpg_il <- ggplot(df_rs_aver_final, aes(x = MPG , y = injury_level, color = Season))+
  geom_point(alpha = 0.3)+
  geom_smooth(se = F)+
  scale_fill_viridis(discrete = TRUE, alpha=0.2, option = "D")+
  theme_ipsum()+
  ggtitle("Minutes play per game vs. Injury Level")+
  xlab("Minutes play per game")+
  ylab("Injury Level")+
  theme(legend.key.size = unit(1, 'cm'), legend.text = element_text(size=15,face="bold"),axis.text=element_text(size=15,face="bold"),
        axis.title=element_text(size=15,face="bold"), axis.text.x = element_text(angle = 45, hjust=1), legend.position = "bottom")

# GP vs. injury_level:
gp_il <- ggplot(df_rs_aver_final, aes(x = GP , y = injury_level, color = Season))+
  geom_point(alpha = 0.3)+
  geom_smooth(se = F)+
  scale_fill_viridis(discrete = TRUE, alpha=0.2, option = "D")+
  theme_ipsum()+
  ggtitle("Number of Games played in a Season vs. Injury Level")+
  xlab("Number of Games in a Season")+
  ylab("Injury Level")+
  theme(legend.key.size = unit(1, 'cm'), legend.text = element_text(size=15,face="bold"),axis.text=element_text(size=15,face="bold"),
        axis.title=element_text(size=15,face="bold"), axis.text.x = element_text(angle = 45, hjust=1),legend.position = "bottom")

# X3PAPG vs. injury_level:
x3papg_il <- ggplot(df_rs_aver_final, aes(x = X3PAPG , y = injury_level, color = Season))+
  geom_point(alpha = 0.3)+
  geom_smooth(se = F)+
  scale_fill_viridis(discrete = TRUE, alpha=0.2, option = "D")+
  theme_ipsum()+
  ggtitle("Number of 3 points attempts per game vs. Injury Level")+
  xlab("Number of 3 points attempts per gam")+
  ylab("Injury Level")+
  theme(legend.key.size = unit(1, 'cm'), legend.text = element_text(size=15,face="bold"),axis.text=element_text(size=15,face="bold"),
        axis.title=element_text(size=15,face="bold"), axis.text.x = element_text(angle = 45, hjust=1),legend.position = "bottom")

# PFPG vs. injury_level:

pfpg_il <- ggplot(df_rs_aver_final, aes(x = PFPG , y = injury_level, color = Season))+
  geom_point(alpha = 0.3)+
  geom_smooth(se = F)+
  scale_fill_viridis(discrete = TRUE, alpha=0.2, option = "D")+
  theme_ipsum()+
  ggtitle("Number of personal fouls per game vs. Injury Level")+
  xlab("Number of personal fouls per game")+
  ylab("Injury Level")+
  theme(legend.key.size = unit(1, 'cm'), legend.text = element_text(size=15,face="bold"),axis.text=element_text(size=15,face="bold"),
        axis.title=element_text(size=15,face="bold"), axis.text.x = element_text(angle = 45, hjust=1),legend.position = "bottom")

# PER vs. injury_level:

per_il <- ggplot(df_rs_aver_final, aes(x = PER , y = injury_level, color = Season))+
  geom_point(alpha = 0.3)+
  geom_smooth(se = F)+
  scale_fill_viridis(discrete = TRUE, alpha=0.2, option = "D")+
  theme_ipsum()+
  ggtitle("Player Efficiency Rating vs. Injury Level")+
  xlab("Player Efficiency Rating")+
  ylab("Injury Level")+
  theme(legend.key.size = unit(1, 'cm'), legend.text = element_text(size=15,face="bold"),axis.text=element_text(size=15,face="bold"),
        axis.title=element_text(size=15,face="bold"), axis.text.x = element_text(angle = 45, hjust=1),legend.position = "bottom")

# height_cm vs. injury_level:

height_il <- ggplot(df_rs_aver_final, aes(x = height_cm , y = injury_level, color = Season))+
  geom_point(alpha = 0.3)+
  geom_smooth(se = F)+
  scale_fill_viridis(discrete = TRUE, alpha=0.2, option = "D")+
  theme_ipsum()+
  ggtitle("Player's Height vs. Injury Level")+
  xlab("Player's Height(cm)")+
  ylab("Injury Level")+
  theme(legend.key.size = unit(1, 'cm'), legend.text = element_text(size=15,face="bold"),axis.text=element_text(size=15,face="bold"),
        axis.title=element_text(size=15,face="bold"), axis.text.x = element_text(angle = 45, hjust=1))

# weight_kg vs. injury_level:

weight_il <- ggplot(df_rs_aver_final, aes(x = weight_kg , y = injury_level, color = Season))+
  geom_point(alpha = 0.3)+
  geom_smooth(se = F)+
  scale_fill_viridis(discrete = TRUE, alpha=0.2, option = "D")+
  theme_ipsum()+
  ggtitle("Player's Weight vs. Injury Level")+
  xlab("Player's Weight(kg)")+
  ylab("Injury Level")+
  theme(legend.key.size = unit(1, 'cm'), legend.text = element_text(size=15,face="bold"),axis.text=element_text(size=15,face="bold"),
        axis.title=element_text(size=15,face="bold"), axis.text.x = element_text(angle = 45, hjust=1))



ggplot(df_rs_aver_final,aes(y= Season, x = X3PAPG ,  fill=Season)) +
  geom_density_ridges(alpha=0.6, stat="binline", bins=20) +
  theme_ridges() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.7, "lines"),
    strip.text.x = element_text(size = 8)
  )
