library(tidyverse)
library(tidyr)
library(ggplot2)
library(viridis)
library(hrbrthemes)


source("data_clean_wrangle.R")

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


