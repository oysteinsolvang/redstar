library(ggplot2)
library(tidyverse)
df <- read.csv2(url("https://raw.githubusercontent.com/oysteinsolvang/redstar/main/scrambles_rnoaf.csv"))
df <- df %>% filter(year>=2000)
g <- ggplot(df,aes(x=year,y=identified)) +
  geom_line() +
  xlab("Year") +
  ylab("No. of identifications") + 
  ggtitle("Aircraft identified by RNoAF QRA since 2000") +
  theme_classic()

g <- g + geom_curve(aes(x = 2003, y = 75, xend = 2006.7, yend = 88), 
               colour = "#555555", 
               size=0.5, 
               curvature = -0.2,
               arrow = arrow(length = unit(0.03, "npc")))


g +   geom_label(aes(x = 2000, y = 65, label = "Russia resumes \n bomber patrols"), 
                 hjust = 0, 
                 vjust = 0.5, 
                 lineheight = 0.8,
                 colour = "#555555", 
                 fill = "white", 
                 label.size = NA, 
                 family="Helvetica", 
                 size = 4) 
