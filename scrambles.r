setwd("/Users/oysteinsolvang/Documents/NUPI/red_star")
rm(list=ls())
library(tidyverse)
library(zoo) # rolling average
library(readxl) # read excel file

#df <- read.csv2("rs.csv", header=TRUE,skip=1)
#df <- as.data.frame(read_excel("scrambles.xlsx"))
df <- read.csv2("scrambles.csv")
colnames(df) <- c("Date","Week","Aircraft","UAV","Other1","Other2","Other3","URL")
#df$Date <- as.Date(as.numeric(df$Date), origin = "1899-12-30")
df$Aircraft <- as.numeric(df$Aircraft)
df$UAV <- as.numeric(df$UAV)
df$Other1 <- as.numeric(df$Other1)
df$Other2 <- as.numeric(df$Other2)
df$Other3 <- as.numeric(df$Other3)
df$UAV[is.na(df$UAV)] <- 0
df$Other1[is.na(df$Other1)] <- 0
df$Other2[is.na(df$Other2)] <- 0
df$Other3[is.na(df$Other3)] <- 0
df$Other <- df$Other1+df$Other2+df$Other3
df <- df %>%
    select(Date,Week,Aircraft,UAV,Other)
df <- na.omit(df)
df$wd <- strtrim(df$Date,4)
df$Week <- paste(df$wd,df$Week,sep="-")
df$Other <- ifelse(df$Other == 0, NA, df$Other)
df$UAV  <- ifelse(df$UAV == 0, NA, df$UAV)
df$Date <- as.Date(df$Date)

d <- df
d$UAV[is.na(d$UAV)] <- 0
d$Other[is.na(d$Other)] <- 0
d$Total <- d$Aircraft+d$UAV+d$Other
df$Total <- d$Total
df$R_mean <- zoo::rollmean(df$Total, k = 12, fill=NA)

df <- df %>%
    select(Date,Week,Aircraft,UAV,Other,Total,R_mean)
d <- gather(df, Type, number, c(Aircraft,UAV,Other,R_mean),factor_key=TRUE) # with rolling mean total
#d <- gather(df, Type, number, c(Aircraft,UAV,Other,Total),factor_key=TRUE) # with raw total

ggplot(d,aes(x=Date,y=number,color=Type)) +
    stat_summary(fun = sum, # adds up all observations for the month
      geom = "line") + # or "line"
    scale_x_date(date_breaks=("20 weeks")) +
#    theme_classic()+
    theme(legend.justification = "top") +
    xlab("")+
    ylab("Observations reported per week") +
    ggtitle("Krasnaya Zvezda: Foreign military aircraft near Russia's borders")
filename <- paste(Sys.Date(),"redstar-scrambles.png",sep="-")
ggsave(filename,height=10,width=30)
