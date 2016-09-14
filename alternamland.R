library(ggthemes)
library(ggThemeAssist)
library(ggplot2)
library(magrittr)
library(dplyr)
library(tidyr)
library(git2r)
library(formatR)
library(scales)
library(grid)
library("googlesheets")
library("gganimate")

suppressPackageStartupMessages(library("dplyr"))


#Unser Style
theme <- theme(plot.background = element_rect(fill = "gray97"), panel.grid.major = element_line(colour = "gray86", linetype = "dotted"), 
               panel.grid.minor = element_line(colour = "gray86", linetype = "dotted")) + 
  theme(plot.title = element_text(size = 22, face = "bold"), 
        plot.background = element_rect(fill = "gray97", colour = "antiquewhite", size = 10, linetype = "solid")) +
  theme(axis.ticks = element_blank(), 
        axis.line = element_blank(),
        axis.title = element_text(vjust = 8), 
        panel.background = element_rect(fill = "grey97", linetype = "solid"), 
        plot.background = element_rect(colour = "gray97"), 
        plot.title = element_text(hjust=0, margin=unit(c(0,1,0.2,1), "cm")), 
        plot.margin = unit(c(1,0.5,0.5,0.5), "cm")) +
  theme(axis.text=element_text(size=16))

data <- read.csv("~/Google Drive/dStd.at/alterspyramide/alternamland/alterstrukur.csv", check.names = TRUE)
###data <- subset( data, select = -'95 bis 99 Jahre - Anmerkungen')
###rename(data, c("X.Unter.5.Jahre."="unter 5", "X.5.bis.9.Jahre."="5 bis 9"))
###names(data) <- sub("X.", "", names(data))
###data <- gsub("X.", "", data$alter)

data <- data %>%
  gather(alter, count, X.Unter.5.Jahre.:X.100.Jahre.und.älter.)


data$count <- as.numeric(as.character(data$count))
data$ggk <- as.factor(data$ggk)
data <- subset(data, alter!="X.95.bis.99.Jahre...Anmerkungen.")

datasums <- aggregate(data$count, by=list(ggk=data$ggk), FUN=sum)
#datasums <- rename(datasums, ggk = "ggk2")

datadone <- merge(data, datasums, by="ggk")
datadone$pct <- datadone$count/datadone$x


datadone$alter <- factor(datadone$alter, levels=c("X.Unter.5.Jahre.", "X.5.bis.9.Jahre.", "X.10.bis.14.Jahre.", 
"X.15.bis.19.Jahre.",    
"X.20.bis.24.Jahre.",     "X.25.bis.29.Jahre.",     "X.30.bis.34.Jahre.",     "X.35.bis.39.Jahre.",    
"X.40.bis.44.Jahre.",     "X.45.bis.49.Jahre.",     "X.50.bis.54.Jahre.",     "X.55.bis.59.Jahre.",    
"X.60.bis.64.Jahre.",     "X.65.bis.69.Jahre.",     "X.70.bis.74.Jahre.",     "X.75.bis.79.Jahre.",    
"X.80.bis.84.Jahre.",     "X.85.bis.89.Jahre.",     "X.90.bis.94.Jahre.",     "X.95.bis.99.Jahre.", "X.100.Jahre.und.älter."))

p <- ggplot(data = datadone, aes(x = alter, y = pct, fill = gender, frame=ggk)) +
  geom_bar(data = subset(datadone, gender=="Männer"), alpha = 0.5,
           stat = "identity",
           position = "identity") +
  geom_bar(data = subset(datadone, gender=="Frauen"),alpha = 0.5,
           stat = "identity",
           position = "identity",
           mapping = aes(y = -pct)) +
  scale_x_discrete(labels = c("unter 5 Jahre", "bis 9 Jahre", "bis 14 Jahre", 
                                "bis 19 Jahre",    
                                "bis 24 Jahre",     "bis 29 Jahre",     "bis 34 Jahre",     "bis 39 Jahre",    
                                "bis 44 Jahre",     "bis 49 Jahre",     "bis 54 Jahre",     "bis 59 Jahre",    
                               "bis 64 Jahre",     "bis 69 Jahre",     "bis 74 Jahre",     "bis 79 Jahre",    
                                "bis 84 Jahre",     "bis 89 Jahre",     "bis 94 Jahre",     "bis 99 Jahre", "+100Jahre")) +
  scale_fill_manual(values=c("#749672", "#c15e5a"))+
  scale_y_continuous(labels = scales::percent)+
  coord_flip() +
  #facet_grid(. ~ ggk)+
  theme

gg_animate(p)

geom_line(data = subset(datadone, gender=="Männer" & ggk=="1"), 
          stat = "identity",
          position = "identity") +
  geom_line(data = subset(datadone, gender=="Frauen" & ggk=="1"),
            stat = "identity",
            position = "identity",
            mapping = aes(y = -pct)) +