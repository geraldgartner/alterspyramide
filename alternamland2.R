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
library(googlesheets)
library(gganimate)

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
        plot.title = element_text(hjust=0, margin=unit(c(0,0,0.2,0), "cm")), 
        plot.margin = unit(c(1,0.5,0.5,0.5), "cm")) +
  theme(axis.text=element_text(size=16))

data <- read.csv("~/Google Drive/dStd.at/alterspyramide/alternamland/alterstrukur.csv", check.names = TRUE)
###data <- subset( data, select = -'95 bis 99 Jahre - Anmerkungen')
###rename(data, c("X.Unter.5.Jahre."="unter 5", "X.5.bis.9.Jahre."="5 bis 9"))
###names(data) <- sub("X.", "", names(data))
###data <- gsub("X.", "", data$alter)


#Reshape der Daten
data <- data %>%
  gather(alter, count, X.Unter.5.Jahre.:X.100.Jahre.und.älter.)

#Formatieren der Daten
data$count <- as.numeric(as.character(data$count))
data$ggk <- as.factor(data$ggk)
data <- subset(data, alter!="X.95.bis.99.Jahre...Anmerkungen.")

#
datasums <- aggregate(data$count, by=list(ggk=data$ggk), FUN=sum)
#datasums <- rename(datasums, ggk = "ggk2")

datadone <- merge(data, datasums, by="ggk")
datadone$pct <- round((datadone$count/datadone$x)*100,1)

#Anordnen der Daten
datadone$alter <- factor(datadone$alter, levels=c("X.Unter.5.Jahre.", "X.5.bis.9.Jahre.", "X.10.bis.14.Jahre.", 
"X.15.bis.19.Jahre.",    
"X.20.bis.24.Jahre.",     "X.25.bis.29.Jahre.",     "X.30.bis.34.Jahre.",     "X.35.bis.39.Jahre.",    
"X.40.bis.44.Jahre.",     "X.45.bis.49.Jahre.",     "X.50.bis.54.Jahre.",     "X.55.bis.59.Jahre.",    
"X.60.bis.64.Jahre.",     "X.65.bis.69.Jahre.",     "X.70.bis.74.Jahre.",     "X.75.bis.79.Jahre.",    
"X.80.bis.84.Jahre.",     "X.85.bis.89.Jahre.",     "X.90.bis.94.Jahre.",     "X.95.bis.99.Jahre.", "X.100.Jahre.und.älter."))

#Umbenennung der Gemeindegrößenklassen
datadone$ggk <- gsub("^1$", "weniger als 1.000 Bürger", datadone$ggk)
datadone$ggk <- gsub("^2$", "1.000-2.000 Bürger", datadone$ggk)
datadone$ggk <- gsub("^3$", "2.000-5.000 Bürger", datadone$ggk)
datadone$ggk <- gsub("^4$", "5.000-10.000 Bürger", datadone$ggk)
datadone$ggk <- gsub("^5$", "10.000-20.000 Bürger", datadone$ggk)
datadone$ggk <- gsub("^6$", "20.000-50.000 Bürger", datadone$ggk)
datadone$ggk <- gsub("^7$", "50.000-100.000 Bürger", datadone$ggk)
datadone$ggk <- gsub("^8$", "100.000-200.000 Bürger", datadone$ggk)
datadone$ggk <- gsub("^9$", "Stadt Graz", datadone$ggk)
datadone$ggk <- gsub("^10$", "Stadt Wien", datadone$ggk)

#Umreihung der Gemeindegrößenklassen
datadone$ggk <- factor(datadone$ggk, levels=c(
  "weniger als 1.000 Bürger",
  "1.000-2.000 Bürger",
  "2.000-5.000 Bürger", 
  "5.000-10.000 Bürger", 
  "10.000-20.000 Bürger", 
  "20.000-50.000 Bürger",
  "50.000-100.000 Bürger", 
  "100.000-200.000 Bürger", 
  "Stadt Graz", 
  "Stadt Wien"))

#absolute Prozentwerte in den Skalen
abs_percent <- function(x) {abs(scales::percent(x))}



  #Plotting
np <- ggplot(data = datadone, aes(x = alter, y = pct, fill = gender)) +
  geom_bar(aes(frame=ggk), data = subset(datadone, gender=="Männer"),
           stat = "identity", 
           position = "identity") +
  geom_bar(aes(frame=ggk, y=-pct), data = subset(datadone, gender=="Frauen"),
           stat = "identity",
           position = "identity") +
  geom_bar(data = subset(datadone, gender=="Männer" & ggk =="weniger als 1.000 Bürger"),
           stat = "identity",
           alpha = 0,
           colour = "black",
           position = "identity") +
  geom_bar(aes(y=-pct), data = subset(datadone, gender=="Frauen" & ggk =="weniger als 1.000 Bürger"),
           stat = "identity",
           alpha = 0, 
           colour = "black", 
           position = "identity") +
  coord_flip()+
  scale_x_discrete(labels = c("unter 5 Jahre", "<9 Jahre", "<14 Jahre", 
                              "<19 Jahre",    
                              "<24 Jahre",     "<29 Jahre",     "<34 Jahre",     "<39 Jahre",    
                              "<44 Jahre",     "<49 Jahre",     "<54 Jahre",     "<59 Jahre",    
                              "<64 Jahre",     "<69 Jahre",     "<74 Jahre",     "<79 Jahre",    
                              "<84 Jahre",     "<89 Jahre",     "<94 Jahre",     "<99 Jahre", "+100Jahre")) +
  scale_fill_manual(values=c("#749672", "#c15e5a"))+
  ggtitle("Alterspyramide nach Einwohner:\n")+
  guides(fill=guide_legend(title=NULL))+
  ylab("Bevölkerungsanteil der Fünf-Jahres-Altersgruppe in Prozent")+
  xlab("Altersgruppe") +
  scale_y_continuous(labels=abs)+
  theme


gg_animate(np, ani.width = 496, interval=1.5, "alterspyramide2.gif")
