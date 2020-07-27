### clear everything ###
rm(list=ls(all=TRUE)) 
cat("\014") 

list.of.packages <- c("ggplot2", "reshape2", "gridExtra", "tidyverse", "vegan",
                      "ggpubr", "grid", "AMR", "scales", "dplyr", "maps", 
                      "sp", "rgdal", "sf", "remotes")
new.packages <- list.of.packages[!(list.of.packages %in% 
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(ggplot2)
library(reshape2)
library(gridExtra)
library(vegan)
library(ggpubr)
library(grid)
library(AMR)
library(scales)
library(dplyr)
library(maps)
library(sp)
library(rgdal)
library(tidyverse)
library(sf)
library(remotes)

install_github("dkahle/ggmap", ref = "tidyup")
library(ggmap)

## Read SOF File ##
SOF = read.csv("Season of Fire Papers - New Work After 5_28.csv")
SOF$YearName <- paste(SOF$Year, SOF$Last.Name, sep=" ")

## Create Summary of Defined Data & Graph ##
DefinedSum = as.data.frame(summary(SOF$Defined.))
DefinedSum <- cbind(rownames(DefinedSum), DefinedSum)
colnames(DefinedSum)[colnames(DefinedSum)=="rownames(DefinedSum)"] <- "Defined"
colnames(DefinedSum)[colnames(DefinedSum)=="summary(SOF$Defined.)"] <- "Count"
DefinedSum = as.data.frame(DefinedSum)

ggplot(data = DefinedSum, aes(Defined, Count, fill = Defined)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.2) +
  scale_fill_manual(values=c("grey2", "lightgoldenrod2")) +
  ylim(0, 40) +
  xlab("") +
  ylab("Publication Count") +
  ggtitle("Season of Fire Terminology Defined?") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(title = element_text(face = "bold", size = 16)) +
  theme(axis.title = element_text(face = "bold", size = 14)) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(legend.position ="none") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

## Create Summary of Terminology Used in Papers & Graph ##
SOF1 = dplyr::select(SOF, Dormant.Growing, Astronomical.Seasons, Dry.Wet, Lightning)
TermCounts <- colSums(SOF1 == "Yes")
TermCounts = as.data.frame(TermCounts)
TermCounts <- cbind(rownames(TermCounts), TermCounts)
colnames(TermCounts)[colnames(TermCounts)=="rownames(TermCounts)"] <- "Terminologies"
colnames(TermCounts)[colnames(TermCounts)=="TermCounts"] <- "Count"
TermCounts = as.data.frame(TermCounts)

ggplot(data = TermCounts, aes(Terminologies, Count, fill = Terminologies)) +
  geom_bar(stat = "identity", color ="Black", alpha=0.3) +
  ylim(0, 40) +
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values=c("Orange", "Green", "dodgerblue1", "yellow")) +
  xlab("") +
  ylab("Publication Count") +
  theme(title = element_text(size = 16)) +
  theme(axis.title = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position ="none")

ggsave("TermsUses&PubCount.png", width = 8, height = 5)

## Dates Graph ##
## First select data from SOF and the Filter to omit NA values ##
SOF2 = dplyr::select(SOF, YearName, Last.Name, Year.Published, X.Cited, DormantStart, DormantEnd, GrowingStart, 
              GrowingEnd, DormantStart2, DormantEnd2, DryStart1, DryEnd1, WetStart, WetEnd, DryStart2, DryEnd2,
              LightningStart, LightningEnd, Calendar.MonthsStart, Calendar.MonthsEnd, 
              Calendar.MonthsStart2, Calendar.MonthsEnd2, WinterStartEx, WinterEndEx, SpringStartEx, SpringEndEx, 
              SummerStartEx, SummerEndEx, FallStartEx, FallEndEx, WinterStartEx2, WinterEndEx2, DormantStartEx, 
              DormantEndEx, GrowingStartEx, GrowingEndEx, GrowingStartEx2, GrowingEndEx2, DormantStartEx2, DormantEndEx2,
              DryStartEx, DryEndEx, WetStartEx, WetEndEx, LightningStartEx, LightningEndEx)
SOF3 = dplyr::select(SOF2, YearName, Last.Name, Year.Published, X.Cited, DormantStart, DormantEnd, GrowingStart, 
              GrowingEnd, DormantStart2, DormantEnd2, DryStart1, DryEnd1, WetStart, WetEnd, DryStart2, DryEnd2,
              LightningStart, LightningEnd)
SOF3 =  filter(SOF3,DormantStart != "." | GrowingStart != "." | WetStart != "." | LightningStart != ".")

## Convert Dates into Format that can be graphed ##
SOF2$DormantStart <- lubridate::ymd(SOF2$DormantStart)
SOF2$DormantEnd <- lubridate::ymd(SOF2$DormantEnd)
SOF2$GrowingStart <- lubridate::ymd(SOF2$GrowingStart)
SOF2$GrowingEnd <- lubridate::ymd(SOF2$GrowingEnd)
SOF2$DormantStart2 <- lubridate::ymd(SOF2$DormantStart2)
SOF2$DormantEnd2 <- lubridate::ymd(SOF2$DormantEnd2)
SOF2$DryStart1 <- lubridate::ymd(SOF2$DryStart1)
SOF2$DryEnd1 <- lubridate::ymd(SOF2$DryEnd1)
SOF2$WetStart <- lubridate::ymd(SOF2$WetStart)
SOF2$WetEnd <- lubridate::ymd(SOF2$WetEnd)
SOF2$DryStart2<- lubridate::ymd(SOF2$DryStart2)
SOF2$DryEnd2 <- lubridate::ymd(SOF2$DryEnd2)
SOF2$LightningStart <- lubridate::ymd(SOF2$LightningStart)
SOF2$LightningEnd <- lubridate::ymd(SOF2$LightningEnd)
SOF2$Calendar.MonthsStart <- lubridate::ymd(SOF2$Calendar.MonthsStart)
SOF2$Calendar.MonthsEnd <- lubridate::ymd(SOF2$Calendar.MonthsEnd)
SOF2$Calendar.MonthsStart2 <- lubridate::ymd(SOF2$Calendar.MonthsStart2)
SOF2$Calendar.MonthsEnd2 <- lubridate::ymd(SOF2$Calendar.MonthsEnd2)
SOF2$WinterStartEx <- lubridate::ymd(SOF2$WinterStartEx)
SOF2$WinterEndEx <- lubridate::ymd(SOF2$WinterEndEx)
SOF2$SpringStartEx <- lubridate::ymd(SOF2$SpringStartEx)
SOF2$SpringEndEx <- lubridate::ymd(SOF2$SpringEndEx)
SOF2$SummerStartEx <- lubridate::ymd(SOF2$SummerStartEx)
SOF2$SummerEndEx <- lubridate::ymd(SOF2$SummerEndEx)
SOF2$FallStartEx <- lubridate::ymd(SOF2$FallStartEx)
SOF2$FallEndEx <- lubridate::ymd(SOF2$FallEndEx)
SOF2$WinterStartEx2 <- lubridate::ymd(SOF2$WinterStartEx2)
SOF2$WinterEndEx2 <- lubridate::ymd(SOF2$WinterEndEx2)
SOF2$DormantStartEx <- lubridate::ymd(SOF2$DormantStartEx)
SOF2$DormantEndEx <- lubridate::ymd(SOF2$DormantEndEx)
SOF2$GrowingStartEx <- lubridate::ymd(SOF2$GrowingStartEx)
SOF2$GrowingEndEx <- lubridate::ymd(SOF2$GrowingEndEx)
SOF2$GrowingStartEx2 <- lubridate::ymd(SOF2$GrowingStartEx2)
SOF2$GrowingEndEx2 <- lubridate::ymd(SOF2$GrowingEndEx2)
SOF2$DormantStartEx2 <- lubridate::ymd(SOF2$DormantStartEx2)
SOF2$DormantEndEx2 <- lubridate::ymd(SOF2$DormantEndEx2)
SOF2$DryStartEx <- lubridate::ymd(SOF2$DryStartEx)
SOF2$DryEndEx <- lubridate::ymd(SOF2$DryEndEx)
SOF2$WetStartEx <- lubridate::ymd(SOF2$WetStartEx)
SOF2$WetEndEx <- lubridate::ymd(SOF2$WetEndEx)
SOF2$LightningStartEx <- lubridate::ymd(SOF2$LightningStartEx)
SOF2$LightningEndEx <- lubridate::ymd(SOF2$LightningEndEx)
SOF2$Year.Published <- lubridate::ymd(SOF2$Year.Published)
Lightning1 <- lubridate::ymd("2019-04-15")
Lightning2 <- lubridate::ymd("2019-06-15")


## Graph the results ##

ggplot(SOF2) +
  geom_segment(aes( x = Calendar.MonthsStart, xend = Calendar.MonthsEnd, y= YearName, 
                    yend = YearName), size = 1, colour = "black", alpha = 0.5) +
  geom_segment(aes( x = Calendar.MonthsStart2, xend = Calendar.MonthsEnd2, y= YearName, 
                    yend = YearName), size = 1, colour = "black", alpha = 0.5) +
  geom_segment(aes( x = WinterStartEx, xend = WinterEndEx, y= YearName, 
                    yend = YearName), size = 1, colour = "black", alpha = 0.5) +
  geom_segment(aes( x = SpringStartEx, xend = SpringEndEx, y= YearName, 
                    yend = YearName), size = 1, colour = "yellow", alpha = 0.5) +
  geom_segment(aes( x = SummerStartEx, xend = SummerEndEx, y= YearName, 
                    yend = YearName), size = 1, colour = "black", alpha = 0.5) +
  geom_segment(aes( x = FallStartEx, xend = FallEndEx, y= YearName, 
                    yend = YearName), size = 1, colour = "black", alpha = 0.5) +
  geom_segment(aes( x = WinterStartEx2, xend = WinterEndEx2, y= YearName, 
                    yend = YearName), size = 1, colour = "black", alpha = 0.5) +
  geom_segment(aes( x = DormantStart, xend = DormantEnd, y= YearName, 
                    yend = YearName), size = 6, colour = "darkorange1", alpha = 0.5) +
  geom_segment(aes( x = GrowingStart, xend = GrowingEnd, y= YearName, 
                    yend = YearName), size = 6, colour = "springgreen3", alpha = 0.5) +
  geom_segment(aes( x = DormantStart2, xend = DormantEnd2, y= YearName, 
                    yend = YearName), size = 6, colour = "darkorange1", alpha = 0.5) +
  geom_segment(aes( x = DryStart1, xend = DryEnd1, y= YearName, 
                    yend = YearName), size = 6, colour = "orchid2", alpha = 0.5) +
  geom_segment(aes( x = WetStart, xend = WetEnd, y= YearName, 
                    yend = YearName), size = 6, colour = "dodgerblue") +
  geom_segment(aes( x = DryStart2, xend = DryEnd2, y= YearName, 
                    yend = YearName), size = 6, colour = "orchid2", alpha = 0.5) +
  geom_segment(aes( x = LightningStart, xend = LightningEnd, y= YearName, 
                    yend = YearName), size = 1.5, colour = "yellow2") +
  geom_segment(aes(x = DormantStartEx, xend = DormantEndEx, y= YearName, 
                   yend = YearName), size = 1, color = "coral4", linetype = 1) +
  geom_segment(aes(x = GrowingStartEx, xend = GrowingEndEx, y= YearName, 
                   yend = YearName), size = 1, color = "green4", linetype = 1) +
  geom_segment(aes( x = GrowingStartEx2, xend = GrowingEndEx2, y= YearName, 
                    yend = YearName), size = 1, colour = "green4", alpha = 0.5) +
  geom_segment(aes( x = DormantStartEx2, xend = DormantEndEx2, y= YearName, 
                    yend = YearName), size = 1, colour = "coral4", alpha = 0.5) +
  geom_segment(aes(x = DryStartEx, xend = DryEndEx, y= YearName, 
                   yend = YearName), size = 1, color = "purple", linetype = 1) +
  geom_segment(aes(x = WetStartEx, xend = WetEndEx, y= YearName, 
                   yend = YearName), size = 1, color = "turquoise1", linetype = 1) +
  geom_segment(aes(x = LightningStartEx, xend = LightningEndEx, y= YearName, 
                   yend = YearName), size = 1, color = "red1", linetype = 1) +
  scale_x_date(breaks = seq(as.Date("2019-01-01"), as.Date("2019-12-31"), 
                            by="1 months"), date_labels = "%B", 
               limits = as.Date(c("2019-01-01", "2019-12-31"))) +
  xlab("") + 
  ylab("Authors") +
  geom_vline(xintercept=Lightning1, size=1, color="red") +
  geom_vline(xintercept=Lightning2, size=1, color="red") +
  ggtitle(label = "Authors and Defined Terminologies") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Authors&DefinedTermswithDates.png", width = 8, height = 5)

############
SOF3$DormantStart <- lubridate::ymd(SOF3$DormantStart)
SOF3$DormantEnd <- lubridate::ymd(SOF3$DormantEnd)
SOF3$GrowingStart <- lubridate::ymd(SOF3$GrowingStart)
SOF3$GrowingEnd <- lubridate::ymd(SOF3$GrowingEnd)
SOF3$DormantStart2 <- lubridate::ymd(SOF3$DormantStart2)
SOF3$DormantEnd2 <- lubridate::ymd(SOF3$DormantEnd2)
SOF3$DryStart1 <- lubridate::ymd(SOF3$DryStart1)
SOF3$DryEnd1 <- lubridate::ymd(SOF3$DryEnd1)
SOF3$WetStart <- lubridate::ymd(SOF3$WetStart)
SOF3$WetEnd <- lubridate::ymd(SOF3$WetEnd)
SOF3$DryStart2<- lubridate::ymd(SOF3$DryStart2)
SOF3$DryEnd2 <- lubridate::ymd(SOF3$DryEnd2)
SOF3$LightningStart <- lubridate::ymd(SOF3$LightningStart)
SOF3$LightningEnd <- lubridate::ymd(SOF3$LightningEnd)
SOF2$Year.Published <- lubridate::ymd(SOF3$Year.Published)
Lightning1 <- lubridate::ymd("2019-04-15")
Lightning2 <- lubridate::ymd("2019-06-15")


ggplot(SOF3) +
  geom_segment(aes( x = DormantStart, xend = DormantEnd, y= YearName, 
                    yend = YearName), size = 6, colour = "darkorange1", alpha = 0.5) +
  geom_segment(aes( x = GrowingStart, xend = GrowingEnd, y= YearName, 
                    yend = YearName), size = 6, colour = "springgreen3", alpha = 0.5) +
  geom_segment(aes( x = DormantStart2, xend = DormantEnd2, y= YearName, 
                    yend = YearName), size = 6, colour = "darkorange1", alpha = 0.5) +
  geom_segment(aes( x = DryStart1, xend = DryEnd1, y= YearName, 
                    yend = YearName), size = 6, colour = "orchid2", alpha = 0.5) +
  geom_segment(aes( x = WetStart, xend = WetEnd, y= YearName, 
                    yend = YearName), size = 6, colour = "dodgerblue") +
  geom_segment(aes( x = DryStart2, xend = DryEnd2, y= YearName, 
                    yend = YearName), size = 6, colour = "orchid2", alpha = 0.5) +
  geom_segment(aes( x = LightningStart, xend = LightningEnd, y= YearName, 
                    yend = YearName), size = 1.5, colour = "yellow2") +
  scale_x_date(breaks = seq(as.Date("2019-01-01"), as.Date("2019-12-31"), 
                            by="1 months"), date_labels = "%B", 
               limits = as.Date(c("2019-01-01", "2019-12-31"))) +
  xlab("") + 
  ylab("Authors") +
  geom_vline(xintercept=Lightning1, size=1, color="red") +
  geom_vline(xintercept=Lightning2, size=1, color="red") +
  ggtitle(label = "Authors and Defined Terminologies") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Authors&DefinedTerms.png", width = 8, height = 5)

##
 
ggplot(SOF, aes(YearName, X.Cited, fill = Defined.)) +
  geom_bar(stat = "identity", size=1) +
  scale_fill_manual(values=c("grey", "yellow3")) +
  labs(x = "", color='Terminology Defined?') +
  ylab("Number of Citations") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(), 
        legend.position = c(.85,.65))

 ggsave("NumberCited.png", width = 8, height = 5)

##

ggplot(SOF, aes(Defined., X.Cited, color = Defined.)) +
  geom_bar(fill = "transparent", stat = "identity", size = 0.7, 
           position = 'dodge', width = 0.6) +
  ylim(0, 420) +
  geom_text(aes(x = Defined., y = X.Cited, label = Last.Name),
            position = position_dodge(width = 2), 
            vjust = 0, hjust = -2, size = 3) +
  scale_color_manual(values=c("grey1", "yellow3")) +
  labs(x = "", color='Terminology Defined?') +
  ylab("Number of Citations") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(), 
        legend.position = "")

ggsave("NumberCited2.png", width = 8, height = 5)


## Maps
SOF = read.csv("Season of Fire Papers - New Work After 5_28.csv")
SOF$YearName <- paste(SOF$Year, SOF$Last.Name, sep=" ")

SOF$Latitude1 <- as.numeric(as.character(SOF$Latitude))
SOF$Longitude1 <- as.numeric(as.character(SOF$Longitude))

qmplot(Longitude1, Latitude1, data=SOF)

register_google(key = "AIzaSyAvVGbAlCCNtPhgx77kDMFowE9LorppU9Q")

map <- get_map(location = c(lon = mean(-85), lat = mean(33)), scale = 4, 
               zoom = 5, source = "stamen", maptype = "toner")       
ggmap(map) +
  geom_point(data = SOF, aes(x=Longitude1, y=Latitude1, size = X.Cited, 
                             color = Color), alpha = 0.5) +
  scale_color_manual(values=c("Blue", "Brown", "Green", "Grey", "Orange", "Pink",
                              "Purple", "Red", "White"), name = "Terminology", 
                     labels = c("Dry-Wet", "Multiple","Dry-Wet+Lightning", "NA",
                                "Dormant-Growing+Lightning", "Winter-Summer+Dormant-Growing",
                                "Dry-Wet+Dormant-Growing", "Dormant-Growing", "Winter-Summer")) +
  scale_x_continuous(limits=c(-95, -77)) +
  scale_y_continuous(limits=c(24, 36)) 
 

##ggplot(SOF2) +
#  geom_segment(aes(x = GrowingStart, xend = GrowingEnd, y= Year.Published, 
#                   yend = Year.Published), size = 6, colour = "springgreen3", alpha = 0.4) +
#  geom_segment(aes(x = WetStart, xend = WetEnd, y= Year.Published, 
#                   yend = Year.Published), size = 4, colour = "dodgerblue") +
#  geom_segment(aes(x = LightningStart, xend = LightningEnd, y= Year.Published, 
#                   yend = Year.Published), size = 2, colour = "yellow2") +
#  geom_segment(aes(x = GrowingStartEx, xend = GrowingEndEx, y= Year.Published, 
#                   yend = Year.Published), size = 1, linetype = 1) +
#  geom_segment(aes(x = WetStartEx, xend = WetEndEx, y= Year.Published, 
#                   yend = Year.Published), size = 1, linetype = 1) +
#  geom_segment(aes(x = LightningStartEx, xend = LightningEndEx, y= Year.Published, 
#                   yend = Year.Published), size = 1, linetype = 1) +
#  scale_x_date(breaks = seq(as.Date("2019-01-01"), as.Date("2019-12-31"), 
#                            by="1 months"), date_labels = "%B", 
#               limits = as.Date(c("2019-01-01", "2019-12-31"))) +
#  scale_y_date(breaks = seq(as.Date("1988-01-01"), as.Date("2018-12-31"), 
#                            by="2 years"), date_labels = "%Y", 
#               limits = as.Date(c("1988-01-01", "2018-12-31"))) +
#  xlab("") + 
#  ylab("Papers & Years Published") +
#  geom_vline(xintercept=Lightning1, size=1, color="red") +
#  geom_vline(xintercept=Lightning2, size=1, color="red") +
#  ggtitle(label = "Years and Defined Terminologies") +
#  theme_bw() +
#  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
# theme(plot.title = element_text(hjust = 0.5))##

#SOF2 = filter(SOF2, GrowingStart != "." | WetStart != "." | LightningStart != ".")
