### clear everything ###
rm(list=ls(all=TRUE)) 
cat("\014") 

list.of.packages <- c("ggplot2", "devtools", "reshape2", "gridExtra", "tidyverse", "vegan",
                      "ggpubr", "grid", "AMR", "scales", "dplyr", "maps", 
                      "sp", "rgdal", "sf", "remotes", "ggrepel", "RgoogleMaps",
                      "mapproj", "magrittr", install_github("dkahle/ggmap", ref = "tidyup"))
new.packages <- list.of.packages[!(list.of.packages %in% 
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(devtools)
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
library(ggrepel)
library(RgoogleMaps)
library(mapproj)
library(ggmap)
library(magrittr)

################################# Season of Fire (SOF) Graphs ####################################################

## Read SOF File ##
SOF = read.csv("Season of Fire Papers - New Work After 5_28.csv")
SOF$YearName <- paste(SOF$Year, SOF$Last.Name, sep=" ")

## Dates Graph ##
## First select data from SOF and the Filter to omit NA values ##
SOF2 = dplyr::select(SOF, YearName, Last.Name, Year.Published, X.Cited, DormantStart, DormantEnd, GrowingStart, 
              GrowingEnd, DormantStart2, DormantEnd2, DryStart1, DryEnd1, WetStart, WetEnd, DryStart2, DryEnd2,
              LightningStart, LightningEnd, Calendar.MonthsStart, Calendar.MonthsEnd, 
              Calendar.MonthsStart2, Calendar.MonthsEnd2, WinterStartEx, WinterEndEx, SpringStartEx, SpringEndEx, 
              SummerStartEx, SummerEndEx, FallStartEx, FallEndEx, WinterStartEx2, WinterEndEx2, DormantStartEx, 
              DormantEndEx, GrowingStartEx, GrowingEndEx, GrowingStartEx2, GrowingEndEx2, DormantStartEx2, DormantEndEx2,
              DryStartEx, DryEndEx, WetStartEx, WetEndEx, LightningStartEx, LightningEndEx, Latitude1, Color)
SOF3 = dplyr::select(SOF2, YearName, Last.Name, Year.Published, X.Cited, DormantStart, DormantEnd, GrowingStart, 
              GrowingEnd, DormantStart2, DormantEnd2, DryStart1, DryEnd1, WetStart, WetEnd, DryStart2, DryEnd2,
              LightningStart, LightningEnd, Latitude1, Color)
SOF3 =  filter(SOF3, DormantStart != "." | GrowingStart != "." | WetStart != "." | LightningStart != ".")

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

####################################### SOF Authors & Defined Terms ############################################

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
SOF3$Year.Published <- lubridate::ymd(SOF3$Year.Published)
Lightning1 <- lubridate::ymd("2019-04-15")
Lightning2 <- lubridate::ymd("2019-06-15")

SOF3$Latitude1 = as.numeric(SOF3$Latitude1)
SOF3$YearName = as.character(SOF3$YearName)
SOF3$YearName <- reorder(SOF3$YearName, SOF3$Latitude1)


ggplot(SOF3) +
  geom_segment(aes( x = DormantStart, xend = DormantEnd, y= YearName, 
                    yend = YearName, color = "darkorange1"), size = 6, alpha = 0.5) +
  geom_segment(aes( x = GrowingStart, xend = GrowingEnd, y= YearName, 
                    yend = YearName, color = "springgreen3"), size = 6, alpha = 0.5) +
  geom_segment(aes( x = DormantStart2, xend = DormantEnd2, y= YearName, 
                    yend = YearName, color = "darkorange1"), size = 6, alpha = 0.5) +
  geom_segment(aes( x = DryStart1, xend = DryEnd1, y= YearName, 
                    yend = YearName, color = "orchid2"), size = 6, alpha = 0.5) +
  geom_segment(aes( x = WetStart, xend = WetEnd, y= YearName, 
                    yend = YearName, color = "dodgerblue"), size = 6) +
  geom_segment(aes( x = DryStart2, xend = DryEnd2, y= YearName, 
                    yend = YearName, color = "orchid2"), size = 6, alpha = 0.5) +
  geom_segment(aes( x = LightningStart, xend = LightningEnd, y= YearName, 
                    yend = YearName, color = "yellow2"), size = 1.5) +
  scale_color_manual(values = c("darkorange1" = "darkorange1", "springgreen3" = "springgreen3", 
                                "orchid2" = "orchid2", "dodgerblue" = "dodgerblue", 
                                "Orange" = "Orange", "Pink" = "Pink",
                               "yellow2" = "yellow2"), name = "Terminology", 
                    labels = c("darkorange1" = "Dormant", "springgreen3" = "Growing", 
                               "orchid2" = "Dry","dodgerblue" = "Wet", "yellow2" = "Lightning")) +
  scale_x_date(breaks = seq(as.Date("2019-01-01"), as.Date("2019-12-31"), 
                            by="1 months"), date_labels = "%b", 
               limits = as.Date(c("2019-01-01", "2019-12-31"))) +
  xlab("") + 
  ylab("Authors") +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black", size = 12, face = "bold"),
        axis.text.y = element_text(color = "black", size = 12, face = "bold"),  
        axis.title.x = element_text(color = "black", size = 12, face = "bold"),
        axis.title.y = element_text(color = "black", size = 12, face = "bold"),
        title = element_text(color = "black", size = 12, face = "bold"),
        legend.text = element_text(color = "black", size = 12, face = "bold"),
        legend.title=element_text(color = "black", size=12),
        plot.title = element_text(hjust = 0.5),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm"))
ggsave("Authors&DefinedTerms_Lat.png", width = 10, height = 6)

############################### Influence of SOF Across Papers in SE ######################################
sum(SOF$X.Cited)
CitationsDefined = select(SOF, Defined., X.Cited)
Yes = filter(CitationsDefined, Defined. == "Yes")
No = filter(CitationsDefined, Defined. == "No")
sum(Yes$X.Cited)
sum(No$X.Cited)
DG = filter(SOF, Dormant.Growing == "Yes")
sum(DG$X.Cited)
DW = filter(SOF, Dry.Wet == "Yes")
sum(DW$X.Cited)
L = filter(SOF, Lightning == "Yes")
sum(L$X.Cited)
A = filter(SOF, Astronomical.Seasons == "Yes")
sum(A$X.Cited)

ggplot(SOF, aes(YearName, X.Cited, fill = Defined.)) +
  geom_bar(stat = "identity", size=1, color = "grey30", width=.65) +
  scale_fill_manual(values=c("white", "gold")) +
  labs(x = "", color='Terminology Defined') +
  ylab("Total Number of Citations") +
  guides(fill=guide_legend(title="Defined", reverse = TRUE) ) +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black", size = 10, face = "bold", 
                                   angle = 90, hjust=0.95,vjust=0.2),
        axis.text.y = element_text(color = "black", size = 12, face = "bold"),  
        axis.title.x = element_text(color = "black", size = 10, face = "bold"),
        axis.title.y = element_text(color = "black", size = 12, face = "bold"),
        title = element_text(color = "black", size = 12, face = "bold"),
        legend.text = element_text(color = "black", size = 10, face = "bold"),
        legend.title=element_text(color = "black", size=10),
        plot.title = element_text(hjust = 0.5),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm"))

 ggsave("NumberCited2.png", width = 8, height = 5)

 
############################### Influence of SOF Terms Across SE USA ############################################

SOF = read.csv("Season of Fire Papers - New Work After 5_28.csv")
SOF$YearName <- paste(SOF$Year, SOF$Last.Name, sep=" ")

SOF$Latitude1 <- as.numeric(as.character(SOF$Latitude))
SOF$Longitude1 <- as.numeric(as.character(SOF$Longitude))

register_google(key = "AIzaSyAvVGbAlCCNtPhgx77kDMFowE9LorppU9Q")

map <- get_googlemap(center=c(lon = -85, lat = 30), 
               zoom = 5, source = "google", maptype = "roadmap",
               style="feature:all|element:labels|visibility:off")       

ggmap(map) +
  geom_point(data = SOF, aes(x = Longitude1, y = Latitude1, size = X.Cited, fill = Color),
             position = position_jitterdodge(jitter.width = 0.6, jitter.height = 0.2, seed = 1),
             alpha = 0.5, stroke = 1.5, pch=21) +
  scale_fill_manual(values = c("Blue" = "Blue", "Brown" = "Brown", "Green" = "Green", 
                               "Grey" = "Black", "Orange" = "Orange", "Pink" = "Pink",
                                "Purple" = "Purple", "Red" = "Red", "White" = "Grey"), name = "Terminology", 
                     labels = c("Blue" = "Dry-Wet", "Brown" = "Multiple", 
                                "Green" = "Dry-Wet+Lightning", "Grey" = "NA", 
                                "Orange" = "Dormant-Growing+Lightning", 
                                "Pink" = "Winter-Summer+Dormant-Growing", 
                                "Purple" = "Dry-Wet+Dormant-Growing",
                                "Red" = "Dormant-Growing", "White" = "Winter-Summer")) +
  geom_label_repel(data = SOF, aes(x=Longitude1, y=Latitude1, label = ifelse(X.Cited>1000,as.character(YearName),'')),
                   fill = "transparent", color = "grey2",
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'transparent') +
  scale_size_continuous(range = c(3, 22)) +
  labs(x = "Longitude", y = "Latitude", size ='Number of Citations')  +
  scale_x_continuous(limits=c(-95, -77)) +
  scale_y_continuous(limits=c(24, 36)) +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black", size = 10, face = "bold", 
                                   angle = 90, hjust=0.95,vjust=0.2),
        axis.text.y = element_text(color = "black", size = 12, face = "bold"),  
        axis.title.x = element_text(color = "black", size = 10, face = "bold"),
        axis.title.y = element_text(color = "black", size = 12, face = "bold"),
        title = element_text(color = "black", size = 12, face = "bold"),
        legend.text = element_text(color = "black", size = 10, face = "bold"),
        legend.title=element_text(color = "black", size=10),
        plot.title = element_text(hjust = 0.5),
        legend.key = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size=10)))

ggsave("InfluenceMap.png", width = 10, height = 8)
 