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

################# Create Summary of Terminology Used in Papers & Graph #########################################

SOF1 = dplyr::select(SOF, Dormant.Growing, Astronomical.Seasons, Dry.Wet, Lightning, X.Cited, Color)

TermCounts <- colSums(SOF1 == "Yes")
TermCounts = as.data.frame(TermCounts)
TermCounts <- cbind(rownames(TermCounts), TermCounts)
colnames(TermCounts)[colnames(TermCounts)=="rownames(TermCounts)"] <- "Terminologies"
colnames(TermCounts)[colnames(TermCounts)=="TermCounts"] <- "Count"
TermCounts = as.data.frame(TermCounts)

ggplot() +
  geom_bar(data = TermCounts, aes(Terminologies, Count), 
           stat = "identity", alpha=0.3) +
  ylim(0, 40) +
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values=c("gold", "grey30")) +
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


ggplot(SOF, aes(YearName, X.Cited, fill = Color)) +
  geom_bar(stat = "identity", size=1, color = "grey30") +
  scale_fill_manual(values = c("Blue" = "Blue", "Brown" = "Brown", "Green" = "Green", "Grey" = "Black", "Orange" = "Orange", "Pink" = "Pink",
                               "Purple" = "Purple", "Red" = "red1", "White" = "Grey"), name = "Terminology", 
                    labels = c("Blue" = "Dry-Wet", "Brown" = "Multiple", "Green" = "Dry-Wet+Lightning", "Grey" = "NA",
                               "Orange" = "Dormant-Growing+Lightning", "Pink" = "Winter-Summer+Dormant-Growing",
                               "Purple" = "Dry-Wet+Dormant-Growing","Red" = "Dormant-Growing", "White" = "Winter-Summer")) +
  labs(x = "", color='Terminology Defined?') +
  ylab("Number of Citations") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(), 
        legend.position = c(.85,.65)) +
  guides(fill=guide_legend(title="Terminologies Used", reverse = TRUE) )


## Terminolgy Definitions by Latitude

ggplot(SOF3) +
  geom_segment(aes( x = DormantStart, xend = DormantEnd, y= Latitude1, 
                    yend = Latitude1), size = 6, colour = "darkorange1", alpha = 0.5) +
  geom_segment(aes( x = GrowingStart, xend = GrowingEnd, y= Latitude1, 
                    yend = Latitude1), size = 6, colour = "springgreen3", alpha = 0.5) +
  geom_segment(aes( x = DormantStart2, xend = DormantEnd2, y= Latitude1, 
                    yend = Latitude1), size = 6, colour = "darkorange1", alpha = 0.5) +
  geom_segment(aes( x = DryStart1, xend = DryEnd1, y= Latitude1, 
                    yend = Latitude1), size = 6, colour = "orchid2", alpha = 0.5) +
  geom_segment(aes( x = WetStart, xend = WetEnd, y= Latitude1, 
                    yend = Latitude1), size = 6, colour = "dodgerblue") +
  geom_segment(aes( x = DryStart2, xend = DryEnd2, y= Latitude1, 
                    yend = Latitude1), size = 6, colour = "orchid2", alpha = 0.5) +
  geom_segment(aes( x = LightningStart, xend = LightningEnd, y= Latitude1, 
                    yend = Latitude1), size = 1.5, colour = "yellow2") +
  scale_x_date(breaks = seq(as.Date("2019-01-01"), as.Date("2019-12-31"), 
                            by="1 months"), date_labels = "%B", 
               limits = as.Date(c("2019-01-01", "2019-12-31"))) +
  ylim(c(24,36)) +
  xlab("") + 
  ylab("Authors") +
  geom_vline(xintercept=Lightning1, size=1, color="red") +
  geom_vline(xintercept=Lightning2, size=1, color="red") +
  ggtitle(label = "Authors and Defined Terminologies") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Authors&DefinedTermsLatitude.png", width = 8, height = 5)
ggsave("AuthorsTermsCited.png", width = 8, height = 5)