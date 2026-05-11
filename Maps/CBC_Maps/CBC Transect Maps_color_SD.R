library(nlme)
library(plyr)
#install.packages('car')
library(car)
library(tidyverse)
library(reshape2)
#install.packages('ggmap')
library(ggmap)
#install.packages('sp')
library(sp)
#install.packages('ggrepel')
library(ggrepel)
library(ggplot2)
library(RColorBrewer)

getwd()

setwd('/Users/brookesienkiewicz/Documents/Code_notebook/Maps/CBC_Maps/')
corals <- read_csv("/Users/brookesienkiewicz/Documents/sctld/SCTLD_samples/Sample_Data/CBC_ColonyData.csv")

corals$Direction <- dplyr::recode(corals$Direction, "L" = "left",
                                  "R" = "right")

# make sure datedoc mortality is updated 
  # view conditions
conditions <- corals %>% 
  select(contains("Condition"))

# Get mortality date per row
mortality_date <- apply(conditions, 1, function(x) {
  dead_idx <- which(x == "Dead")
  if (length(dead_idx) == 0) NA else names(x)[dead_idx[1]]
})

# Add to df
conditions <- conditions %>%
  mutate(Date_DocMortality = mortality_date)

test <- corals %>% 
  mutate(Date_DocMortality = conditions$Date_DocMortality)
# show side by side 
test<-test %>%
  select(NewTagNum, Species, Transect, Date_DocumentedMortality, Date_DocMortality, `062025_Condition`)
# look at which were not visited last time 
not_visited<-test %>%
    filter(`062025_Condition` == 'Not_Visited')

coral_subset<-corals %>%
    select(Transect,NewTagNum, Species, Meter,Meters_90,Direction,MaxDiameter,Height,Date_DocumentedMortality,`062025_Condition`,Notes_062025)%>%
    rename(Condition = "062025_Condition")

# update ones we know are dead
coral_subset<-coral_subset %>%
    mutate(Condition = case_when(
      NewTagNum == '7' & Species == 'PAST' & Transect == "LAGOON"~ "Check",
      NewTagNum == '8' & Species == 'PAST' & Transect == "LAGOON"~ "Check",
      NewTagNum == '24' & Species == 'PAST' & Transect == "LAGOON"~ "Check",
      NewTagNum == '24' & Species == 'SSID' & Transect == "LAGOON"~ "Dead",
      NewTagNum == '26' & Species == 'SSID' & Transect == "LAGOON"~ "Check",
      NewTagNum == '66' & Species == 'OFAV' & Transect == "LAGOON"~ "Healthy",
      NewTagNum == '12flag' & Species == 'PAST' & Transect == "LAGOON" ~ "Dead",
      NewTagNum == '20' & Species == 'PAST' & Transect == "CBC30N"~ "Check", #based on 062025 notes
      NewTagNum == '341' & Species == 'SSID' & Transect == "SR30N" ~ "Dead",
      TRUE ~ Condition
    ))

# use last known condition 
coral_subset <- coral_subset %>% 
  mutate(Meters_90 = ifelse(Direction == "left",
                                               -Meters_90, Meters_90)) %>%
  mutate(MaxDiameter = ifelse(is.na(MaxDiameter),
                              40, MaxDiameter)) %>%
  # add check column
  mutate(check = ifelse(Condition == 'Check', 'y','n'))
  
coral_subset$MaxDiameter <- as.numeric(coral_subset$MaxDiameter)
coral_subset$Condition <- as.factor(coral_subset$Condition)
coral_subset$Species <- dplyr::recode(coral_subset$Species, "OANN/OFAV?" = "OANN")
# coral_subset$use_immune <- as.factor(coral_subset$'immune_y/n')

##CBC30N
CBC30N <- coral_subset %>% subset(Transect == "CBC30N") %>%
  subset(!(is.na(NewTagNum)))

unique(coral_subset$Condition)

specalpha = c('Dead'= 1,'Diseased'= 0,'Healthy'= 0, "CLP")
speccolors = c('SSID'='red3','MCAV'='darkorchid4','PAST'='orange',
               'MMEA' = 'black', 'PSTR' ='green4', 
               'CNAT' = 'lightgoldenrod', 'OFAV' = 'pink', 'OANN' = 'dodgerblue',
               'DLAB' = 'tan4')


tiff("CBC30N_color.tif",width = 6, height = 8, units = "in", res = 300)
ggplot() +
  geom_point(data=CBC30N, aes(x = Meters_90, y = Meter, fill = Species, size = MaxDiameter, shape = check), color = "black") +
  scale_fill_manual(values=c(speccolors), guide = guide_legend(override.aes = list(pch = 21, size = 5))) +
  geom_point(data=CBC30N, aes(x = Meters_90, y = Meter, alpha = Condition == 'Dead'),
             pch = 4, color = "snow", stroke = 0.5) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0), guide = 'none')+
  geom_vline(xintercept = 0, lty = 2, lwd = 0.25) +
  geom_text_repel(data=CBC30N, aes(x=Meters_90, y=Meter, label=NewTagNum), max.overlaps = 100, color="black", size = 4, hjust=-0.25,
                  nudge_x = 0.1,
                  box.padding = 0.5, point.padding = 0.5)+
  scale_y_continuous("Transect Length (m)", breaks = seq(0, 31, by = 1)) +
  scale_x_continuous("Meters Perpendicular", breaks = seq(-5, 7, by = 1)) +
  scale_size_continuous(range = c(2,6.5), name = "", guide = 'none') +
  scale_shape_manual(values = c(21,24)) +
  ggtitle("CBC30N") +
  theme(plot.title = element_text(size = 12,hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", hjust = 1, size = 12),
        axis.title = element_text(size = 12)) +
  theme(legend.key.size = unit(1.5, "line"))
dev.off()



##CBC Lagoon

# change coordinates of coral 18 so it can be seen in the map 
coral_subset <- coral_subset %>%
  mutate(Meters_90 = if_else(Species == "PAST" & Transect == "LAGOON" & NewTagNum == "18",
                             2.2,
                             Meters_90))
coral_subset <- coral_subset %>%
  mutate(Meters_90 = if_else(Species == "SSID" & Transect == "LAGOON" & NewTagNum == "19",
                             2.6,
                             Meters_90)) %>%
  mutate(Meter = if_else(Species == "SSID" & Transect == "LAGOON" & NewTagNum == "19",
                             15,
                             Meter)) %>%
  mutate(Meters_90 = if_else(Species == "MCAV" & Transect == "LAGOON" & NewTagNum == "12",
                       2.7,
                       Meters_90))
  mutate(Meters_90 = if_else(Species == "MCAV" & Transect == "LAGOON" & NewTagNum == "14",
                           -1,
                           Meters_90))

Lagoon <- coral_subset %>% subset(Transect == "LAGOON") %>%
  subset(NewTagNum != "12flag") #%>%
  # subset(NewTagNum != "17") # exactly overlaps with an alive coral

  #14 and 15 MCAV overlap - fixed
  
tiff("CBCLagoon_color.tif",width = 6, height = 8, units = "in", res = 300)
ggplot() +
  geom_point(data=Lagoon, aes(x = Meters_90, y = Meter, fill = Species, size = MaxDiameter, shape = check), color = "black") +
  scale_fill_manual(values=c(speccolors), guide = guide_legend(override.aes = list(pch = 21, size = 5))) +
  geom_point(data=Lagoon, aes(x = Meters_90, y = Meter, alpha = Condition == 'Dead'),
             pch = 4, color = "snow", stroke = 0.5) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0), guide = 'none')+
  geom_vline(xintercept = 0, lty = 2, lwd = 0.25) +
  geom_text_repel(data=Lagoon, aes(x=Meters_90, y=Meter, label=NewTagNum), max.overlaps = 100, color="black", size = 4, hjust=-0.25,
                  nudge_x = 0.1,
                  box.padding = 0.4, point.padding = 0.5)+
  scale_y_continuous("Transect Length (m)", breaks = seq(0, 42, by = 1)) +
  scale_x_continuous("Meters Perpendicular", breaks = seq(-12, 7, by = 1)) +
  scale_size_continuous(range = c(2,6.5), name = "", guide = 'none') +
  scale_shape_manual(values = c(21,24)) +
  ggtitle("CBC Lagoon") +
  theme(plot.title = element_text(size = 12,hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", hjust = 1, size = 12),
        axis.title = element_text(size = 12)) +
  theme(legend.key.size = unit(1.5, "line"))
dev.off()



#SR30N
SR30N <- coral_subset %>% subset(Transect == "SR30N") %>%
  mutate(NewTagNum = as.numeric(NewTagNum)) %>%
  subset(NewTagNum < 300) %>%
  subset(NewTagNum != "51")

tiff("SR30N_color4.tif",width = 6, height = 8, units = "in", res = 300)
ggplot() +
  geom_point(data=SR30N, aes(x = Meters_90, y = Meter, fill = Species, size = MaxDiameter), shape = 21, color = "black") +
  scale_fill_manual(values=c(speccolors), guide = guide_legend(override.aes = list(pch = 21, size = 5))) +
  geom_point(data=SR30N, aes(x = Meters_90, y = Meter, alpha = Condition == 'Dead'),
             pch = 4, color = "snow", stroke = 0.5) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0), guide = 'none')+
  geom_vline(xintercept = 0, lty = 2, lwd = 0.25) +
  geom_text_repel(data=SR30N, aes(x=Meters_90, y=Meter, label=NewTagNum), max.overlaps = 100, color="black", size = 4, hjust=-0.25,
                  nudge_x = 0.2,
                  box.padding = 0.4, point.padding = 0.4
                  )+
  scale_y_continuous("Transect Length (m)", breaks = seq(0, 42, by = 1)) +
  scale_x_continuous("Meters Perpendicular", breaks = seq(-12, 7, by = 1)) +
  scale_size_continuous(range = c(2,6.5), name = "", guide = 'none') +
  # scale_shape_manual(values = c(21, 24)) +
  ggtitle("SR30N") +
  theme(plot.title = element_text(size = 12,hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", hjust = 1, size = 12),
        axis.title = element_text(size = 12)) +
  theme(legend.key.size = unit(1.5, "line"))
  
dev.off()

#Curlew
Curlew <- coral_subset %>% subset(Transect == "CURLEW")

tiff("Curlew_color.tif",width = 5, height = 8, units = "in", res = 300)
ggplot() +
  geom_point(data=Curlew, aes(x = Meters_90, y = Meter, fill = Species, size = MaxDiameter),shape = 21, color = "black", ) +
  scale_fill_manual(values=c(speccolors), guide = guide_legend(override.aes = list(pch = 21, size = 5))) +
  geom_point(data=Curlew, aes(x = Meters_90, y = Meter, alpha = Condition == 'Dead'),
             pch = 4, color = "snow", stroke = 0.5) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0), guide = 'none')+
  geom_vline(xintercept = 0, lty = 2, lwd = 0.25) +
  geom_text_repel(data=Curlew, aes(x=Meters_90, y=Meter, label=NewTagNum), max.overlaps = 100, color="black", size = 4, hjust=-0.5) +
  scale_y_continuous("Transect Length (m)", breaks = seq(0, 42, by = 1)) +
  scale_x_continuous("Meters Perpendicular", breaks = seq(-6, 10, by = 1)) +
  scale_size_continuous(range = c(2,6.5), name = "", guide = 'none') +
  # scale_shape_manual(values = c(21, 24)) +
  ggtitle("Curlew") +
  theme(plot.title = element_text(size = 12,hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", hjust = 1, size = 12),
        axis.title = element_text(size = 12)) + 
  theme(legend.key.size = unit(1.5, "line"))

dev.off()


#BB
BB <- coral_subset %>% subset(Transect == "BB")

tiff("BB_color.tif",width = 5, height = 8, units = "in", res = 300)
ggplot() +
  geom_point(data=BB, aes(x = Meters_90, y = Meter, fill = Species, size = MaxDiameter),shape = 21, color = "black", ) +
  scale_fill_manual(values=c(speccolors), guide = guide_legend(override.aes = list(pch = 21, size = 5))) +
  geom_point(data=BB, aes(x = Meters_90, y = Meter, alpha = Condition == "Dead"),
             pch = 4, color = "snow", stroke = 0.5) +
  geom_vline(xintercept = 0, lty = 2, lwd = 0.25) +
  geom_text_repel(data=BB, aes(x=Meters_90, y=Meter, label=NewTagNum), max.overlaps = 100, color="black", size = 4, hjust=-0.5) +
  scale_y_continuous("Transect Length (m)", breaks = seq(0, 42, by = 1)) +
  scale_x_continuous("Meters Perpendicular", breaks = seq(-6, 10, by = 1)) +
  scale_size_continuous(range = c(2,6.5), name = "", guide = 'none') +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0), guide = 'none')+
  # scale_alpha_manual(values=c(specalpha), guide = 'none') +
  # scale_shape_manual(values = c(21, 24)) +
  ggtitle("Bread and Butter") +
  theme(plot.title = element_text(size = 12,hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", hjust = 1, size = 12),
        axis.title = element_text(size = 12)) + 
  theme(legend.key.size = unit(1.5, "line"))

dev.off()

#Hangman
HANGMAN <- coral_subset %>% subset(Transect == "HANGMAN")

tiff("HANGMAN_color.tif",width = 5, height = 8, units = "in", res = 300)
ggplot() +
  geom_point(data=HANGMAN, aes(x = Meters_90, y = Meter, fill = Species, size = MaxDiameter),shape = 21, color = "black", ) +
  scale_fill_manual(values=c(speccolors), guide = guide_legend(override.aes = list(pch = 21, size = 5))) +
  geom_point(data=HANGMAN, aes(x = Meters_90, y = Meter, alpha = Condition == "Dead"),
             pch = 4, color = "snow", stroke = 0.5) +
  geom_vline(xintercept = 0, lty = 2, lwd = 0.25) +
  geom_text_repel(data=HANGMAN, aes(x=Meters_90, y=Meter, label=NewTagNum), max.overlaps = 100, color="black", 
                  size = 4, hjust=-0.6, nudge_x = 0.1,
                  box.padding = 0.4, point.padding = 0.5) +
  scale_y_continuous("Transect Length (m)", breaks = seq(0, 42, by = 1)) +
  scale_x_continuous("Meters Perpendicular", breaks = seq(-6, 10, by = 1)) +
  scale_size_continuous(range = c(2,6.5), name = "", guide = 'none') +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0), guide = 'none')+
  ggtitle("Hangman") +
  theme(plot.title = element_text(size = 12,hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", hjust = 1, size = 12),
        axis.title = element_text(size = 12)) + 
  theme(legend.key.size = unit(1.5, "line"))

dev.off()


