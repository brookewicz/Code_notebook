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

setwd('/Users/brookesienkiewicz/Documents/Code_notebook')

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
  select(NewTagNum, Species, Transect, Date_DocumentedMortality, Date_DocMortality)

# cbc30n - past 19 in 042024, and past 21 in 082024, 51 ssid in sr30n 122022??, 64 cnat in sr30n in 012024, 67 ssid in sr30n in 062024, 
#6 past & 8 past lagoon in 012024, are 24 past and 26 ssid in lagoon dead? says they died in 5/22, 96 pstr curley in 092023,


str(corals$Date_DocumentedMortality)


corals <- corals %>% mutate(Meters_90 = ifelse(Direction == "left",
                                               -Meters_90, Meters_90)) %>%
  mutate(Condition = ifelse(Date_DocumentedMortality != "Healthy"& Date_DocumentedMortality != "Diseased",
                            "Dead", Date_DocumentedMortality)) %>%
  mutate(MaxDiameter = ifelse(is.na(MaxDiameter),
                              40, MaxDiameter))

specalpha = c('Dead'= 1,'Diseased'= 0,'Healthy'= 0)

speccolors = c('SSID'='red3','MCAV'='darkorchid4','PAST'='orange',
              'MMEA' = 'black', 'PSTR' ='green4', 
              'CNAT' = 'lightgoldenrod', 'OFAV' = 'pink', 'OANN' = 'dodgerblue',
              'DLAB' = 'tan4')

corals$MaxDiameter <- as.numeric(corals$MaxDiameter)



corals$Condition <- as.factor(corals$Condition)

corals$Species <- dplyr::recode(corals$Species, "OANN/OFAV?" = "OANN")


corals$use_immune <- as.factor(corals$'immune_y/n')

##CBC30N
CBC30N <- corals %>% subset(Transect == "CBC30N") %>%
  subset(!(is.na(NewTagNum)))


tiff("CBC30N_color.tif",width = 6, height = 8, units = "in", res = 300)
ggplot() +
  geom_point(data=CBC30N, aes(x = Meters_90, y = Meter, fill = Species, size = MaxDiameter, shape = use_immune), color = "black") +
  scale_fill_manual(values=c(speccolors), guide = guide_legend(override.aes = list(pch = 21, size = 5))) +
  geom_point(data=CBC30N, aes(x = Meters_90, y = Meter, alpha = Condition),
             pch = 4, color = "snow", stroke = 0.5) +
  geom_vline(xintercept = 0, lty = 2, lwd = 0.25) +
  geom_text_repel(data=CBC30N, aes(x=Meters_90, y=Meter, label=NewTagNum), max.overlaps = 100, color="black", size = 4, hjust=-0.25,
                  #nudge_x = 0.1,
                  box.padding = 0.5, point.padding = 0.5)+
  scale_y_continuous("Transect Length (m)", breaks = seq(0, 31, by = 1)) +
  scale_x_continuous("Meters Perpendicular", breaks = seq(-5, 7, by = 1)) +
  scale_size_continuous(range = c(2,6.5), name = "", guide = 'none') +
  scale_alpha_manual(values=c(specalpha), guide = 'none') +
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
corals <- corals %>%
  mutate(Meters_90 = if_else(Species == "PAST" & Transect == "LAGOON" & NewTagNum == "18", 
                             2.2, 
                             Meters_90))
corals <- corals %>%
  mutate(Meters_90 = if_else(Species == "SSID" & Transect == "LAGOON" & NewTagNum == "19", 
                             2.6, 
                             Meters_90)) %>%
  mutate(Meter = if_else(Species == "SSID" & Transect == "LAGOON" & NewTagNum == "19", 
                             15, 
                             Meter))

Lagoon <- corals %>% subset(Transect == "LAGOON") %>%
  subset(NewTagNum != "12flag") #%>%
  #subset(NewTagNum != "17") # exactly overlaps with an alive coral 

tiff("CBCLagoon_color.tif",width = 6, height = 8, units = "in", res = 300)
ggplot() +
  geom_point(data=Lagoon, aes(x = Meters_90, y = Meter, fill = Species, size = MaxDiameter, shape = use_immune), color = "black") +
  scale_fill_manual(values=c(speccolors), guide = guide_legend(override.aes = list(pch = 21, size = 5))) +
  geom_point(data=Lagoon, aes(x = Meters_90, y = Meter, alpha = Condition),
             pch = 4, color = "snow", stroke = 0.5) +
  geom_vline(xintercept = 0, lty = 2, lwd = 0.25) +
  geom_text_repel(data=Lagoon, aes(x=Meters_90, y=Meter, label=NewTagNum), max.overlaps = 100, color="black", size = 4, hjust=-0.25,
                  #nudge_x = 0.1,
                  box.padding = 0.4, point.padding = 0.5)+
  scale_y_continuous("Transect Length (m)", breaks = seq(0, 42, by = 1)) +
  scale_x_continuous("Meters Perpendicular", breaks = seq(-12, 7, by = 1)) +
  scale_size_continuous(range = c(2,6.5), name = "", guide = 'none') +
  scale_alpha_manual(values=c(specalpha), guide = 'none') +
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

# update location of newl

SR30N <- corals %>% subset(Transect == "SR30N") %>%
  subset(NewTagNum != "not_found")

tiff("SR30N_color4.tif",width = 6, height = 8, units = "in", res = 300)
ggplot() +
  geom_point(data=SR30N, aes(x = Meters_90, y = Meter, fill = Species, size = MaxDiameter, shape = use_immune), color = "black") +
  scale_fill_manual(values=c(speccolors), guide = guide_legend(override.aes = list(pch = 21, size = 5))) +
  geom_point(data=SR30N, aes(x = Meters_90, y = Meter, alpha = Condition),
             pch = 4, color = "snow", stroke = 0.5) +
  geom_vline(xintercept = 0, lty = 2, lwd = 0.25) +
  geom_text_repel(data=SR30N, aes(x=Meters_90, y=Meter, label=NewTagNum), max.overlaps = 100, color="black", size = 4, hjust=-0.25,
                  nudge_x = 0.1,
                  box.padding = 0.3, point.padding = 0.3
                  )+
  scale_y_continuous("Transect Length (m)", breaks = seq(0, 42, by = 1)) +
  scale_x_continuous("Meters Perpendicular", breaks = seq(-12, 7, by = 1)) +
  scale_size_continuous(range = c(2,6.5), name = "", guide = 'none') +
  scale_alpha_manual(values=c(specalpha), guide = 'none') +
  scale_shape_manual(values = c(21, 24)) +
  ggtitle("SR30N") +
  theme(plot.title = element_text(size = 12,hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", hjust = 1, size = 12),
        axis.title = element_text(size = 12)) +
  theme(legend.key.size = unit(1.5, "line"))
  
dev.off()

#Curlew
Curlew <- corals %>% subset(Transect == "CURLEW")

tiff("Curlew_color.tif",width = 5, height = 8, units = "in", res = 300)
ggplot() +
  geom_point(data=Curlew, aes(x = Meters_90, y = Meter, fill = Species, size = MaxDiameter, shape = use_immune),color = "black") +
  scale_fill_manual(values=c(speccolors), guide = guide_legend(override.aes = list(pch = 21, size = 5))) +
  geom_point(data=Curlew, aes(x = Meters_90, y = Meter, alpha = Condition),
             pch = 4, color = "snow", stroke = 0.5) +
  geom_vline(xintercept = 0, lty = 2, lwd = 0.25) +
  geom_text_repel(data=Curlew, aes(x=Meters_90, y=Meter, label=NewTagNum), max.overlaps = 100, color="black", size = 4, hjust=-0.5) +
  scale_y_continuous("Transect Length (m)", breaks = seq(0, 42, by = 1)) +
  scale_x_continuous("Meters Perpendicular", breaks = seq(-6, 10, by = 1)) +
  scale_size_continuous(range = c(2,6.5), name = "", guide = 'none') +
  scale_alpha_manual(values=c(specalpha), guide = 'none') +
  scale_shape_manual(values = c(21, 24)) +
  ggtitle("Curlew") +
  theme(plot.title = element_text(size = 12,hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", hjust = 1, size = 12),
        axis.title = element_text(size = 12)) + 
  theme(legend.key.size = unit(1.5, "line"))

dev.off()


