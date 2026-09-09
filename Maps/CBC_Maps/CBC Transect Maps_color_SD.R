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
set.seed()

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
# set last date
last_date = "062026"
last_date_column=paste0(last_date, "_Condition")

# show side by side 
test<-test %>%
  select(NewTagNum, Species, Transect, Date_DocumentedMortality, Date_DocMortality, all_of(last_date_column))

# look at which were not visited last time 
not_visited<-test %>%
    filter(last_date_column == 'Not_Visited')
# check on dead or not visited 
conditions<-c('Dead','Not_Visited')
check<-test %>%
    filter(.data[[last_date_column]] %in% conditions & is.na(Date_DocMortality))
# add notes and previous condition
check<-check %>%
    left_join(corals %>% select(Transect,Species,NewTagNum, `Notes_062026`,`062025_Condition`), by = c('Transect','Species','NewTagNum'))
# T6 PAST 10 was missed - need to keep 
# T4 DLAB 43 WAS MISSED 

notes_col = paste0("Notes_",last_date)
notes_col
coral_subset<-corals %>%
    select(Date_InitialTag,Transect,NewTagNum, Species, Meter,Meters_90,Direction,MaxDiameter,Height,Date_DocumentedMortality,all_of(last_date_column), all_of(notes_col))%>%
    rename(Condition = last_date_column)

# update ones we know are dead
# coral_subset<-coral_subset %>%
#     mutate(Condition = case_when(
#       NewTagNum == '7' & Species == 'PAST' & Transect == "LAGOON"~ "Check",
#       NewTagNum == '8' & Species == 'PAST' & Transect == "LAGOON"~ "Check",
#       NewTagNum == '24' & Species == 'PAST' & Transect == "LAGOON"~ "Check",
#       NewTagNum == '24' & Species == 'SSID' & Transect == "LAGOON"~ "Dead",
#       NewTagNum == '26' & Species == 'SSID' & Transect == "LAGOON"~ "Check",
#       NewTagNum == '66' & Species == 'OFAV' & Transect == "LAGOON"~ "Healthy",
#       NewTagNum == '12flag' & Species == 'PAST' & Transect == "LAGOON" ~ "Dead",
#       NewTagNum == '20' & Species == 'PAST' & Transect == "CBC30N"~ "Check", #based on 062025 notes
#       NewTagNum == '341' & Species == 'SSID' & Transect == "SR30N" ~ "Dead",
#       NewTagNum == '47' & Species == 'PAST' & Transect == "LAGOON"~ "Check",
#       TRUE ~ Condition
#     ))

# use last known condition 
coral_subset <- coral_subset %>% 
  mutate(Meters_90 = ifelse(Direction == "left",-Meters_90, Meters_90)) %>%
  mutate(MaxDiameter = ifelse(is.na(MaxDiameter),40, MaxDiameter)) %>%
  # add check column
  mutate(check = ifelse(Condition == 'Check', 'y','n'))
  
coral_subset$MaxDiameter <- as.numeric(coral_subset$MaxDiameter)
coral_subset$Condition <- as.factor(coral_subset$Condition)
coral_subset$Species <- dplyr::recode(coral_subset$Species, "OANN/OFAV?" = "OANN")
# coral_subset$use_immune <- as.factor(coral_subset$'immune_y/n')

unique(coral_subset$Condition)

specalpha = c('Dead'= 1,'Diseased'= 0,'Healthy'= 0, "CLP")
speccolors = c('SSID'='red3','MCAV'='darkorchid4','PAST'='orange',
               'MMEA' = 'black', 'PSTR' ='green4', 
               'CNAT' = 'lightgoldenrod', 'OFAV' = 'pink', 'OANN' = 'dodgerblue',
               'DLAB' = 'tan4')


######## CBC30N ########
CBC30N_all <- coral_subset %>% subset(Transect == "CBC30N") %>%
  subset(!(is.na(NewTagNum)))

# confirm it includes newly tagged colonies
length(unique(CBC30N_all$NewTagNum))
unique(CBC30N_all$Date_InitialTag)

# remove dead 
CBC30N<-CBC30N_all %>%
    filter(Condition != "Dead")

length(unique(CBC30N$NewTagNum))
unique(CBC30N$Date_InitialTag)

tiff("CBC30N_color.tif",width = 6, height = 8, units = "in", res = 300)
ggplot() +
  geom_point(data=CBC30N,aes(x = Meters_90, y = Meter, fill = Species, size = MaxDiameter), alpha = 0.7, shape = 21, color = "black") +
  scale_fill_manual(values=c(speccolors), guide = guide_legend(override.aes = list(pch = 21, size = 5))) +
  geom_point(data=CBC30N, aes(x = Meters_90, y = Meter, alpha = Condition == 'Dead'),
             pch = 4, color = "snow", stroke = 0.5) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0), guide = 'none')+
  geom_vline(xintercept = 0, lty = 2, lwd = 0.25) +
  geom_text_repel(data=CBC30N, aes(x=Meters_90, y=Meter, label=NewTagNum), max.overlaps = 100, color="black", size = 4, hjust=-0.25,
                  nudge_x = 0.1,
                  box.padding = 0.4, point.padding = 0.5)+
  scale_y_continuous("Transect Length (m)", breaks = seq(0, 42, by = 1)) +
  scale_x_continuous("Meters Perpendicular", breaks = seq(-12, 7, by = 1)) +
  scale_size_continuous(range = c(2,6.5), name = "", guide = 'none') +
  scale_shape_manual(values = c(21,24)) +
  labs(title = "CBC CBC30N", shape = "Check if dead") +
  theme(plot.title = element_text(size = 12,hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", hjust = 1, size = 12),
        axis.title = element_text(size = 12)) +
  theme(legend.key.size = unit(1.5, "line"))
dev.off()

# tiff("CBC30N_color2.tif",width = 6, height = 8, units = "in", res = 300)
# ggplot() +
#   geom_point(data=CBC30N,aes(x = Meters_90, y = Meter, fill = Species, size = MaxDiameter), alpha = 0.7, shape = 21, color = "black") +
#   scale_fill_manual(values=c(speccolors), guide = guide_legend(override.aes = list(pch = 21, size = 5))) +
#   geom_point(data=CBC30N, aes(x = Meters_90, y = Meter, alpha = Condition == 'Dead'),
#              pch = 4, color = "snow", stroke = 0.5) +
#   scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0), guide = 'none')+
#   geom_vline(xintercept = 0, lty = 2, lwd = 0.25) +
#   geom_text_repel(data=CBC30N, aes(x=Meters_90, y=Meter, label=NewTagNum), max.overlaps = 100, color="black", size = 4, hjust=-0.50,
#                   nudge_x = 0.2,
#                   box.padding = 0.4, point.padding = 0.5)+
#   scale_y_continuous("Transect Length (m)", breaks = seq(0, 42, by = 1)) +
#   scale_x_continuous("Meters Perpendicular", breaks = seq(-12, 7, by = 1)) +
#   scale_size_continuous(range = c(2,6.5), name = "", guide = 'none') +
#   scale_shape_manual(values = c(21,24)) +
#   labs(title = "CBC CBC30N", shape = "Check if dead") +
#   theme(plot.title = element_text(size = 12,hjust = 0.5),
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"),
#         axis.text = element_text(colour = "black", hjust = 1, size = 12),
#         axis.title = element_text(size = 12)) +
#   theme(legend.key.size = unit(1.5, "line"))
# dev.off()



############ CBC Lagoon ##########

# change coordinates of coral 18 so it can be seen in the map 
# coral_subset <- coral_subset %>%
#   mutate(Meters_90 = if_else(Species == "PAST" & Transect == "LAGOON" & NewTagNum == "18",
#                              2.2,
#                              Meters_90))
# coral_subset <- coral_subset %>%
#   mutate(Meters_90 = if_else(Species == "SSID" & Transect == "LAGOON" & NewTagNum == "19",
#                              2.6,
#                              Meters_90)) %>%
#   mutate(Meter = if_else(Species == "SSID" & Transect == "LAGOON" & NewTagNum == "19",
#                              15,
#                              Meter)) %>%
#   mutate(Meters_90 = if_else(Species == "MCAV" & Transect == "LAGOON" & NewTagNum == "12",
#                        2.7,
#                        Meters_90)) %>%
#   mutate(Meters_90 = if_else(Species == "MCAV" & Transect == "LAGOON" & NewTagNum == "14",
#                            -1,
#                            Meters_90))

lagoon_all <- coral_subset %>% subset(Transect == "LAGOON")
# %>%
#   subset(NewTagNum != "12flag") #%>%
  # subset(NewTagNum != "17") # exactly overlaps with an alive coral

  #14 and 15 MCAV overlap - fixed
# confirm it includes newly tagged colonies
length(unique(lagoon_all$NewTagNum))
unique(lagoon_all$Date_InitialTag)

# remove dead 
Lagoon<-lagoon_all %>%
  filter(Condition != "Dead")

length(unique(Lagoon$NewTagNum))
unique(Lagoon$Date_InitialTag)

tiff("CBCLagoon_color.tif",width = 6, height = 8, units = "in", res = 300)
ggplot() +
  geom_point(data=Lagoon,aes(x = Meters_90, y = Meter, fill = Species, size = MaxDiameter, shape = check), 
             color = "black", alpha = 0.7) +
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
  labs(title = "CBC Lagoon", shape = "Check if dead") +
  theme(plot.title = element_text(size = 12,hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", hjust = 1, size = 12),
        axis.title = element_text(size = 12)) +
  theme(legend.key.size = unit(1.5, "line"))
dev.off()



########## SR30N ##########
SR30N_all <- coral_subset %>% subset(Transect == "SR30N") 
# %>%
#   mutate(NewTagNum = as.numeric(NewTagNum)) %>%
#   subset(NewTagNum < 300) %>%
#   subset(NewTagNum != "51")

# confirm it includes newly tagged colonies
length(unique(SR30N_all$NewTagNum))
unique(SR30N_all$Date_InitialTag)

# remove dead 
SR30N<-SR30N_all %>%
  filter(Condition != "Dead")

length(unique(SR30N$NewTagNum))
unique(SR30N$Date_InitialTag)

tiff("SR30N_color.tif",width = 6, height = 8, units = "in", res = 300)
ggplot() +
  geom_point(data=SR30N,aes(x = Meters_90, y = Meter, fill = Species, size = MaxDiameter, shape = check), 
             color = "black", alpha = 0.7) +
  scale_fill_manual(values=c(speccolors), guide = guide_legend(override.aes = list(pch = 21, size = 5))) +
  geom_point(data=SR30N, aes(x = Meters_90, y = Meter, alpha = Condition == 'Dead'),
             pch = 4, color = "snow", stroke = 0.5) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0), guide = 'none')+
  geom_vline(xintercept = 0, lty = 2, lwd = 0.25) +
  geom_text_repel(data=SR30N, aes(x=Meters_90, y=Meter, label=NewTagNum), max.overlaps = 100, color="black", size = 4, hjust=-0.25,
                  nudge_x = 0.1,
                  box.padding = 0.4, point.padding = 0.5)+
  scale_y_continuous("Transect Length (m)", breaks = seq(0, 42, by = 1)) +
  scale_x_continuous("Meters Perpendicular", breaks = seq(-12, 7, by = 1)) +
  scale_size_continuous(range = c(2,6.5), name = "", guide = 'none') +
  scale_shape_manual(values = c(21,24)) +
  labs(title = "CBC SR30N", shape = "Check if dead") +
  theme(plot.title = element_text(size = 12,hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", hjust = 1, size = 12),
        axis.title = element_text(size = 12)) +
  theme(legend.key.size = unit(1.5, "line"))
dev.off()



########## Curlew #############
Curlew_all <- coral_subset %>% subset(Transect == "CURLEW")

# confirm it includes newly tagged colonies
length(unique(Curlew_all$NewTagNum))
unique(Curlew_all$Date_InitialTag)

# remove dead 
Curlew<-Curlew_all %>%
  filter(Condition != "Dead")

length(unique(Curlew$NewTagNum))
unique(Curlew$Date_InitialTag)

tiff("Curlew_color.tif",width = 5, height = 8, units = "in", res = 300)
ggplot() +
  geom_point(data=Curlew,aes(x = Meters_90, y = Meter, fill = Species, size = MaxDiameter), 
             color = "black", alpha = 0.7, shape = 21) +
  scale_fill_manual(values=c(speccolors), guide = guide_legend(override.aes = list(pch = 21, size = 5))) +
  geom_point(data=Curlew, aes(x = Meters_90, y = Meter, alpha = Condition == 'Dead'),
             pch = 4, color = "snow", stroke = 0.5) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0), guide = 'none')+
  geom_vline(xintercept = 0, lty = 2, lwd = 0.25) +
  geom_text_repel(data=Curlew, aes(x=Meters_90, y=Meter, label=NewTagNum), max.overlaps = 100, color="black", size = 4,
                  nudge_x = 0.1,
                  box.padding = 0.4, point.padding = 0.5)+
  scale_y_continuous("Transect Length (m)", breaks = seq(0, 42, by = 1)) +
  scale_x_continuous("Meters Perpendicular", breaks = seq(-12, 7, by = 1)) +
  scale_size_continuous(range = c(2,6.5), name = "", guide = 'none') +
  scale_shape_manual(values = c(21,24)) +
  labs(title = "Curlew", shape = "Check if dead") +
  theme(plot.title = element_text(size = 12,hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", hjust = 1, size = 12),
        axis.title = element_text(size = 12)) +
  theme(legend.key.size = unit(1.5, "line"))
dev.off()


############## BB #############
BB_all <- coral_subset %>% subset(Transect == "BB")

# confirm it includes newly tagged colonies
length(unique(BB_all$NewTagNum))
unique(BB_all$Date_InitialTag)

# remove dead 
BB<-BB_all %>%
  filter(Condition != "Dead")

length(unique(BB$NewTagNum))
unique(BB$Date_InitialTag)

tiff("BB_color.tif",width = 5, height = 8, units = "in", res = 300)
ggplot() +
  geom_point(data=BB,aes(x = Meters_90, y = Meter, fill = Species, size = MaxDiameter), 
             color = "black", alpha = 0.7, , shape = 21) +
  scale_fill_manual(values=c(speccolors), guide = guide_legend(override.aes = list(pch = 21, size = 5))) +
  geom_point(data=BB, aes(x = Meters_90, y = Meter, alpha = Condition == 'Dead'),
             pch = 4, color = "snow", stroke = 0.5) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0), guide = 'none')+
  geom_vline(xintercept = 0, lty = 2, lwd = 0.25) +
  geom_text_repel(data=filter(BB, NewTagNum != "25"), aes(x=Meters_90, y=Meter, label=NewTagNum), max.overlaps = 100, color="black", size = 4, 
                  # hjust=-0.25,
                  nudge_x = 0.1,
                  box.padding = 0.4, point.padding = 0.4)+
 # custom positioning for the annoying ones
  geom_text_repel(
    data = filter(BB, NewTagNum %in% "25"),
    aes(x = Meters_90, y = Meter, label = NewTagNum),
    nudge_x = -0.05, # Nudge left instead of right
    nudge_y = -0.1,  # Shift down
    # box.padding = 0.5, point.padding = 0.6
  )+
  scale_y_continuous("Transect Length (m)", breaks = seq(0, 42, by = 1)) +
  scale_x_continuous("Meters Perpendicular", breaks = seq(-12, 7, by = 1)) +
  scale_size_continuous(range = c(2,6.5), name = "", guide = 'none') +
  scale_shape_manual(values = c(21,24)) +
  labs(title = "BB", shape = "Check if dead") +
  theme(plot.title = element_text(size = 12,hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", hjust = 1, size = 12),
        axis.title = element_text(size = 12)) +
  theme(legend.key.size = unit(1.5, "line"))
dev.off()

########## Hangman ########## 
HANGMAN_all <- coral_subset %>% subset(Transect == "HANGMAN")

# confirm it includes newly tagged colonies
length(unique(HANGMAN_all$NewTagNum))
unique(HANGMAN_all$Date_InitialTag)

# remove dead 
HANGMAN<-HANGMAN_all %>%
  filter(Condition != "Dead")

length(unique(HANGMAN$NewTagNum))
unique(HANGMAN$Date_InitialTag)

tiff("HANGMAN_color2.tif",width = 5, height = 8, units = "in", res = 300)
ggplot() +
  geom_point(data=HANGMAN,aes(x = Meters_90, y = Meter, fill = Species, size = MaxDiameter), 
             color = "black", alpha = 0.7, shape = 21) +
  scale_fill_manual(values=c(speccolors), guide = guide_legend(override.aes = list(pch = 21, size = 5))) +
  geom_point(data=HANGMAN, aes(x = Meters_90, y = Meter, alpha = Condition == 'Dead'),
             pch = 4, color = "snow", stroke = 0.5) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0), guide = 'none')+
  geom_vline(xintercept = 0, lty = 2, lwd = 0.25) +
  geom_text_repel(data=HANGMAN, aes(x=Meters_90, y=Meter, label=NewTagNum), max.overlaps = 100, color="black", size = 4, 
                  # hjust=-0.25,
                  nudge_x = 0.1,
                  box.padding = 0.4, point.padding = 0.6,
                  min.segment.length = 0.2
                  )+
  # custom labels for 11, 12, 35
  # geom_text_repel(data=filter(HANGMAN, NewTagNum == '11'), aes(x=Meters_90, y=Meter, label=NewTagNum), max.overlaps = 100, color="black", size = 4, 
  #                 hjust=-0.25, nudge_x=-0.5,
  #                 nudge_y = 0.5,
  #                 box.padding = 0.4, point.padding = 0.6,
  #                 min.segment.length = 0
  # )+
  # geom_text_repel(data=filter(HANGMAN, NewTagNum == '23'), aes(x=Meters_90, y=Meter, label=NewTagNum), max.overlaps = 100, color="black", size = 4,
  #                 nudge_x=-0.5,
  #                 nudge_y = 0.5,
  #                 box.padding = 0.4, point.padding = 0.6,
  #                 min.segment.length = 0
  # )+
  scale_y_continuous("Transect Length (m)", breaks = seq(0, 42, by = 1)) +
  scale_x_continuous("Meters Perpendicular", breaks = seq(-12, 7, by = 1)) +
  scale_size_continuous(range = c(2,6.5), name = "", guide = 'none') +
  scale_shape_manual(values = c(21,24)) +
  labs(title = "HANGMAN", shape = "Check if dead") +
  theme(plot.title = element_text(size = 12,hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", hjust = 1, size = 12),
        axis.title = element_text(size = 12)) +
  theme(legend.key.size = unit(1.5, "line"))
dev.off()


