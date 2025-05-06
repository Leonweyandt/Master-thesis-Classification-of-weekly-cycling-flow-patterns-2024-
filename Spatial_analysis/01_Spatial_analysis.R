###############################################################################
### Spatial analysis of the cluster groups of the bicycle counting stations ###
###############################################################################

# Install required packages
install.packages("readxl")

library(openxlsx)
library(readxl)

### loading data ###

# path to excel sheet with the data from QGIS with information to counting station, cluster group and spatial information
excel_file_path <- "C:/Leon/Studium/Master/Masterarbeit/Rscripte_Masterarbeit_Leon_Weyandt clean/Spatial_analysis/Exceldatei/20240218_Analyse.xlsx"
Analysedatei <- read_excel(excel_file_path) # load excel page in a data frame

# load excel file with information of Regiostar
Regiostarinformationen <- read_excel("C:/Leon/Studium/Master/Masterarbeit/Rscripte_Masterarbeit_Leon_Weyandt clean/Spatial_analysis/Exceldatei/RegioStaR-Referenzdateien.xlsx", sheet = "Codeplan")

# path of the folder to save figures
Abbildungen <- "C:/Leon/Studium/Master/Masterarbeit/Rscripte_Masterarbeit_Leon_Weyandt clean/Spatial_analysis/Abbildungen/"

### Preparations ###

# create column with net category
Analysedatei$Netzkategorie <- ifelse(Analysedatei$`auf_Hauptnetz?` == "ja" & Analysedatei$`Radfernweg_ja/nein` == "ja", "HRFW und RNI",
                                     ifelse(Analysedatei$`auf_Hauptnetz?` == "ja" & Analysedatei$`Radfernweg_ja/nein` == "nein", "RNI",
                                            ifelse(Analysedatei$`auf_Hauptnetz?` == "nein" & Analysedatei$`Radfernweg_ja/nein` == "ja", "HRFW", "nicht klassifiziert")))

Analysedatei <- subset(Analysedatei, Cluster != 0) # remove counting stations without distribution to a cluster group

# define colors for the plots  
farben <-  c(  "#00CC00"  , "blue",  "red", "#FF9900", "#999999",   
  "#ff99ff", "#33ffFF",  "#ffcc99"  , "purple", "#006600"   )


#######################
#####  Plotting  ######
#######################

## RegioStaRGem5 ~ cluster group ##
library(dplyr)

# Filtering the data
RegiostarGem5 <- subset(Analysedatei,Entfernung_Ortslage == 0 & Silhouettenbreite > 0.1) # just counting stations in urban area and with silhouette width over 0.1
RegiostarGem5 <- merge(RegiostarGem5, Regiostarinformationen, by.x = "In_RegioStaRGem5", by.y = "Werte") # add RegioStaR-informationsn
RegiostarGem5 <- subset(RegiostarGem5, Variable == "RegiostarGem5") 

# set order of data for the figure
RegiostarGem5$In_RegioStaRGem5 <- factor(RegiostarGem5$In_RegioStaRGem5, levels = c(51,52,53,54,55))
RegiostarGem5 <- RegiostarGem5[order(RegiostarGem5$In_RegioStaRGem5), ]

# create field with description ("Beschreibung") of the different RegioStaR5Gem-areas
RegiostarGem5 <- RegiostarGem5 %>%
  mutate(Beschreibung = factor(Beschreibung, levels = unique(Beschreibung)))

# calculate amount ("Anzahl") 
RegiostarGem5 <- RegiostarGem5 %>%   
  group_by(In_RegioStaRGem5, Beschreibung) %>%
  mutate(Anzahl = n())

# calculate proportion ("Anteil") and amount ("Anzahl")
RegiostarGem5 <- RegiostarGem5 %>% 
  group_by(In_RegioStaRGem5, Beschreibung,Cluster) %>%
  reframe(Anteil = min(n()/Anzahl),
          Anzahl = min(Anzahl))

# Set names of x-axis-labels and order of data
nn_values <- c("Metropole\n\nn = 15","Metropole\n\nn = 15",
               "Regiopole,\nGroßstadt\nn = 46",
               "Regiopole,\nGroßstadt\nn = 46",
                "zentrale Stadt,\nMittelstadt\nn = 47",
               "zentrale Stadt,\nMittelstadt\nn = 47",
               "städtischer\nRaum\nn = 42",
               "städtischer\nRaum\nn = 42",
               "städtischer\nRaum\nn = 42",
               "kleinstädtischer,\ndörflicher Raum\nn = 4",
               "kleinstädtischer,\ndörflicher Raum\nn = 4")  

RegiostarGem5$Beschreibung <- paste0(nn_values)

RegiostarGem5$Beschreibung <- factor(RegiostarGem5$Beschreibung, levels = c("Metropole\n\nn = 15",
                                                                      "Regiopole,\nGroßstadt\nn = 46",
                                                                      "zentrale Stadt,\nMittelstadt\nn = 47",
                                                                      "städtischer\nRaum\nn = 42",
                                                                      "kleinstädtischer,\ndörflicher Raum\nn = 4"))
# Create plot
library(scales)
library(ggplot2)
RSG5 <- ggplot(RegiostarGem5, aes(x = factor(Beschreibung), y = Anteil, fill = factor(Cluster))) +
   geom_bar(stat = "identity", width = 0.7) + 
   theme_classic() +
   theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
         axis.title = element_text(size =13, face= "plain"),
         axis.text.y  = element_text(size= 11, face= "plain"),
         legend.position = c(1.145, 1), legend.justification = c(1, 1),
         plot.margin = margin(t = 0, r = 2.5, b = 0, l = 0, unit = "cm"), 
         axis.text.x =element_text(size=13, face= "plain",lineheight = 1.1), 
         title = element_text(size =13, face= "bold"),
         legend.text = element_text(size = 13, face= "plain"))+
         labs(title = "Lage in Regionen von RegioStaRGem5", x = "", y = "Anteil", ) +
         scale_fill_manual(name = "Cluster", values = farben[c(1,2,3)])+
         coord_cartesian(ylim = c(0, 1))

 ggsave(RSG5,width = 7.8,  filename = file.path(Abbildungen,"RegioStaRGem5.png"), height = 3.5, dpi = 300)

 rm(nn_values, RegiostarGem5)
 
## Distance to secondary schools in urban area  ##

library(dplyr)
distanz_ranges <- c(-1,  1000,  2000, 100000) # set range of the distance of the counting stations to secondary schools for plot

Schuldistanzi <- subset(Analysedatei, Entfernung_Ortslage <= 0 & Silhouettenbreite >0.1) # just counting stations in urban area and with silhouette width over 0.1

Schuldistanzi$Distanzbereich <- cut(Schuldistanzi$Distanz_weiterführende_Schulen,
                        distanz_ranges, labels = c("0 bis 1000 m\nn = 102",
            "1000 bis 2000 m\nn = 41","über 2000 m\nn = 11"), include.lowest = TRUE)

# Calculate amount and proportion of the distances
Schuldistanzi <- Schuldistanzi %>%  
  group_by(Distanzbereich) %>%
  mutate(Anzahl = n())

Schuldistanzi <- Schuldistanzi %>% # find out number of cluster in set distances
  group_by(Cluster, Distanzbereich) %>%
  reframe(Anteil = min(n()/Anzahl),
          Anzahl = min(Anzahl))

# Create plot of cluster in different distances to secondary schools

library(scales)
library(ggplot2)

RSG5<- ggplot(Schuldistanzi, aes(x = factor(Distanzbereich), y = Anteil, fill = factor(Cluster))) +
  geom_bar(stat = "identity", width = 0.6) + 
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        axis.title = element_text(size =13, face= "plain"),
        axis.text.y = element_text(size = 11, face = "plain"),
        axis.text.x =element_text(size=13, face= "plain",lineheight=1.25), 
        title = element_text(size =13, face= "bold"),
        legend.position = c(1.2, 1), legend.justification = c(1, 1),
        plot.margin = margin(t = 0, r = 2.5, b = 0, l = 0, unit = "cm"),  
        legend.text = element_text(size = 13, face= "plain"))+
        labs(title = "Entfernung zu weiterführenden Schulen", x = "", y = "Anteil", ) +
        scale_fill_manual(name = "Cluster", values = farben[c(1,2,3)])+
        coord_cartesian(ylim = c(0, 1))

# Save plot
ggsave(RSG5,width = 6,  filename = file.path(Abbildungen,"weiterführendeSchule_innerorts.png"), height = 3.5, dpi = 300)

rm(distanz_ranges, Schuldistanzi)

## Distance to rivers ##

distanz_ranges <- c(-1, 200,  600000) # set distance range to rivers for plot

Flussdistanz <- subset(Analysedatei, Entfernung_Ortslage >= 0 & Silhouettenbreite > 0.1) # just counting stations in urban area and with silhouette width over 0.1

# add column with distance range to river
Flussdistanz$Distanzbereich <- cut(Flussdistanz$Distanz_naechster_Fluss,
                                   distanz_ranges, labels = c("< 200 m",
                                  "> 200 m"), include.lowest = TRUE)

Flussdistanz <- Flussdistanz %>% # calculate amount of counting stations in the distance ranges
  group_by(Distanzbereich) %>%
  mutate(Anzahl = n())

Flussdistanz <- Flussdistanz %>%  # calculate amount and proportion of counting station in the distance ranges
  group_by(Distanzbereich, Cluster) %>%
  reframe(Anteil = min(n()/Anzahl),
          Anzahl = min(Anzahl))

# Field for labeling of x-axis
Flussdistanz <- Flussdistanz %>%
  mutate(Combined = paste(Distanzbereich,"n =", sep = "\n"))
Flussdistanz <- Flussdistanz %>%
  mutate(Combined = paste(Combined, Anzahl, sep = ""))

# Create plot of cluster nearby rivers

library(scales)
library(ggplot2)

RSG5 <- ggplot(Flussdistanz, aes(x = Combined, y = Anteil, fill = factor(Cluster))) +
  geom_bar(stat = "identity", width = 0.5) + 
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        axis.title = element_text(size =13, face= "plain"),
        axis.text.y  = element_text(size= 11, face= "plain"),
        axis.text.x =element_text(size=13, face= "plain", lineheight = 1.25), 
        title = element_text(size =13, face= "bold"),
        legend.position = c(1.2, 1), legend.justification = c(1, 1),
        plot.margin = margin(t = 0, r = 2.5, b = 0, l = 0, unit = "cm"),  
        legend.text = element_text(size = 13, face= "plain"))+
  labs(title = "Entfernung zu Fliessgewässern", x = "", y = "Anteil", ) +
  scale_fill_manual(name = "Cluster", values = farben[c(1,2,3)])+
  coord_cartesian(ylim = c(0, 1))+
  scale_x_discrete(labels = label_wrap(8))  

# Save plot
ggsave(RSG5,width = 6,  filename = file.path(Abbildungen,"Flussdistanz.png"), height = 3.5, dpi = 300)

rm(Flussdistanz, distanz_ranges)

# street type percentage (not used in master thesis)  ##

# Produce data set and filter it
Strassentyp <- Analysedatei
Strassentyp <- subset(Strassentyp, Entfernung_Ortslage > 0)

library(dplyr)
Strassentyp <- Strassentyp %>%
  group_by(Straßentyp) %>%
  mutate(Anzahl = n())

Strassentyp <- Strassentyp %>%
  group_by(Straßentyp, Cluster) %>%
  reframe(Anteil =  min(n()/ Anzahl))

# Create plot
library(ggplot2)
RSG5 <- ggplot(Strassentyp, aes(x = factor(Straßentyp), y = Anteil, fill = factor(Cluster))) +
  geom_bar(stat = "identity", width = 0.9) + 
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        axis.title = element_text(size =12, face= "plain"),
        axis.text.y  = element_text(size= 10, face= "plain"),
        axis.text.x =element_text(size=12, face= "plain"), 
        title = element_text(size =12, face= "bold"),
        legend.text = element_text(size = 12, face= "plain"))+
  labs(title = "Lage an verschiedenen Straßentypen", x = "", y = "Anteil", ) +
  scale_fill_manual(name = "Cluster", values = farben[c(1,2,3)])+
  coord_cartesian(ylim = c(0, 1))+
 scale_x_discrete(labels = label_wrap(14))  

# Save plot
ggsave(RSG5,width = 8,  filename = file.path(Abbildungen,"Straßentyp.png"), height = 4, dpi = 300)

rm(Strassentyp) # remove not needed data

################################################################
# Führungsform in percentage (not used in master thesis) ##
################################################################

# Produce data set and filter it
Verkehr <- Analysedatei
Verkehr <- subset(Verkehr, Entfernung_Ortslage > 0) # just counting stations in urban area

# Changing the names of the fields
Verkehr = mutate(Verkehr, Verkehr = replace(Verkehr, Verkehr %in% 
                c("Gemischter Wirtschaftsverkehr (Wirtschaftswege, Forstwege, …)", 
                  "hier werden Mehrzweckstreifen detektiert",
                  "Radfahrstreifen, Norden. Busspur/Rad frei, Süden", 
                  "Separierter Verkehr (insbes. bei unselbstständigen Radwegen/Wegen)",
                  "separierter Verkehr (insbes. bei unselbstständigen Radwegen/Wegen)",
        "„Echter Mischverkehr“ (Gemeindestraße, Landesstraße … mit regelmäßigem gemischten Verkehr)", 
                  "Fahrradstraße",
                  "Radfahrstreifen"),
                c("Gemischter Wirtschafts-\nverkehr", 
                  "Mehrzweck-\nstreifen",
                  "Radfahr-\nstreifen", 
                  "Separierter Verkehr", 
                  "Separierter Verkehr",
                  "Echter Mischverkehr", 
                  "Fahrrad-\nstraße", 
                  "Radfahr-\nstreifen")))

library(dplyr)  
Verkehr <- Verkehr %>%
  group_by(Verkehr) %>%
  mutate(Anzahl = n())

Verkehr <- Verkehr %>%  
  group_by(Verkehr, Cluster) %>%
  reframe(Anteil =  min(n()/Anzahl))

# Create plot
library(ggplot2)
library(scales)
RSG5<-ggplot(Verkehr, aes(x = Verkehr, y = Anteil, fill = factor(Cluster))) +
    geom_bar(stat = "identity", width = 0.9) + 
    theme_classic() +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          axis.title = element_text(size =12, face= "plain"),
          axis.text.y  = element_text(size= 10, face= "plain"),
          axis.text.x = element_text(size = 12, face = "plain", angle = 0, vjust = 0.65 ),
          title = element_text(size =12, face= "bold"),
          legend.text = element_text(size = 12, face= "plain"))+
    labs(title = "Vergleich verschiedener Führungsformen", x = "", y = "Anteil", ) +
    scale_fill_manual(name = "Cluster", values = farben[c(1,2,3)])+
    coord_cartesian(ylim = c(0, 1))+
    scale_x_discrete(labels = label_wrap(12))  
  
# Save plot
  ggsave(RSG5,width = 8,  filename = file.path(Abbildungen,"Verkehrsführung.png"), height = 4, dpi = 300)

  rm(Verkehr)
  
####################################################
### Population density in surrounding 1km-square ###
####################################################

distanz_ranges <- c(-1, 250, 500, 2000, 4000,  600000) # set range for population density

Einwohner <- subset(Analysedatei, Silhouettenbreite > 0.1) # remove data with silhouette width over 0.1

# Set labels
Einwohner$Einwohneranzahl <- cut(Einwohner$Einwohner_min,
              distanz_ranges, labels = c("0 - 250\nn = 198", "250 - 500\nn = 44",
                                         "500 - 2000\nn = 94", "2000 - 4000\nn = 62",
                    "über 4000\nn = 43"), include.lowest = TRUE)

Einwohner <- Einwohner %>%  
  group_by(Einwohneranzahl) %>%
  mutate(Anzahl = n())

Einwohner <- Einwohner %>%  
  group_by(Einwohneranzahl, Cluster) %>%
  reframe(Anteil = min(n()/Anzahl),
          Anzahl = min(Anzahl),
          Anzahln = n())

# Create plot
RSG5 <-  ggplot(Einwohner, aes(x = factor(Einwohneranzahl), y = Anteil, fill = factor(Cluster))) +
    geom_bar(stat = "identity", width = 0.7) + 
    theme_classic() +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          axis.title = element_text(size =13, face= "plain"),
          axis.text.y  = element_text(size= 11, face= "plain"),
          axis.text.x =element_text(size=13, face= "plain",lineheight=1.1), 
          title = element_text(size =13, face= "bold"),
          legend.position = c(1.15, 1), legend.justification = c(1, 1),
          plot.margin = margin(t = 0, r = 2.5, b = 0, l = 0, unit = "cm"),  # Platz für die Legende außerhalb des Plots
          legend.text = element_text(size = 13, face= "plain"))+
    
    labs(title = "Bevölkerungsdichte im umgebenden 1km-Raster", x = "Einwohner*innen pro km²", y = "Anteil", ) +
    scale_fill_manual(name = "Cluster", values = farben[c(1,2,3)])+
    coord_cartesian(ylim = c(0, 1))

# Save plot
  ggsave(RSG5,width = 7.2,  filename = file.path(Abbildungen,"Einwohnerzahlen.png"), height = 3.5, dpi = 300)

  rm(distanz_ranges, Einwohner) # remove not needed data
  
##########################    
# Distance to urban area #
##########################

distanz_ranges <- c(-1, 0, 1000, 600000) # set ranges for distance to urban area

Ortslage <- subset(Analysedatei, Silhouettenbreite > 0.1) # remove data with silhouette width over 0.1

# Set labels
Ortslage$Entfernung_Ortslage <- cut(Ortslage$Entfernung_Ortslage, distanz_ranges,
          labels = c("innerörtlich\n = 154", "< 1000 m\nn = 262",
                     "> 1000 m\nn = 25"), include.lowest = TRUE)

Ortslage <- Ortslage %>%    
  group_by(Entfernung_Ortslage) %>%
  mutate(Anzahl = n())

Ortslage <- Ortslage %>%    
  group_by(Entfernung_Ortslage, Cluster) %>%
  reframe(Anteil = min(n()/Anzahl),
          Anzahl= min(Anzahl))

# Create plot
library(ggplot2)
RSG5 <- ggplot(Ortslage, aes(x = factor(Entfernung_Ortslage), y = Anteil  , fill = factor(Cluster))) +
  geom_bar(stat = "identity", width = 0.5) + 
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        axis.title = element_text(size =13, face= "plain"),
        axis.text.y  = element_text(size= 11, face= "plain"),
        axis.text.x =element_text(size=13, face= "plain", lineheight =1.25), 
        title = element_text(size =13, face= "bold"),
        legend.position = c(1.2, 1), legend.justification = c(1, 1),
        plot.margin = margin(t = 0, r = 2.5, b = 0, l = 0, unit = "cm"),  
        legend.text = element_text(size = 13, face= "plain"))+
        labs(title = "Inner- oder außerörtliche Lage", x = "", y = "Anteil", ) +
        scale_fill_manual(name = "Cluster", values = farben[c(1,2,3)])+
        coord_cartesian(ylim = c(0, 1))

# Save plot
 ggsave(RSG5,width = 6,  filename = file.path(Abbildungen,"Ortslage.png"), height = 3.5, dpi = 300)

rm(Ortslage, distanz_ranges)

 #######################################################################
 ## Radwegeklasse - Cluster in Prozent für HRFW UND RNI ################
 #######################################################################
 
# Datensatz erstellen und filtern
 Analysedatei <- subset(Analysedatei, Cluster != 0) # nicht zugewiesene DZS mit Cluster 0 entfernen
 RNI_HRFW <- subset(Analysedatei, Silhouettenbreite > 0.1) # remove data with silhouette width over 0.1
 
 # Feld für Radwegeklasse erstellen
 RNI_HRFW$Weg <- ifelse(RNI_HRFW$`auf_Hauptnetz?` == "ja" & RNI_HRFW$`Radfernweg_ja/nein` == "ja", "HRFW und RNI\nn = 71",
                        ifelse(RNI_HRFW$`auf_Hauptnetz?` == "ja" & RNI_HRFW$`Radfernweg_ja/nein` == "nein", "RNI\nn = 106",
                               ifelse(RNI_HRFW$`auf_Hauptnetz?` == "nein" & RNI_HRFW$`Radfernweg_ja/nein` == "ja",
                                      "HRFW\nn = 33", "nicht klassifiziert\nn = 77")))

  RNI_HRFW <- subset(RNI_HRFW, Entfernung_Ortslage > 0) # just analyze counting stations which are not in urban area
  
 library(dplyr)
 RNI_HRFW <- RNI_HRFW %>%  
   group_by(Weg) %>%
   mutate(Anzahl = n())
 
 RNI_HRFW <- RNI_HRFW %>%   
   group_by(Weg, Cluster) %>%
   reframe(Anteil =  min(n()/ Anzahl),
           Anzahl = min(Anzahl))
 
 # Set labels
 RNI_HRFW$Weg <- factor(RNI_HRFW$Weg, levels = c("HRFW\nn = 33", "HRFW und RNI\nn = 71", "RNI\nn = 106",
                                                 "nicht klassifiziert\nn = 77"))
 
 # Create plot 
 library(ggplot2)
 RSG5<- ggplot(RNI_HRFW, aes(x = Weg, y = Anteil, fill = factor(Cluster))) +
   geom_bar(stat = "identity", width = 0.7) + 
   theme_classic() +
   theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
         axis.title = element_text(size =13, face= "plain"),
         axis.text.y  = element_text(size= 11, face= "plain"),
         axis.text.x =element_text(size=13, face= "plain",lineheight = 1.25), 
         title = element_text(size =13, face= "bold"),
         legend.position = c(1.175, 1), legend.justification = c(1, 1),
         plot.margin = margin(t = 0, r = 2.5, b = 0, l = 0, unit = "cm"),  
         legend.text = element_text(size = 13, face= "plain"))+
         labs(title = "Netzkategorie an der Zählstelle", x = "", y = "Anteil", ) +
         scale_fill_manual(name = "Cluster", values = farben[c(1,2,3)])+
         coord_cartesian(ylim = c(0, 1))#+

 # Save plot
 ggsave(RSG5,width = 6.5,  filename = file.path(Abbildungen,"RNI_HRFW.png"), height = 3.5, dpi = 300)

 rm(RNI_HRFW) # remove not needed data
