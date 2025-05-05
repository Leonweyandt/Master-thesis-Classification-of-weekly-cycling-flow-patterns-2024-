##########################################################
#### Validation of the result of the cluster analysis ####
##########################################################

## Determine rand indices for all types of cluster analyses ##

library(fossil)
rand.index(Clustertabelle_kmeans$kmeans, Analysetabelle$weightedaverage)
rand.index(Clustertabelle_kmeans$kmeans, Analysetabelle$average)
rand.index(Clustertabelle_kmeans$kmeans, Analysetabelle$Ward.D2)
rand.index(Clustertabelle_kmeans$kmeans, Analysetabelle$centroid)
rand.index(Analysetabelle$weightedaverage, Analysetabelle$average)
rand.index(Analysetabelle$weightedaverage, Analysetabelle$Ward.D2)
rand.index(Analysetabelle$weightedaverage, Analysetabelle$centroid)
rand.index(Analysetabelle$average, Analysetabelle$Ward.D2)
rand.index(Analysetabelle$average, Analysetabelle$centroid)
rand.index(Analysetabelle$Ward.D2, Analysetabelle$centroid)

### Determine statistical values  ###
library(dplyr)
library(tidyr)
Stattabelle   <- Clustertabelle_kmeans # Copy data set

# Rotate table and create columns for the days of the week ("Wochentag") and the count data ("Zaehlung")
Stattabelle <- Stattabelle %>%
  pivot_longer(cols = 3:9, names_to = "Wochentag", values_to = "Zaehlung") 

Stattabelle$Wochentag <- factor(Stattabelle$Wochentag, levels = c("Montag", "Dienstag", "Mittwoch", "Donnerstag", 
                                                                  "Freitag", "Samstag", "Sonntag"), ordered = TRUE)

Stattabelle$Wochentag_num <- as.numeric(Stattabelle$Wochentag) # set data type of day of the week ("Wochentag") to numeric

# Calculate statistical values of the cluster
Stat <- Stattabelle %>%
  group_by(kmeans, Wochentag, Wochentag_num) %>%
  reframe(Min = round(min(Zaehlung), digits = 4),
          Max = round(max(Zaehlung), digits = 4),
          Mean   = round(mean(Zaehlung), digits = 4),
          SD = round(sd(Zaehlung), digits = 4),
          Anzahl = n())

rm(Stattabelle) # Remove not needed table

############################
## Create silhouette plot ##
############################

library(dplyr)

# Preparation of the silhouette scores for the plot
Bewertung <- Clustertabelle_kmeans
library(dplyr)
Bewertung <- Bewertung %>%
  arrange(desc(Silhouette))
Bewertung <- Bewertung %>%
  group_by(kmeans) %>%
  mutate(Reihennummer = row_number()) %>%
  ungroup()

# Set path to save the plot 
file_path <-  "C:/Leon/Studium/Master/Masterarbeit/Rscripte_Masterarbeit_Leon_Weyandt clean/Clustering/figures"

# Create silhouette plot 
facet_specific_text <- data.frame(kmeans = 1, x = 0, y = 0.2, size =20, label = "Average \nsilhouette width = 0,47")
library(ggplot2)
p <- ggplot(data = Bewertung) +
  geom_bar(data = subset(Bewertung, kmeans == 1), aes(x = Reihennummer, y = Silhouette), 
           stat = "identity", fill = "green", position = "dodge") +
  geom_bar(data = subset(Bewertung, kmeans == 2), aes(x = Reihennummer, y = Silhouette),
           stat = "identity", fill = "blue", position = "dodge") +
  geom_bar(data = subset(Bewertung, kmeans == 3), aes(x = Reihennummer, y = Silhouette),
           stat = "identity", fill = "red", position = "dodge") +
  theme_classic() +
  coord_cartesian(ylim = c(0, 1))+
  theme(axis.text.x = element_blank(),  # Remove x-axis labels
        plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
        strip.background = element_blank(),  # Remove box around facet labels
        panel.border = element_blank(),  # Remove borders around facets
        panel.spacing = unit(0, "lines"),
        axis.title.y =element_text(size=13, face = "plain"),
        axis.text.y = element_text(size=11),
        strip.text = element_text(size = 13),  # Adjust strip text (facet labels) font size
        axis.ticks.x = element_blank(),# Remove spacing between facets
        axis.line.x = element_blank()) +  # Remove x-axis line
  labs(title = "Silhouette widths for k-means clustering with 3 cluster groups", x = "", y="Silhouette width") +  
  geom_hline(yintercept = 0.47, color = "black") +
  geom_text(data = facet_specific_text, aes(x = x, y = y, hjust= -0.025,label = label), color = "black", size = 4.5, vjust = -1)+
  facet_wrap(~kmeans, scales = "free_x", ncol = 3,  strip.position = "bottom", 
             labeller = as_labeller(c(`1` = "Cluster 1 \n0,46", `2` = "Cluster 2 \n0,42", `3` = "Cluster 3\n0,54")))

# Save plot
ggsave(p,width = 7.2,  filename = file.path(file_path,"silhoeutte_widths.png"), height = 5, dpi = 300)

rm(Bewertung, facet_specific_text, p) # remove not needed data

