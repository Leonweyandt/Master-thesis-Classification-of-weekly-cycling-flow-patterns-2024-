###########################################################################################
############################# Test stability of results ###################################
###########################################################################################

#### Load data ####

# Remove counting stations which are not included in cluster analysis 
Wochenganglinie <- subset(Alle_summiert, !Kurzname %in% c("DZS_071b","140", "140 b", 
                                                          "140a","140 a", "140b", "514a",
                                                          "514b","514 a", "514 b"))
Wochenganglinie <- subset(Wochenganglinie, !is.na(Zaehlung)) # Remove count data with value NA
# remove counting stations which are not included in the cluster analysis
Wochenganglinie <- subset(Wochenganglinie, !Zaehlstelle %in% c("DZS_300a_Fahrraeder_Richtung_Frankfurt_Bockenheim___695",
                                                            "DZS_590__Bike_OUT_"))

### Join  count and weather data ###

# Create field for joining the data sets
Wochenganglinie$x <- sub(".*_(.*?)_.*", "\\1", Wochenganglinie$Zaehlstelle) 
Wetterdaten_taeglich$x <- Wetterdaten_taeglich$Zaehlstelle 

library(dplyr)
# Add columns of the weather data set
Wochenganglinie <- Wochenganglinie %>%
  left_join(Wetterdaten_taeglich, by = c("x", "Datum"))

### Rename columns ###
names(Wochenganglinie)[names(Wochenganglinie) == "Zaehlstelle.x"] <- "Zaehlstelle"
names(Wochenganglinie)[names(Wochenganglinie) == "Zaehlstelle.y"] <- "Zaehlstelle_Kurzname"

Wochenganglinie <- subset(Wochenganglinie, select= -x) # Delete column

##################################################################################
## Split data set in half to test stability of results of the kmeans clustering ##
##################################################################################

# the stability of the results gets tested by splitting the data set of the counting station in half
# and executing the k-means clustering for one half of the data set.
# This process is repeated a defined number of times and each time the results are saved
# In the end, with the variance of the results, it can be determined the stability of the results of the k-means clustering.

Stabilitätscluster <- Wochenganglinie # copy data

alle_cluster_ergebnisse <- data.frame() # Create empty table for all results of the cluster analysis

# Settings for cluster analysis
max_halbierungen <- 100 #  set amount of repetitions for splitting the data and executing the cluster analysis
max_iterations_pro_halbierung <- 1000 # set amount of iterations of the k-means-clustering to get the result with the highest silhouette width
Clusteranzahl <- 3 # set number of clusters

# Loop for splitting the data set in half #
for (halbierung in 1:max_halbierungen) {
  
  # generate random indizes
  zufallsindizes <- sample(1:nrow(Stabilitätscluster), size = nrow(Stabilitätscluster) / 2, replace = FALSE)
  
  # Analyze first half of data set
  Wochenganglinie <- Stabilitätscluster[zufallsindizes, ]


  ################ Filter the data ##############################

  Wochenganglinie <- subset(Wochenganglinie, !is.na(Zaehlung)) # remove NA-values
  
  
  ### Remove vacation days, holidays and days between holidays and weekends ###
  Wochenganglinie <- subset(Wochenganglinie, Tagtyp != "Feiertag" & Tagtyp != "Brückentag")
  Wochenganglinie <- subset(Wochenganglinie, Tagtyp != "Ferientag")

  
  ##################################################
  ######### Filter by the weather data ##############
  ###################################################
  
  ###  Average daily temperature of over 10°C ###
  Wochenganglinie <- subset(Wochenganglinie, Temperatur_C >= 10    ) 
  Wochenganglinie <- subset(Wochenganglinie, !is.na(Temperatur_C))  # remove NA-values

  ### Wind speed (not filtered) ###
  # Wochenganglinie <- subset(Wochenganglinie, Wind_kmh <=17.99182) 
  #  Wochenganglinie <- subset(Wochenganglinie, !is.na(Wind_kmh))

  ### Humidity (not filtered)  ###
  #  Wochenganglinie <- subset(Wochenganglinie, Luftfeuchtigkeit_Prozent <= 80.29895)
  #  Wochenganglinie <- subset(Wochenganglinie, !is.na(Luftfeuchtigkeit_Prozent))

  ### Snow fall ###
  Wochenganglinie <- subset(Wochenganglinie, Schnee_cm == 0)
  
  ### Rain (not filtered) ###
  # Wochenganglinie <- subset(Wochenganglinie, Regen_mm > 5)   # under set amount of rain
  # Wochenganglinie <- subset(Wochenganglinie, !is.na(Regen_mm))  # remove fields without data for amount of rain

  #####################################################################################
  ### Remove outliers with unexpected high or low count data with "Adjusted Boxplot" ###
  #####################################################################################
    
  # Load package if required
  
  if(!require('mrfDepth')) {
    install.packages('mrfDepth')
    library('mrfDepth')}

  # Function for Adjusted Boxplot
  remove_outliers_medcouple <- function(x, threshold = 1.5) {
    q <- quantile(x, c(0.25, 0.75))
    iqr <- q[2] - q[1]
    mc <- medcouple(x)
  
      if (mc >= 0) {
      lower_bound <- q[1] - 1.5 * exp(-3.5 * mc) * iqr
      upper_bound <- q[2] + 1.5 * exp(4 * mc) * iqr
    } else {
      lower_bound <- q[1] - 1.5 * exp(-4 * mc) * iqr
      upper_bound <- q[2] + 1.5 * exp(3.5 * mc) * iqr  }
        x_filtered <- ifelse(x < lower_bound | x > upper_bound, NA, x)
    return(x_filtered)  }
  
  # Use function of Adjusted Boxplot on data to remove outliers
    Wochenganglinie <- Wochenganglinie %>%
    group_by(Zaehlstelle, Wochentag)%>%
    mutate(Zaehlung_gefiltert = remove_outliers_medcouple(Zaehlung))
    Wochenganglinie <- subset(Wochenganglinie, !is.na(Zaehlung_gefiltert))
  
  ##############################################
  ### Filter by the median of the count data ###
  ############################################## 
  
  ### Remove counting stations with a median of counts under 10 over the whole count data ###
    
  library(dplyr)

  # Calculate median of the count data for each counting station (over the whole count data)
  Nullwerte <- Wochenganglinie %>%
    group_by(Zaehlstelle) %>%
    summarise(
      Min = min(Zaehlung),
      Q1 = quantile(Zaehlung, 0.25),
      Median = median(Zaehlung),
      mean = mean(Zaehlung),
      Q3 = quantile(Zaehlung, 0.75),
      Max = max(Zaehlung),
      SD = sd(Zaehlung),
      Var = var(Zaehlung),
      MAD = mad(Zaehlung),
      Spannweite=(max(Zaehlung)-min(Zaehlung))
    )
  Nullwerte_Ausreißer <- Nullwerte
  Nullwerte <- subset(Nullwerte, Median > 10) # remove counting stations with a median under 10 over all days of the week
  Nullwerte_Zaehlstellen <- data.frame(Zaehlstelle = unique(Nullwerte$Zaehlstelle))

  Wochenganglinie <- subset(Wochenganglinie, Zaehlstelle %in% Nullwerte_Zaehlstellen$Zaehlstelle )
  rm(Nullwerte_Zaehlstellen, Nullwerte) # remove not needed data
  
  ### Remove counting stations with a median of counts of 0 at one or more weekdays ###
  
  # Calculate median of the count data for each counting station and day

  Wochentage <- Wochenganglinie %>%
    group_by(Zaehlstelle, Wochentag) %>%
    summarise(
      Min = min(Zaehlung),
      Q1 = quantile(Zaehlung, 0.25),
      Median = median(Zaehlung),
      mean = mean(Zaehlung),
      Q3 = quantile(Zaehlung, 0.75),
      Max = max(Zaehlung),
      SD = sd(Zaehlung),
      Var = var(Zaehlung),
      MAD = mad(Zaehlung),
      Spannweite=(max(Zaehlung)-min(Zaehlung)))

  Wochentage_Ausreißer <- Wochentage %>%
    group_by(Zaehlstelle) %>%
    reframe(min_Median = min(Median))

  Wochentage <- subset(Wochentage, Median > 0 ) # remove days with a median of 0 from the data set
  
  Wochentage_Zaehlstellen <- Wochentage %>%
    group_by(Zaehlstelle) %>%
    summarise(count = n()) # count the days of the weeks which have a median of over 0
  
  # remove counting stations with one or more days with a median of 0
  Wochentage_Zaehlstellen <- subset(Wochentage_Zaehlstellen, count == 7) 
  Wochenganglinie <- subset(Wochenganglinie, Zaehlstelle %in% Wochentage_Zaehlstellen$Zaehlstelle)

  ###########################################################################
  ### Filter counting station by the number of values per day of the week ###
  ###########################################################################
  Datensatz_M2 <- Wochenganglinie # Copy data set 
  Zaehlstellen <- Datensatz_M2 # Data set to determine the counting stations used in analysis

  ### Counting stations with at least 8 values pro weekday ###
  
  library(dplyr)
  Zaehlstellen <- Zaehlstellen %>%
    group_by(Zaehlstelle, Wochentag) %>%
    summarise(count = n()) # count number of values for each weekday of a counting station

  Zaehlstellen <- subset(Zaehlstellen, count > 7 ) # remove days of a counting station with under 8 values

  # Count how many weekdays per counting station have more than 8 values
  Zaehlstellen1 <- Zaehlstellen %>%
    group_by(Zaehlstelle) %>%
    summarize(count = n()) 

  # remove counting stations which dont have more than 8 values on all days of the week
  Zaehlstellen1 <- subset( Zaehlstellen1, count==7) 

  # Remove days which don't have enough data from the data set  
  Wochenganglinie_alle_tage <- subset(Datensatz_M2, Zaehlstelle %in% Zaehlstellen1$Zaehlstelle)
  
  rm(Zaehlstellen, Zaehlstellen1, Datensatz_M2) # remove not needed data

  ############################################################################################
  ### Calculate portion of count data on one day compared to the average of the whole week ###
  ############################################################################################
  
  # Create data set with the median and average of the count data over each day of a counting station  
  wochenganglinien_gefiltert <- Wochenganglinie_alle_tage %>%
  group_by(Wochentag, Zaehlstelle, Kurzname) %>%
  reframe(Median_Wochentag = median(Zaehlung, na.rm = TRUE),
    Mean_Wochentag = mean(Zaehlung, na.rm =TRUE)) 

  # Calculate ratio of average count data of one day of the week in comparison to the average of the whole data of a counting station
  wochenganglinien_gefiltert <- wochenganglinien_gefiltert %>%
    group_by(Zaehlstelle) %>%
    mutate(Anteil_mean = Mean_Wochentag/ sum(Mean_Wochentag),
           Anteil_median= Median_Wochentag/sum(Median_Wochentag))

  rm(Clustertabelle) # remove not needed data
  
  #################################################################
  ### Create table to prepare the data for the cluster analysis ###
  #################################################################
  
  library(reshape2)
  library(stats)
  #  reshape data table from long format to wide format
  
  Clustertabelle <- reshape2::dcast(wochenganglinien_gefiltert, Zaehlstelle + Kurzname  ~ Wochentag, value.var = 'Anteil_mean', fill = 0)

  ############################################
  ######## Single linkage clustering #########
  ############################################
  
  # Create table for cluster analysis with single linkage clustering
  Clustertabelle_sl1 <- data.frame(Montag     = Clustertabelle$Montag,
                                   Dienstag   = Clustertabelle$Dienstag,
                                  Mittwoch   = Clustertabelle$Mittwoch,
                                  Donnerstag = Clustertabelle$Donnerstag,
                                  Freitag    = Clustertabelle$Freitag,
                                  Samstag    = Clustertabelle$Samstag,
                                  Sonntag    = Clustertabelle$Sonntag)

  Clustertabelle_sl1 <- na.omit(Clustertabelle_sl1) # remove NA-Values

  dist_matrix_sl1 <- dist(Clustertabelle_sl1, method = 'euclidean')^2 #  create distance matrix with squared euclidean distance

  cluster_sl1 <- hclust(dist_matrix_sl1, method = 'single') # execute single-linkage-clustering


  ### Filter through ranges of euclidean distance ###
  
  # Nr. 1  All weekly flow patterns in single groups
  cutree_sl1.1 <- cutree(cluster_sl1, h = -0.002) # Determine cluster stations within a set euclidean distance
  Clustertabelle$Cluster_sl1.1 <- cutree_sl1.1 # add all cluster within the  set range to the column "sl1.1"

  # Count amount of counting stations in the cluster groups and add in table  sl1.1
  Häufigkeiten_sl1.1 <- data.frame(Häufigkeiten = table(Clustertabelle$Cluster_sl1.1)) # count amount of counting stations per cluster group
  Häufigkeiten_sl1.1$Häufigkeiten.Var1 <- as.integer((Häufigkeiten_sl1.1$Häufigkeiten.Var1)) # set type of data

  library(dplyr)
  Clustertabelle <- left_join(Clustertabelle, Häufigkeiten_sl1.1, by = c("Cluster_sl1.1" = "Häufigkeiten.Var1")) # join cluster assignment with main data set
  colnames(Clustertabelle)[colnames(Clustertabelle) == "Häufigkeiten.Freq"] <- "Anzahl_Clusterobjekte_1" # Rename column

  # Nr. 2 Cluster groups with weekly flow patterns in euclidean distance of 0.002    
  cutree_sl1.2 <- cutree(cluster_sl1, h = 0.001) # Determine cluster stations within a set euclidean distance
  Clustertabelle$Cluster_sl1.2 <- cutree_sl1.2 # Add all cluster within the set range to the column "sl1.2"

  # Count amount of counting stations in the cluster groups and add in table  sl1.2
  Häufigkeiten_sl1.2 <- data.frame(Häufigkeiten = table(Clustertabelle$Cluster_sl1.2)) # count amount of counting stations per cluster group
  Häufigkeiten_sl1.2$Häufigkeiten.Var1 <- as.integer((Häufigkeiten_sl1.2$Häufigkeiten.Var1)) # set type of data
  colnames(Häufigkeiten_sl1.2)[colnames(Häufigkeiten_sl1.2) == "Häufigkeiten.Freq"] <- "Anzahl_Clusterobjekte_2"  # Rename column

  Clustertabelle <- left_join(Clustertabelle, Häufigkeiten_sl1.2, by = c("Cluster_sl1.2" = "Häufigkeiten.Var1")) # join cluster assignment with main data set

  rm(Clustertabelle_gefiltert1,Häufigkeiten_sl1.3,Häufigkeiten_sl1.2,Häufigkeiten_sl1.1, Häufigkeiten_sl1,Clustertabelle_sl1, dist_matrix, dist_matrix_sl1)  # Löschen nicht mehr benötigten Datensets

# Filter 0.001 
  Clustertabelle_0.001 <- subset(Clustertabelle, Anzahl_Clusterobjekte_2 >=50)

# ###################################################################
####################### clustering with k-means ##########################
#####################################################################

  # Set starting partition
  # define of order of columns
    Tabellenorder1 <- c("Zaehlstelle", "Kurzname", "Montag", "Dienstag", "Mittwoch", "Donnerstag", 
                      "Freitag",  "Samstag", "Sonntag") 
  Clustertabelle_0.001 <- Clustertabelle_0.001[, Tabellenorder1] # set order

  # Execute k-means-Clustering
  library(cluster)
  
  # Set variables
  min_min_silhouette <- -1
  max_avg_silhouette <- -1
  best_kmeans_result <- NULL
  best_silhouette_scores <- NULL
  best_max_avg_silhouette <- -1
  best_kmeans_result <- NULL
 
  # Repeat k means clustering in for-loop and save result with highest silhouette width 
  for (iteration in 1:max_iterations_pro_halbierung) {
    kmeans_result <- kmeans(Clustertabelle_0.001[, 3:9], centers = Clusteranzahl, iter.max = 1000) # clustering with k-means
    
    # determine silhouette width
    silhouette_scores <- silhouette(kmeans_result$cluster, dist(Clustertabelle_0.001[, 3:9]))
    Silhouettenbreiten <- data.frame(silhouette_scores)
    Avg_Silhouettenbreite <- median(Silhouettenbreiten$sil_width)
    
    # Save the result with the higher silhouette width
    if (Avg_Silhouettenbreite > best_max_avg_silhouette) {
      best_max_avg_silhouette <- Avg_Silhouettenbreite
      best_kmeans_result <- kmeans_result
    }
  }
  
  # Convert result in a data frame
  best_kmeans_result <- data.frame(
    Halbierung = halbierung,
    Cluster    = c(1:Clusteranzahl),
    Montag     = best_kmeans_result$centers[, 1],
    Dienstag   = best_kmeans_result$centers[, 2],
    Mittwoch   = best_kmeans_result$centers[, 3],
    Donnerstag = best_kmeans_result$centers[, 4],
    Freitag    = best_kmeans_result$centers[, 5],
    Samstag    = best_kmeans_result$centers[, 6],
    Sonntag    = best_kmeans_result$centers[, 7],
    Avg_Silhouette = Avg_Silhouettenbreite
  )
  
  # Sort cluster center by their values
    max_values <- sort(unique(best_kmeans_result$Sonntag), decreasing = TRUE)
  
    # Rename cluster groups according to their relative traffic volume on Sunday ("Sonntag")
    # --> highest value on sunday is cluster group 1, next highest cluster group 2...  
    best_kmeans_result <- best_kmeans_result %>%
    mutate(Cluster = ifelse(Sonntag == max_values[1], 1,
                            ifelse(Sonntag == max_values[2], 2,
                                   ifelse(Sonntag == max_values[3], 3,
                                          ifelse(Sonntag == max_values[4], 4,
                                                 ifelse(Sonntag == max_values[5], 5, 6))))))
  
  alle_cluster_ergebnisse <- rbind(alle_cluster_ergebnisse, best_kmeans_result) # add result of this iteration of splitting to "alle_cluster_ergebnisse"
}

# remove not needed data
rm(halbierung, iteration, max_avg_silhouette, max_halbierungen, max_iterations_pro_halbierung,
   max_values, min_min_silhouette, zufallsindizes, cutree_sl1.1, cutree_sl1.2, Clusteranzahl, 
   best_max_avg_silhouette, Avg_Silhouettenbreite, remove_outliers_medcouple,wochenganglinien_gefiltert,
   Wochenganglinie_alle_tage, Clustertabelle_0.001, best_kmeans_result, cluster_sl1, Nullwerte_Ausreißer,
   Wochentage_Ausreißer, Wochentage_Zaehlstellen, Stabilitätscluster, kmeans_result,
   best_max_avg_silhouette, best_silhouette_scores, Silhouettenbreiten, silhouette_scores)


Stattabelle<- alle_cluster_ergebnisse# copy results of clustering

library(tidyr)
# Rotate table and create columns for every day of the week ("Wochentag") and for counts ("Zaehlung")
Stattabelle <- Stattabelle %>%
  pivot_longer(cols = 3:9, names_to = "Wochentag", values_to = "Zaehlung") 

Stattabelle$Wochentag <- factor(Stattabelle$Wochentag,
                  levels = c("Montag","Dienstag","Mittwoch","Donnerstag","Freitag","Samstag","Sonntag")
                  , ordered = TRUE)

Stattabelle$Wochentag_num <- as.numeric(Stattabelle$Wochentag)

# calculate variance of the data 
Stat_Stabilitätsprüfung <- Stattabelle %>%
  group_by(Cluster, Wochentag, Wochentag_num) %>%
  reframe(Min = round(min(Zaehlung), digits = 4),
          Max = round(max(Zaehlung), digits = 4),
          Mean   = round(mean(Zaehlung), digits = 4),
          SD = round(sd(Zaehlung), digits = 4),
          Anzahl = n())

rm(Stattabelle) # remove not needed data

################
### Plotting ###
################

### Cluster 1 ###
cluster_1_data <- alle_cluster_ergebnisse[alle_cluster_ergebnisse$Cluster == 1, ]

# set file path
file_path <- file.path("C:/Leon/Studium/Master/Masterarbeit/Rscripte_Masterarbeit_Leon_Weyandt clean/Clustering/figures")
# set plot name
png(file = paste0(file_path, "/", "Stability_test_cluster_1", ".png"), 
    width = 20, height = 15.5, units = 'cm', res= 300)

# Adjust size of plot
par(mar = c(4.5, 4.5,2.5, 1.5))  # set margins for the bottom, left, top and right sides of the plot

# Create plot
matplot(t(cluster_1_data[, 3:9]), type = "l", lty = 1, lwd = 4, cex.lab = 1.6,
        cex.axis = 1.4, cex.main = 1.6, main = "Cluster 1 - Stability test",
        col = farben, xlab = "Day of the week",
        ylab = "Proportion of weekly traffic volume", ylim = c(0, 0.4),
        xaxt = "n")  # Prevent x-axis from being drawn
        axis(1, at = 1:7, labels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),  cex.axis =1.4)
dev.off()

### Cluster 2 ###
cluster_1_data <- alle_cluster_ergebnisse[alle_cluster_ergebnisse$Cluster == 2, ]

# set file path
file_path <- file.path("C:/Leon/Studium/Master/Masterarbeit/Rscripte_Masterarbeit_Leon_Weyandt clean/Clustering/figures")
# set plot name
png(file = paste0(file_path, "/", "Stability_test_cluster_2", ".png"), 
    width = 20, height = 15.5, units = 'cm', res= 300)

# Adjust size of plot
par(mar = c(4.5, 4.5,2.5, 1.5))  # set margins for the bottom, left, top and right sides of the plot

# Create plot
matplot(t(cluster_1_data[, 3:9]), type = "l", lty = 1, lwd = 4, cex.lab = 1.6,
        cex.axis = 1.4, cex.main = 1.6, main = "Cluster 2 - Stability test",
        col = farben, xlab = "Day of the week",
        ylab = "Proportion of weekly traffic volume", ylim = c(0, 0.4),
        xaxt = "n")  # Prevent x-axis from being drawn
        axis(1, at = 1:7, labels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),  cex.axis =1.4)
dev.off()

### Cluster 3 ###
cluster_1_data <- alle_cluster_ergebnisse[alle_cluster_ergebnisse$Cluster == 3, ]

# set file path
file_path <- file.path("C:/Leon/Studium/Master/Masterarbeit/Rscripte_Masterarbeit_Leon_Weyandt clean/Clustering/figures")
# set plot name
png(file = paste0(file_path, "/", "Stability_test_cluster_3", ".png"), 
    width = 20, height = 15.5, units = 'cm', res= 300)

# Adjust size of plot
par(mar = c(4.5, 4.5,2.5, 1.5))  # set margins for the bottom, left, top and right sides of the plot

# Create plot
matplot(t(cluster_1_data[, 3:9]), type = "l", lty = 1, lwd = 4, cex.lab = 1.6,
        cex.axis = 1.4, cex.main = 1.6, main = "Cluster 3 - Stability test",
        col = farben, xlab = "Day of the week",
        ylab = "Proportion of weekly traffic volume",
        ylim = c(0, 0.4), xaxt = "n")  # Prevent x-axis from being drawn
        axis(1, at = 1:7, labels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),  cex.axis =1.4)
dev.off()

# ### Cluster 4 ###
# cluster_1_data <- alle_cluster_ergebnisse[alle_cluster_ergebnisse$Cluster == 4, ]
# 
# set file path
# file_path <- file.path("C:/Leon/Studium/Master/Masterarbeit/Rscripte_Masterarbeit_Leon_Weyandt clean/Clustering/figures")
# set plot name# 
# png(file = paste0(file_path, "/", "Stability test", "4_", Name, ".png"), 
#     width = 20, height = 15.5, units = 'cm', res= 300)
# 
## Adjust size of plot
# par(mar = c(4.5, 4.5,1.5, 1.5))  # set margins for the bottom, left, top and right sides of the plot

 # Create plot
# matplot(t(cluster_1_data[, 3:9]), type = "l", lty = 1, lwd = 4, cex.lab = 1.6,
#         cex.axis = 1.4, cex.main = 1.6, main = "Cluster 4 - Stability test",
#         col = farben, xlab = "Day of the week",
#         ylab = "Proportion of weekly traffic volume",
#         ylim = c(0, 0.4), xaxt = "n")  # Prevent x-axis from being drawn
#         axis(1, at = 1:7, labels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),  cex.axis =1.4)
# dev.off()
 
 rm(cluster_1_data) # remove not needed data

