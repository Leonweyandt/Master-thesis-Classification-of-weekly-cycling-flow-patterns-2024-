####################################################################################
########################## CLUSTER ANALYSIS METHOD 2 ###############################
####################################################################################

### Method 2: Cluster analysis with the whole data of each counting station ###

  # Removal of counting stations which can't be used in analysis
  library(dplyr)
  Wochenganglinie <- subset(Alle_summiert, !(Kurzname %in% c("DZS_140", "DZS_071b")))
  Wochenganglinie <- subset(Wochenganglinie, !(Zaehlstelle %in% c("DZS_300a_Fahrraeder_Richtung_Frankfurt_Bockenheim___695",   
                                                                  "DZS_590__Bike_OUT_")))
  
###################################
### Join count and weather data ###
###################################

Wochenganglinie$x <- sub(".*_(.*?)_.*", "\\1", Wochenganglinie$Zaehlstelle) # create field for joining the data sets
Wetterdaten_taeglich$x <- Wetterdaten_taeglich$Zaehlstelle # create field for joining the data sets

# Join the columns of the weather data to count data set
library(dplyr)
Wochenganglinie <- Wochenganglinie %>%
  left_join(Wetterdaten_taeglich, by = c("x", "Datum"))

# Rename columns
names(Wochenganglinie)[names(Wochenganglinie) == "Zaehlstelle.x"] <- "Zaehlstelle"
names(Wochenganglinie)[names(Wochenganglinie) == "Zaehlstelle.y"] <- "Zaehlstelle_Kurzname"
Wochenganglinie <- subset(Wochenganglinie, select= -x) # remove not needed column

###################################################################
#####################  FILTER THE DATA  ###########################
###################################################################

# Remove NA-values
Wochenganglinie <- subset(Wochenganglinie, !is.na(Zaehlung)) 

### Remove vacation days, holidays and days between holidays and weekends ###
Wochenganglinie <- subset(Wochenganglinie, Tagtyp != "Feiertag" & Tagtyp != "Brückentag")
Wochenganglinie <- subset(Wochenganglinie, Tagtyp != "Ferientag")

###################################################
######### Filter by the weather data ##############
###################################################

###  Average daily temperature of over 10°C ###
Wochenganglinie <- subset(Wochenganglinie, Temperatur_C >= 10) 
Wochenganglinie <- subset(Wochenganglinie, !is.na(Temperatur_C))  # Remove data without information to temperature

### Wind speed (not filtered) ###
# Wochenganglinie <- subset(Wochenganglinie, Wind_kmh <=17.99182) 
# Wochenganglinie <- subset(Wochenganglinie, !is.na(Wind_kmh))

### Humidity (not filtered)  ###
# Wochenganglinie <- subset(Wochenganglinie, Luftfeuchtigkeit_Prozent <= 80.29895)
# Wochenganglinie <- subset(Wochenganglinie, !is.na(Luftfeuchtigkeit_Prozent))

### Snow fall ###
Wochenganglinie <- subset(Wochenganglinie, Schnee_cm == 0) # Remove days with snow fall

### Rain (not filtered) ###
# Wochenganglinie <- subset(Wochenganglinie, Regen_mm == 0)   # setting maximum amount of rain
# Wochenganglinie <- subset(Wochenganglinie, !is.na(Regen_mm))  # remove data without information to amount of rain

#####################################################################################
### Remove outliers with unexpected high or low count data with "Adjusted Boxplot" ###
#####################################################################################

# Load package if required
   if(!require('mrfDepth')) {
      install.packages('mrfDepth')
        library('mrfDepth') }

# Function for Adjusted Boxplot
remove_outliers_medcouple <- function(x, threshold = 1.5) {
  q <- quantile(x, c(0.25, 0.75))
  iqr <- q[2] - q[1]
  mc <- medcouple(x)

  if (mc >= 0) {
    lower_bound <- q[1] - 1.5 * exp(-4 * mc) * iqr
    upper_bound <- q[2] + 1.5 * exp(3 * mc) * iqr
  } else {
    lower_bound <- q[1] - 1.5 * exp(-3 * mc) * iqr
    upper_bound <- q[2] + 1.5 * exp(4 * mc) * iqr
  }

  x_filtered <- ifelse(x < lower_bound | x > upper_bound, NA, x)
  return(x_filtered)
}

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
    Median = median(Zaehlung))

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
    Median = median(Zaehlung))

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

Datensatz_M2 <-  Wochenganglinie # copy data set
Zaehlstellen <- Datensatz_M2 # data set to determine the counting stations used in analysis

### Counting stations with at least 8 values pro weekday ###
library(dplyr)
Zaehlstellen <- Zaehlstellen %>%
  group_by(Zaehlstelle, Wochentag) %>%
  summarise(count = n()) # count number of values per day of the week for each counting station

Zaehlstellen <- subset(Zaehlstellen, count >= 8) # set min value for values per day of the week

# Count for ever counting station the number of days of the week that fit the criteria of 8 values per day
Zaehlstellen1 <- Zaehlstellen %>%
  group_by(Zaehlstelle) %>%
  summarize(count = n())

# remove counting stations which dont have more than 8 values on all days of the week
Zaehlstellen1 <- subset( Zaehlstellen1, count==7) 

# Remove days which dont have enough data from the data set  
Wochenganglinie_alle_tage <- subset(Datensatz_M2, Zaehlstelle %in% Zaehlstellen1$Zaehlstelle)

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

rm(Datensatz_M2, Zaehlstellen1) # remove not needed data

#################################################################
### Create table to prepare the data for the cluster analysis ###
#################################################################

library(reshape2) 
library(stats)
#  reshape data table from long format to wide format
Clustertabelle <- reshape2::dcast(wochenganglinien_gefiltert,
                  Zaehlstelle + Kurzname  ~ Wochentag, value.var = 'Anteil_median', fill = 0)
  
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

Clustertabelle_sl1 <- na.omit(Clustertabelle_sl1) # remove NA-values

Clustertabelle <- merge(Clustertabelle, Zaehlstellenanzahl_ID, by = "Zaehlstelle") # add the previous created "ID" of counting station

library(dplyr)
Clustertabelle <- subset(Clustertabelle, select = -Kurzname.y) # remove duplicate column
Clustertabelle <- Clustertabelle %>% rename(Kurzname=Kurzname.x) # rename column

dist_matrix_sl1 <- dist(Clustertabelle_sl1, method = 'euclidean')^2 # create distance matrix with squared euclidean distance

cluster_sl1 <- hclust(dist_matrix_sl1, method = 'single') # execute single linkage clustering

# Create dendrogram for view in R
par(mar = c(1, 4.5, 1.5, 1))  # set margins for the bottom, left, top and right sides of the plot

plot(cluster_sl1, main = "Dendrogram of single linkage clustering",
     xlab = "Counting station", cex.main = 2.5, cex.axis = 1.9, cex.lab = 2.2, cex= 0.1,
     ylab = "Squared euclidean distance", 
     yaxt = "n") # Prevent x-axis from being drawn

# Set ticks for y-axis
axis(side = 2, at = seq(0, 0.03, by = 0.001), cex.axis = 1.1,
     ylab = 'Squared euclidean distance') 
par(cex.axis = 1.2, cex.lab = 1.4)

### Filter through ranges of euclidean distance ###

# Nr. 1  All weekly flow patterns in single groups

cutree_sl1.1 <- cutree(cluster_sl1, h = -0.002) # Determine cluster stations within a set euclidean distance
Clustertabelle$Cluster_sl1.1 <- cutree_sl1.1 # add all cluster within the set range to the column "sl1.1"

# Count amount of counting stations in the cluster groups and add in table  sl1.1
Häufigkeiten_sl1.1 <- data.frame(Häufigkeiten = table(Clustertabelle$Cluster_sl1.1)) # count amount of counting stations per cluster group
Häufigkeiten_sl1.1$Häufigkeiten.Var1 <- as.integer((Häufigkeiten_sl1.1$Häufigkeiten.Var1)) # set data type

library(dplyr)
Clustertabelle <- left_join(Clustertabelle, Häufigkeiten_sl1.1, by = c("Cluster_sl1.1" = "Häufigkeiten.Var1")) # join cluster assignment with main data set

colnames(Clustertabelle)[colnames(Clustertabelle) == "Häufigkeiten.Freq"] <- "Anzahl_Clusterobjekte_1" # rename column


# Nr. 2 Cluster groups with weekly flow patterns in euclidean distance of 0.002    

cutree_sl1.2 <- cutree(cluster_sl1, h = 0.002) # Determine cluster stations within a set euclidean distance

Clustertabelle$Cluster_sl1.2 <- cutree_sl1.2 # add all cluster within the set range to the column "sl1.2"

# Count amount of counting stations in the cluster groups and add in table  sl1.2
Häufigkeiten_sl1.2 <- data.frame(Häufigkeiten = table(Clustertabelle$Cluster_sl1.2)) # count amount of counting stations per cluster group
Häufigkeiten_sl1.2$Häufigkeiten.Var1 <- as.integer((Häufigkeiten_sl1.2$Häufigkeiten.Var1)) # set data type
colnames(Häufigkeiten_sl1.2)[colnames(Häufigkeiten_sl1.2) == "Häufigkeiten.Freq"] <- "Anzahl_Clusterobjekte_2" # Rename column


library(dplyr)
Clustertabelle <- left_join(Clustertabelle, Häufigkeiten_sl1.2, by = c("Cluster_sl1.2" = "Häufigkeiten.Var1")) # join cluster assignment with main data set

# Nr. 3 Cluster groups with weekly flow patterns in euclidean distance of 0.0015

cutree_sl1.3 <- cutree(cluster_sl1, h = 0.0015) # Determine cluster stations within a set euclidean distance

Clustertabelle$Cluster_sl1.3 <- cutree_sl1.3 # add all cluster within the  set range to the column "sl1.3"

# Count amount of counting stations in the cluster groups and add in table  sl1.3
Häufigkeiten_sl1.3 <- data.frame(Häufigkeiten = table(Clustertabelle$Cluster_sl1.3)) # count amount of counting stations per cluster group
Häufigkeiten_sl1.3$Häufigkeiten.Var1 <- as.integer((Häufigkeiten_sl1.3$Häufigkeiten.Var1)) # set data type
colnames(Häufigkeiten_sl1.3)[colnames(Häufigkeiten_sl1.3) == "Häufigkeiten.Freq"] <- "Anzahl_Clusterobjekte_3" # Rename column

library(dplyr)
Clustertabelle <- left_join(Clustertabelle, Häufigkeiten_sl1.3, by = c("Cluster_sl1.3" = "Häufigkeiten.Var1")) # join cluster assignment with main data set

# Nr. 4 Cluster groups with weekly flow patterns in euclidean distance of 0.001

cutree_sl1.4 <- cutree(cluster_sl1, h = 0.0010) # Determine cluster stations within a set euclidean distance

Clustertabelle$Cluster_sl1.4 <- cutree_sl1.4 # add all cluster within the  set range to the column "sl1.4"

# Count amount of counting stations in the cluster groups and add in table  sl1.4
Häufigkeiten_sl1.4 <- data.frame(Häufigkeiten = table(Clustertabelle$Cluster_sl1.4)) # count amount of counting stations per cluster group
Häufigkeiten_sl1.4$Häufigkeiten.Var1 <- as.integer((Häufigkeiten_sl1.4$Häufigkeiten.Var1)) # set data type
colnames(Häufigkeiten_sl1.4)[colnames(Häufigkeiten_sl1.4) == "Häufigkeiten.Freq"] <- "Anzahl_Clusterobjekte_4" # Rename column

library(dplyr)
Clustertabelle <- left_join(Clustertabelle, Häufigkeiten_sl1.4, by = c("Cluster_sl1.4" = "Häufigkeiten.Var1")) # join cluster assignment with main data set

rm(Häufigkeiten_sl1.4,Häufigkeiten_sl1.2,Häufigkeiten_sl1.1,Häufigkeiten_sl1.3, Häufigkeiten_sl1, Clustertabelle_sl1,
   Analysetabelle,Clustertabelle_0.0005, Clustertabelle_h2, Clustertabelle_h, 
   Clustertabelle_Startpartition, Clustertabelle_0.001)   # remove not needed data 


# Create data set of weekly flow patterns in euclidean distance of 0.001, 0.0015 and 0.002
Clustertabelle_0.001 <- subset(Clustertabelle, Anzahl_Clusterobjekte_4 >= 50)
Clustertabelle_0.0015 <- subset(Clustertabelle, Anzahl_Clusterobjekte_3 >= 50)
Clustertabelle_0.002 <- subset(Clustertabelle, Anzahl_Clusterobjekte_2 >=50)

# Analyze weekly flow patterns in an euclidean distance of over 0.002, 0.0015-0.001, 0.001-0.0015 or 0-0.001 #

# Extract this weekly flow patterns from data set
Ausreißer <- Clustertabelle %>%
    group_by(Zaehlstelle, Kurzname) %>%
    reframe(Ausreißer = if_else(Anzahl_Clusterobjekte_2 <= 50, "over 0.002",
                           if_else(Anzahl_Clusterobjekte_3 <= 50, "0.0015-0.002",
                             if_else(Anzahl_Clusterobjekte_4 <= 50, "0.001-0.0015",
                               if_else(Anzahl_Clusterobjekte_1 <= 50, "0-0.001",
                            "no")))))

# Analysis of the outlier ("Ausreißer") with the median over the whole data of a counting station 
Ausreißer <- left_join(Ausreißer, select(filter(Nullwerte_Ausreißer, 
                       Zaehlstelle %in% Ausreißer$Zaehlstelle), Zaehlstelle, Median),
                       by = "Zaehlstelle")

Ausreißer <- left_join(Ausreißer,select(subset(Clustertabelle, 
                       Zaehlstelle %in% Ausreißer$Zaehlstelle), Zaehlstelle, Cluster_sl1.1),
                       by = "Zaehlstelle") %>%
                       rename(Cluster = Cluster_sl1.1)

#################################################
########## Create plots for outliers ############
#################################################

# Different median values from the pre-filtering of the dataset are tested to determine how many outliers
# exist within specific median ranges, these are visualized in plots
# --> with this the minimum values of the median over the whole data of one counting station is determined

 # set colors ("farben")
 farben <- c("green", "blue", "red", "orange", "purple", "cyan", "magenta",
          "darkgreen", "brown", "grey")

  Medianwert <- 15  # Set median value under which the count data of the counting stations in the plot must be
Liste <- c("Outlier_0.000", "Outlier_0.001", "Outlier_0.0015", "Outlier_0.002")
Namen <- c("0 bis 0,001", "0,001 bis 0,0015","0,0015 bis 0,002" , "over 0,002")

for (i in seq_along(Liste)) {
  
  Name <- paste(Liste[i],"under_median_", Medianwert, ".png")   # set name of plot

  Outlier_0.002 <- subset(Ausreißer, Median < Medianwert & Ausreißer == "over 0.002")
  Outlier_0.0015 <- subset(Ausreißer, Median < Medianwert  & Ausreißer == "0.0015-0.002")
  Outlier_0.001 <- subset(Ausreißer, Median < Medianwert & Ausreißer == "0.001-0.0015")
  Outlier_0.000 <- subset(Ausreißer, Median < Medianwert  & Ausreißer == "0-0.001")

  # Save plot as PNG
  file_path_2 <- file.path("C:/Leon/Studium/Master/Masterarbeit/Rscripte_Masterarbeit_Leon_Weyandt clean/Clustering/figures",Name)
  
  # create data set from list
  datensatz <- get(Liste[i])
  
  # Set order of the columns
  Tabellenorder <- c("Zaehlstelle", "Montag", "Dienstag", "Mittwoch", "Donnerstag",
                     "Freitag", "Samstag", "Sonntag","ID", "Cluster_sl1.1", "Anzahl_Clusterobjekte_1",
                     "Cluster_sl1.2", "Anzahl_Clusterobjekte_2", "Cluster_sl1.3", "Anzahl_Clusterobjekte_3")
  
  # Subsetting the data set
  sl1.3 <- subset(Clustertabelle, Zaehlstelle %in% datensatz$Zaehlstelle)
  
  # Seting order
  sl1.3 <- sl1.3[, Tabellenorder]
  
  # Create plot 
  png(file = file_path_2, width = 20, height = 15, units = 'cm', res = 300)
  par(mar = c(4.5, 4.5, 1.5, 6.8))  # set margins for the bottom, left, top and right sides of the plot  

 matplot(t(sl1.3[1:11111, 2:8]), type = "l", lty = c(rep(1, 10), rep(2, 10), rep(3, 10)), col = farben,
            ylab = "Proportion of weekly traffic volume", lwd = 2, cex.lab = 1.45,  cex.axis = 1.35, 
            xlab= "Day of the week",  main = paste("Weekly flow patterns in SED of", Namen[i]),
            ylim = c(0, 0.4), cex.main = 1.45,
            xaxt = "n")  # Prevent x-axis from being drawn
            axis(1, at = 1:7, labels = c("Mon","Thu","Wed","Thu","Fri","Sat","Sun"),
                 cex.axis =1.25)# Prevent x-axis from being drawn
     
  # extract labels for legend from column "ID"
     sl1.3 <- sl1.3[order(sl1.3$ID), ]

    # set legend in the right top of the plot
    legend("topright", legend = factor(sl1.3$ID[1:nrow(sl1.3)]), col = farben, lty = c(rep(1, 10), rep(2, 10), rep(3, 10)),
    title = "ID", cex = 1.25, xpd = TRUE, inset = c(-0.213, 0), lwd = 2)

   dev.off() }

# remove not needed data
rm(sl1.3, Wochenganglinie_alle_tage, wochenganglinien_gefiltert, Wochentage_Ausreißer,
   Wochentage_Zaehlstellen, Wochentage, Nullwerte_Ausreißer, Ausreißer, Ausreißer_0.000, 
   Ausreißer_0.001, Ausreißer_0.0015, Ausreißer_0.002, Clustertabelle_0.001, Clustertabelle_0.0015,
   Clustertabelle_0.002, cluster_sl1, cutree_sl1.1, cutree_sl1.2, cutree_sl1.3, cutree_sl1.4, i,
   Medianwert, Name, Namen, dist_matrix_sl1, datensatz, file_path_2, remove_outliers_medcouple)

######################################################################
### cluster analysis with Ward.D2, Average, Complete and Centroid  ###
######################################################################
 
# Create data set for clustering #

# set counting stations which will be excluded from the cluster analysis
# this outliers got determined through the analysis of outliers with the single linkage clustering
# and the manual visual control of the weekly flow patterns in an squared euclidean distance of over 0.001
SL_Entfernungen <- c("131","018","034","069","070","166", "209", "165","175", "201","533","207") 


Clustertabelle_Auswahl <- subset(Clustertabelle, !ID %in% SL_Entfernungen) # Create data set

# Settings for hierarchical cluster analysis
Analysetabelle <- Clustertabelle_Auswahl # copy data for analysis

# Setting starting partition for hiercharchical cluster analysis
Clustertabelle_Startpartition <- data.frame(
                                  Montag     = Analysetabelle$Montag,
                                  Dienstag   = Analysetabelle$Dienstag,
                                  Mittwoch   = Analysetabelle$Mittwoch,
                                  Donnerstag = Analysetabelle$Donnerstag,
                                  Freitag    = Analysetabelle$Freitag,
                                 Samstag     = Analysetabelle$Samstag,
                                 Sonntag     = Analysetabelle$Sonntag)

Clustertabelle_Startpartition <- na.omit(Clustertabelle_Startpartition) # remove NA-values

dist_matrix_Startpartition <- dist(Clustertabelle_Startpartition, method = 'euclidean')^2 # create distance matrix with squared euclidean distance


# Execute different types of hierarchical cluster analysis
cluster_ward.D2 <- hclust(dist_matrix_Startpartition, method = 'ward.D2') # Ward.D2-method
cluster_complete <- hclust(dist_matrix_Startpartition, method = 'complete') # Complete-method
cluster_average <- hclust(dist_matrix_Startpartition, method = 'average') # Average-method
cluster_centroid <- hclust(dist_matrix_Startpartition, method = 'centroid') # Centroid-method
cluster_weightedaverage <- hclust(dist_matrix_Startpartition, method = 'mcquitty') # Weighted-Average-method"

# Create dendrograms for visual analysis in R
plot(cluster_ward.D2, main = 'dendrogram Ward.D2 clustering', xlab = 'Counting station', ylab = 'Squared euclidean distance')
plot(cluster_complete, main = 'dendrogram complete clustering', xlab = 'Counting station', ylab = 'Squared euclidean distance')
plot(cluster_average, main = 'dendrogram average clustering', xlab = 'Counting station', ylab = 'Squared euclidean distance')
plot(cluster_centroid, main = 'dendrogram centroid clustering', xlab = 'Counting station', ylab = 'Squared euclidean distance')
plot(cluster_weightedaverage, main = 'dendrogram weigthed average clustering', xlab = 'Counting station', ylab = 'Squared euclidean distance')


### Create Scree plot for each type of hierarchical cluster analysis ####
  
# Define the file path where the plots will be saved
file_path <- file.path("C:/Leon/Studium/Master/Masterarbeit/Rscripte_Masterarbeit_Leon_Weyandt clean/Clustering/figures")

# 1. Ward.D2 Method - Create Scree plot 
png(file = paste0(file_path, "/", "Screeplot_ward.D2.png"), 
    width = 20, height = 15, units = 'cm', res = 1800)  # Save the plot as a PNG file
par(mar = c(4.5, 4.5, 2.5, 1.5))  # Set margins for the plot
matplot(cluster_ward.D2$height[456:447],  # Plot the height values from the hierarchical clustering result
        xlab = "Number of cluster groups", 
        ylab = "Squared euclidean distance",
        type = "l", lty = 1, lwd = 4, 
        cex.lab = 1.6, cex.axis = 1.4, cex.main = 1.6,
        main = "Scree plot - Ward.D2 method",  # Title of the plot
        xaxt = "n")  # Prevent the x-axis from being drawn
axis(1, at = 1:10, labels = 1:10, cex.axis = 1.4)  # Custom x-axis labels
dev.off()  # save plot

# 2. Complete Method - Create Scree plot
png(file = paste0(file_path, "/", "Screeplot_complete.png"), 
    width = 20, height = 15, units = 'cm', res = 1800)
par(mar = c(4.5, 4.5, 2.5, 1.5))
matplot(cluster_complete$height[456:447], 
        xlab = "Number of cluster groups", 
        ylab = "Squared euclidean distance",
        type = "l", lty = 1, lwd = 4, 
        cex.lab = 1.6, cex.axis = 1.4, cex.main = 1.6,
        main = "Scree plot - Complete method", 
        xaxt = "n")
axis(1, at = 1:10, labels = 1:10, cex.axis = 1.4)
dev.off()

# 3. Average Method - Create Scree plot
png(file = paste0(file_path, "/", "Screeplot_average.png"), 
    width = 20, height = 15, units = 'cm', res = 1800)
par(mar = c(4.5, 4.5, 2.5, 1.5))
matplot(cluster_average$height[456:447], 
        xlab = "Number of cluster groups", 
        ylab = "Squared euclidean distance",
        type = "l", lty = 1, lwd = 4, 
        cex.lab = 1.6, cex.axis = 1.4, cex.main = 1.6,
        main = "Scree plot - Average method", 
        xaxt = "n")
axis(1, at = 1:10, labels = 1:10, cex.axis = 1.4)
dev.off()

# 4. Centroid Method - Create Scree plot
png(file = paste0(file_path, "/", "Screeplot_centroid.png"), 
    width = 20, height = 15, units = 'cm', res = 1800)
par(mar = c(4.5, 4.5, 2.5, 1.5))
matplot(cluster_centroid$height[456:447], 
        xlab = "Number of cluster groups", 
        ylab = "Squared euclidean distance",
        type = "l", lty = 1, lwd = 4, 
        cex.lab = 1.6, cex.axis = 1.4, cex.main = 1.6,
        main = "Scree plot - Centroid method", 
        xaxt = "n")
axis(1, at = 1:10, labels = 1:10, cex.axis = 1.4)
dev.off()

# 5. Weighted average Method - Create Scree plot
png(file = paste0(file_path, "/", "Screeplot_weightedaverage.png"), 
    width = 20, height = 15, units = 'cm', res = 1800)
par(mar = c(4.5, 4.5, 2.5, 1.5))
matplot(cluster_weightedaverage$height[456:447], 
        xlab = "Number of cluster groups", 
        ylab = "Squared euclidean distance",
        type = "l", lty = 1, lwd = 4, 
        cex.lab = 1.6, cex.axis = 1.4, cex.main = 1.6,
        main = "Scree plot - Weighted average method", 
        xaxt = "n")
axis(1, at = 1:10, labels = 1:10, cex.axis = 1.4)
dev.off()


## Cluster groups depending on number of cluster groups ##
Clusteranzahl= 3 # Determine best number of cluster through the dendrograms of the different methods of hierarchical cluster analysis 

# get distribution of the weekly flow patterns to the cluster groups for the set number of clusters
cutree_ward.D2 <- cutree(cluster_ward.D2, k = Clusteranzahl) 
cutree_complete <- cutree(cluster_complete, k = Clusteranzahl)
cutree_average <- cutree(cluster_average, k = Clusteranzahl)  
cutree_centroid <- cutree(cluster_centroid, k = Clusteranzahl)
cutree_weightedaverage <- cutree(cluster_weightedaverage, k = 5)

# add the distribution of the weekly flow patterns to the cluster groups to the data set
Analysetabelle$Ward.D2<- cutree_ward.D2
Analysetabelle$complete<- cutree_complete
Analysetabelle$average<- cutree_average
Analysetabelle$centroid<- cutree_centroid
Analysetabelle$weightedaverage<- cutree_weightedaverage

# Determine average weekly flow pattern for every cluster group for the different methods of hierarchical cluster analysis
Clusterward <- Analysetabelle %>%
  group_by(Ward.D2) %>%
  reframe(Montag = mean(Montag),
          Dienstag = mean(Dienstag),
          Mittwoch = mean(Mittwoch),
          Donnerstag = mean(Donnerstag),
          Freitag = mean(Freitag),
          Samstag = mean(Samstag),
          Sonntag = mean(Sonntag),
          Anzahl = n())

Clustercomplete <- Analysetabelle %>%
  group_by(complete) %>%
  reframe(Montag = mean(Montag),
          Dienstag = mean(Dienstag),
          Mittwoch = mean(Mittwoch),
          Donnerstag = mean(Donnerstag),
          Freitag = mean(Freitag),
          Samstag = mean(Samstag),
          Sonntag = mean(Sonntag),
          Anzahl = n())

Clusteraverage <- Analysetabelle %>%
  group_by(average) %>%
  reframe(Montag = mean(Montag),
          Dienstag = mean(Dienstag),
          Mittwoch = mean(Mittwoch),
          Donnerstag = mean(Donnerstag),
          Freitag = mean(Freitag),
          Samstag = mean(Samstag),
          Sonntag = mean(Sonntag),
          Anzahl = n())

Clustercentroid <- Analysetabelle %>%
  group_by(centroid) %>%
  reframe(Montag = mean(Montag),
          Dienstag = mean(Dienstag),
          Mittwoch = mean(Mittwoch),
          Donnerstag = mean(Donnerstag),
          Freitag = mean(Freitag),
          Samstag = mean(Samstag),
          Sonntag = mean(Sonntag),
          Anzahl = n())

Clusterweightedaverage <- Analysetabelle%>%
  group_by(weightedaverage) %>%
  reframe(Montag = mean(Montag),
          Dienstag = mean(Dienstag),
          Mittwoch = mean(Mittwoch),
          Donnerstag = mean(Donnerstag),
          Freitag = mean(Freitag),
          Samstag = mean(Samstag),
          Sonntag = mean(Sonntag),
          Anzahl = n())

# Remove cluster groups with few weekly flow patterns of the clustering with weigthed average
Clusterweightedaverage  <- subset(Clusterweightedaverage,weightedaverage != 3) # remove all weekly flow patterns in cluster 3
Clusterweightedaverage  <- subset(Clusterweightedaverage,weightedaverage != 5) # remove all weekly flow patterns in cluster 5
Clusterweightedaverage$weightedaverage[Clusterweightedaverage$weightedaverage==4] <- 3 # rename cluster group 4 to 3 

# remove not needed data
rm(cutree_average, cutree_centroid, cutree_complete, cutree_ward.D2, cutree_weightedaverage, Liste, file_path,
   dist_matrix_Startpartition, cluster_average, cluster_ward.D2, cluster_weightedaverage, cluster_centroid, 
   Clustertabelle_Startpartition, cluster_complete)

# ####################################################################
####################### K-Means-Clustering  ##########################
######################################################################

# Set starting partition for k means clustering # 

# define of order of columns
Tabellenorder1 <- c("Zaehlstelle", "Kurzname", "Montag", "Dienstag", "Mittwoch", "Donnerstag", 
                   "Freitag", "Samstag", "Sonntag", "Cluster_sl1.1","Anzahl_Clusterobjekte_1",
                   "Cluster_sl1.2","ID", "Anzahl_Clusterobjekte_2", "Cluster_sl1.3","Anzahl_Clusterobjekte_3")

Clustertabelle_kmeans <- Clustertabelle_Auswahl[, Tabellenorder1] # set order

# Execute k-means-Clustering
  
 # Set variables
 min_min_silhouette <- -1
 max_avg_silhouette <- -1
 best_kmeans_result <- NULL
 best_silhouette_scores <- NULL
 
 Clusteranzahl <-3 # Set number of clusters
 max_iterations <- 1000 # Set number of iterations
 
library(cluster)
 
 # Repeat k means clustering in for-loop and save result with highest silhouette width 
  for (iteration in 1:max_iterations) {
     kmeans_result <- kmeans(Clustertabelle_kmeans[,3:9], centers = Clusteranzahl, iter.max = 1000) # Executing k-Means-clustering

   # determine silhouette width
     silhouette_scores <- silhouette(kmeans_result$cluster, 
                                     dist(Clustertabelle_kmeans[,3:9]))
     Silhouettenbreiten <- data.frame(silhouette_scores)
     Avg_Silhouettenbreite <- mean(Silhouettenbreiten$sil_width)
     min_silhouette <- min(Silhouettenbreiten$sil_width)
   
   # Save the result with the higher silhouette width
      if (Avg_Silhouettenbreite > max_avg_silhouette) {
       max_avg_silhouette <- Avg_Silhouettenbreite
       min_min_silhouette <- min_silhouette
       best_kmeans_result <- kmeans_result
       best_silhouette_scores <- Silhouettenbreiten
      }}
 
 # Show results
 print("Beste durchschnittliche Silhouettenbreite:")
 print(max_avg_silhouette)
 print("Beste minimale Silhouettenbreite:")
  print(min_min_silhouette)
 
 # Convert result in a data frame 
kmeans_result_df<- data.frame(
  Cluster = c(1:Clusteranzahl), 
  Montag = best_kmeans_result$centers[, 1],
  Dienstag = best_kmeans_result$centers[, 2],
  Mittwoch = best_kmeans_result$centers[, 3],
  Donnerstag = best_kmeans_result$centers[, 4],
  Freitag = best_kmeans_result$centers[, 5],
  Samstag = best_kmeans_result$centers[,6],
  Sonntag = best_kmeans_result$centers[,7],
  Iterations = best_kmeans_result$iter)

# Sort cluster center by their values
library(dplyr)
max_values <- sort(unique(kmeans_result_df$Sonntag), decreasing = TRUE)

# Rename cluster groups according to their relative traffic volume on Sunday ("Sonntag")
# --> highest value on sunday is cluster group 1, next highest cluster group 2...
Clusterganglinien <- kmeans_result_df %>%
  mutate(Cluster = ifelse(Sonntag == max_values[1], 1,
                      ifelse(Sonntag == max_values[2], 2,
                         ifelse(Sonntag == max_values[3], 3,
                            ifelse(Sonntag == max_values[4], 4,
                               ifelse(Sonntag == max_values[5], 5,
                                  ifelse(Sonntag == max_values[6], 6,
                                     ifelse(Sonntag == max_values[7], 7,
                                        ifelse(Sonntag == max_values[8], 8,
                                           ifelse(Sonntag == max_values[9], 9, 10))))))))))

Clusterganglinien <- Clusterganglinien[order(Clusterganglinien$Cluster, decreasing = FALSE), ] # visually orders table in R

Zuordnung <- data.frame(Cluster=best_kmeans_result$cluster) # get distribution of the counting stations to the cluster groups

Clustertabelle_kmeans$kmeans <- Zuordnung$Cluster # add the distribution of counting stations to cluster groups to the main data set
Clustertabelle_kmeans$Silhouette <- best_silhouette_scores$sil_width #  add silhouette width of each weekly flow pattern to the main data set
# Add the name of the counting station to the list with the number of the distribution of the weekly flow patterns to the cluster groups
Zuordnung <- data.frame(Zaehlstelle = Clustertabelle_kmeans$Zaehlstelle, 
                                                              Cluster = Clustertabelle_kmeans$kmeans)


#########################
###### Plotting  ########
#########################

## Plot cluster center of the different hierarchical clustering methods and K-Means clustering ##

# set file path to save plot in folder
file_path <- file.path("C:/Leon/Studium/Master/Masterarbeit/Rscripte_Masterarbeit_Leon_Weyandt clean/Clustering/figures")
png(file = paste0(file_path, "/", "Comparison_results_of_clustering__methods", ".png"), 
    width = 20, height = 15, units = 'cm', res= 1800)

# Adjust size of plot
par(mar = c(4.5, 4.5,2.5, 1.5))  # set margins for the bottom, left, top and right sides of the plot

matplot( t(Clustercentroid[ 2:8]), type = "l", lty = 1, lwd = 4, cex.lab = 1.6, # create plot of cluster centers of centroid clustering
        cex.axis = 1.4, cex.main = 1.6,col = farben,
        main = "Comparison of the results of the cluster methods",
        ylab = "Proportion of weekly traffic volume",
        ylim = c(0, 0.4), 
        xlab = "Day of the week", xaxt = "n")  # Prevent x-axis from being drawn
axis(1, at = 1:7, labels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), cex.axis =1.4)

matlines( t(Clusterward[ 2:8]), type = "l", lty = 1, lwd = 4, col = farben) # create plot with cluster centers of Ward.D2 clustering
matlines( t(Clusterweightedaverage[1, 2:8]), type = "l", lty = 1, lwd = 4, col = farben)  # create plot with cluster centers of  Weighted average clustering   
matlines( t(Clusteraverage[ 2:8]), type = "l", lty = 1, lwd = 4,     col = farben) # create plot with cluster centers of Within average clustering
matlines( t(Clusterganglinien[ 2:8]), type = "l", lty = 1, lwd = 4,   col = "black") # create plot with cluster centers of K-Means clustering

legend_labels <- as.character(Clusterganglinien$Cluster) # add numbers of cluster groups as labels for the legend 

# Set legend to the left top of the plot
legend("topleft", legend = legend_labels, col = farben, x.intersp = 1, ncol = 1, seg.len = 2, 
       lty = 1, lwd = 4, title = "Cluster", cex = 1.2, xpd = TRUE, inset = c(0, 0))
dev.off()

## Plot cluster centers of K-Means clustering ##

# set file path to save plot in folder
  file_path <- file.path("C:/Leon/Studium/Master/Masterarbeit/Rscripte_Masterarbeit_Leon_Weyandt clean/Clustering/figures")
  png(file = paste0(file_path, "/", "K-Means-clustering", ".png"), width = 20, height = 15, units = 'cm', res= 1800)
  
  # Adjust size of plot
  par(mar = c(4.5, 4.5,2.5, 1.5))  # set margins for the bottom, left, top and right sides of the plot
  # Create plot
  matplot( t(Clusterganglinien[, 2:8]), type = "l", lty = 1, lwd = 4, cex.lab = 1.6,
           cex.axis = 1.4, cex.main = 1.6, col = farben,
           main = "Standardized weekly traffic flow patterns",
           ylab = "Proportion of weekly traffic volume",
           ylim = c(0, 0.4),
           xlab = "Day of the week",xaxt = "n")  # Prevent x-axis from being drawn
  axis(1, at = 1:7, labels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), cex.axis =1.4) # set labels of x-axis
  legend_labels <- as.character(Clusterganglinien$Cluster) # add numbers of cluster groups as labels for the legend 
  
  # Adding legend to the left top of the plot
  legend("topleft", legend = legend_labels, col = farben, x.intersp = 1, ncol = 1, seg.len = 2, 
         lty = 1, lwd = 4, title = "Cluster", cex = 1.4, xpd = TRUE, inset = c(0, 0))
  dev.off()


## Plotting all weekly flow patterns of each clusters ##

library(dplyr)
Clustertabelle <- left_join(Clustertabelle, Zuordnung, by= "Zaehlstelle") # add distribution ("Zuordnung") to main data set

Name <- "weekly_flow_patterns_of_cluster_" # set prefix for the name of the plots

  # Plot with weakly flow patterns for Cluster 1
  cluster_1_data <- Clustertabelle[Clustertabelle$Cluster == 1, ] # get weekly flow patterns in cluster 1
  cluster_1_data<-  cluster_1_data[,Tabellenorder] # set order
  
  # set file path
  file_path <- file.path("C:/Leon/Studium/Master/Masterarbeit/Rscripte_Masterarbeit_Leon_Weyandt clean/Clustering/figures")
  png(file = paste0(file_path, "/", "kmeans_", Name , "1", ".png"), 
      width = 20, height = 15, units = 'cm', res= 1800)

  # Create plot
  matplot(t(cluster_1_data[, 2:8]), type = "l", lty = 1, lwd = 1,
          main = paste("Weekly flow patterns of cluster 1"),
          col = farben, cex.lab = 1.6,cex.axis = 1.4, cex.main = 1.6,
          ylab = "Proportion of weekly traffic volume", ylim = c(0, 0.4),
          xlab= "Day of the week",
          xaxt = "n")  # Prevent x-axis from being drawn  
          axis(1, at = 1:7, labels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), cex.axis =1.4) # set labels of x-axis
  par(mar = c(5, 4, 4, 2) + 0.1)   # Set size of the plot
  dev.off() # save plot
  
  # Plot with weakly flow patterns for Cluster 2
  cluster_1_data <- Clustertabelle[Clustertabelle$Cluster == 2, ] # get weekly flow patterns in cluster 2
  cluster_1_data<-  cluster_1_data[,Tabellenorder] # set order
  
  # set file path
  file_path <- file.path("C:/Leon/Studium/Master/Masterarbeit/Rscripte_Masterarbeit_Leon_Weyandt clean/Clustering/figures")
  png(file = paste0(file_path, "/", "kmeans_", Name , "2", ".png"), 
      width = 20, height = 15, units = 'cm', res= 1800)
  
  # Create plot
  matplot(t(cluster_1_data[, 2:8]), type = "l", lty = 1, lwd = 1,
          main = paste("Weekly flow patterns of cluster 2"),
          col = farben, cex.lab = 1.6,cex.axis = 1.4, cex.main = 1.6,
          ylab = "Proportion of weekly traffic volume", ylim = c(0, 0.4),
          xlab= "Day of the week",
          xaxt = "n")  # Prevent x-axis from being drawn  
          axis(1, at = 1:7, labels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), cex.axis =1.4) # set labels of x-axis
  
  par(mar = c(5, 4, 4, 2) + 0.1)   # Set size of the plot
  dev.off() # save plot
  
  # Plot with weakly flow patterns for Cluster 3
  cluster_1_data <- Clustertabelle[Clustertabelle$Cluster == 3, ] # get weekly flow patterns in cluster 3
  cluster_1_data<-  cluster_1_data[,Tabellenorder] # set order
  
  # set file path
  file_path <- file.path("C:/Leon/Studium/Master/Masterarbeit/Rscripte_Masterarbeit_Leon_Weyandt clean/Clustering/figures")
  png(file = paste0(file_path, "/", "kmeans_", Name , "3", ".png"), 
      width = 20, height = 15, units = 'cm', res= 1800)
  
  # Create plot
  matplot(t(cluster_1_data[, 2:8]), type = "l", lty = 1, lwd = 1,
          main = paste("Weekly flow patterns of cluster 3"),
          col = farben, cex.lab = 1.6,cex.axis = 1.4, cex.main = 1.6,
          ylab = "Proportion of weekly traffic volume", ylim = c(0, 0.4),
          xlab= "Day of the week", xaxt = "n")  # Prevent x-axis from being drawn  
          
  axis(1, at = 1:7, labels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), cex.axis =1.4) # set labels of x-axis
  
  par(mar = c(5, 4, 4, 2) + 0.1)   # Set size of the plot
  dev.off() # save plot
 
  # Plot with weakly flow patterns for Cluster 4  (not used here)
  # cluster_1_data <- Clustertabelle[Clustertabelle$Cluster == 4, ] # get weekly flow patterns in cluster 4
  # cluster_1_data<-  cluster_1_data[,Tabellenorder1] # set order
  
  # set file path
  #file_path <- file.path("C:/Leon/Studium/Master/Masterarbeit/Rscripte_Masterarbeit_Leon_Weyandt clean/Clustering/figures")
  # png(file = paste0(file_path, "/", "kmeans_", Name , "4", ".png"), 
  #     width = 20, height = 15, units = 'cm', res= 1800)
  # 
    # Create plot
    # matplot(t(cluster_1_data[, 3:9]), type = "l", lty = 1, lwd = 1,
  #         main = paste("Weekly flow patterns of cluster 4"),
  #         col = farben, cex.lab = 1.6,cex.axis = 1.4, cex.main = 1.6,
  #         ylab = "Proportion of weekly traffic volume", ylim = c(0, 0.4),
  #         xlab= "Day of the week",         xaxt = "n")  # Prevent x-axis from being drawn  
  #         axis(1, at = 1:7, labels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), cex.axis =1.4) # set labels of x-axis
  # 
  # par(mar = c(5, 4, 4, 2) + 0.1)  # Set size of the plot
  # dev.off() # save plot

# remove not needed data
rm(max_iterations, min_min_silhouette, min_silhouette, iteration, Clusteranzahl, max_values, 
   Avg_Silhouettenbreite, max_avg_silhouette, file_path, cluster_1_data, best_kmeans_result,
   best_silhouette_scores, kmeans_result_df, kmeans_result, Clusterward, Clusteraverage, 
   Clustercentroid, Clustercomplete, Clusterweightedaverage, Clustertabelle_Auswahl, 
   Clustertabelle_Startpartition)

#####################################################
### EXPORT RESULTS OF K-MEANS CLUSTERING TO EXCEL ###
#####################################################

Zuordnung$Kurzname <- sub("^[^_]*_([^_]*)_.+", "\\1", Zuordnung$Zaehlstelle) # adjust short name ("Kurzname")
Zaehlstellenanzahl_ID$Kurzname <- sub("^[^_]*_([^_]*)_.+", "\\1", Zaehlstellenanzahl_ID$Zaehlstelle) # adjust short name ("Kurzname")
Exceldatei <- subset(Zaehlstellenanzahl_ID,!Name %in% Zuordnung$Zaehlstelle) # create data frame for excel file
Exceldatei$Cluster <- 0 # set column for cluster number
# add cluster number from cluster analysis 
Exceldatei$Cluster <-ifelse(!is.na(match(Exceldatei$Zaehlstelle, Zuordnung$Zaehlstelle)), 
                             Zuordnung$Cluster[match(Exceldatei$Zaehlstelle, Zuordnung$Zaehlstelle)], 0)

library(tidyr)
library(dplyr)
# Alter and add fields


Exceldatei <- merge(Exceldatei, Clustertabelle[,1:9], by  = c("Zaehlstelle"), all = TRUE) # add results of distribution and weekly flow patterns
Exceldatei <- subset(Exceldatei, select = -Kurzname.y) # remove not needed column
Exceldatei <- Exceldatei %>% rename("Short_name"="Kurzname.x") # rename column
# add silhoutte width to excel file
Exceldatei$Silhouette_width <- ifelse(!is.na(match(Exceldatei$Zaehlstelle, Clustertabelle_kmeans$Zaehlstelle)), 
                                      Clustertabelle_kmeans$Silhouette[match(Exceldatei$Zaehlstelle,
                                                                             Clustertabelle_kmeans$Zaehlstelle)], 0)
Exceldatei <- Exceldatei %>% rename("Counting_station"="Zaehlstelle") # rename column
Exceldatei <- Exceldatei %>% rename("Monday"="Montag") # rename column
Exceldatei <- Exceldatei %>% rename("Tuesday"="Dienstag") # rename column
Exceldatei <- Exceldatei %>% rename("Wednesday"="Mittwoch") # rename column
Exceldatei <- Exceldatei %>% rename("Thursday"="Donnerstag") # rename column
Exceldatei <- Exceldatei %>% rename("Friday"="Freitag") # rename column
Exceldatei <- Exceldatei %>% rename("Saturday"="Samstag") # rename column
Exceldatei <- Exceldatei %>% rename("Sunday"="Sonntag") # rename column

# set order of columns
Exceldatei <- Exceldatei[, c("Counting_station", "Short_name","ID","Cluster", "Monday", "Tuesday", "Wednesday",
                             "Thursday", "Friday", "Saturday", "Sunday", "Silhouette_width",
                             setdiff(names(Exceldatei), 
                                     c("Counting_station", "Short_name","ID","Cluster", "Monday", "Tuesday", "Wednesday",
                                         "Thursday", "Friday", "Saturday", "Sunday", "Silhouette_width")))]

# Save data frame in excel file
library(writexl)
write_xlsx(Exceldatei,"C:/Leon/Studium/Master/Masterarbeit/Rscripte_Masterarbeit_Leon_Weyandt clean/Clustering/Excel/cluster_distribution.xlsx")

rm(Exceldatei) # remove not needed data

#######################
### Other analyses  ###
#######################

# Plotting weekly flow patterns with low silhouette width #

# The weekly flow patterns which are positioned inbetween two cluster groups and therefor
# were not distributed with a high certainty to the cluster groups 
# --> might get redistributed to another cluster or removed from the result

Nummer = 1 # Select number of cluster
Silhouetten <- subset(Clustertabelle_kmeans , Silhouette < 0.3 & Silhouette >= 0.2) # set range of silhouette score to display in plot

# Set file path
file_path <- file.path(  "C:/Leon/Studium/Master/Masterarbeit/Rscripte_Masterarbeit_Leon_Weyandt clean/Clustering/figures")

png(file = paste0(file_path, "/", "Silhouette_0.2_to_0.3_",Nummer, ".png"), # set name of file
    width = 20, height = 15, units = 'cm', res= 300)

par(mar = c(4.5, 4.5,1.5, 6.8))  # set margins for the bottom, left, top and right sides of the plot

# Create plot
matplot(t(Clusterganglinien[, 2:8]), type = "l", lty = c(rep(2, 10), rep(2, 10), rep(3, 10)),
        col = "grey30", ylab = "Proportion of weekly traffic volume", lwd = 4, cex.lab = 1.6,
        cex.axis = 1.4,
     main = paste("Silhouette widths from 0.2 to 0.3"),  # set title of the plot depending on the chosen range of the silhouette width
        ylim = c(0, 0.4), cex.main = 1.6, xlab = "Day of the week", xaxt = "n" ) # Prevent x-axis from being drawn
axis(1, at = 1:7, labels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),  cex.axis =1.4)

# add second line to plot
Plot <- subset(Silhouetten , kmeans == Nummer) 
matlines(t(Plot[, 3:9]), type = "l", lty = 1, lwd = 4,
         col  = farben)

legend("topright", legend = Plot$ID, col = farben, lty = c(rep(1, 10), rep(2, 10), rep(3, 10)),
       title = "ID", cex = 1.2, xpd = TRUE, inset = c(-0.203, 0), lwd = 2)
dev.off()   

rm(Plot, Nummer, Silhouetten, silhouette_scores) # remove not needed data

#############################
### Demand-specific plots ###
#############################

# Choose counting stations from which the weekly flow pattern should be plotted

# Set file path
file_path_2 <- file.path(  "C:/Leon/Studium/Master/Masterarbeit/Rscripte_Masterarbeit_Leon_Weyandt clean/Clustering/figures",
                           "Removed.png") # set name depending on the topic of the produced plot

Bedarfsganglinien <- c("018","034","069","070","131", "165","175") # choose counting stations for the plot
Bedarfsgraphik <- subset(Clustertabelle, ID %in% Bedarfsganglinien) # create data set with counting stations for the plot

# Set definition of the order of the columns
Tabellenorder <- c("Zaehlstelle", "Montag", "Dienstag", "Mittwoch", "Donnerstag",
                   "Freitag", "Samstag", "Sonntag", "ID", "Kurzname", "Cluster_sl1.1", "Anzahl_Clusterobjekte_1",
                   "Cluster_sl1.2", "Anzahl_Clusterobjekte_2", "Cluster_sl1.3", "Anzahl_Clusterobjekte_3")

# Set order
sl1.3 <- Bedarfsgraphik[, Tabellenorder]
    
# Create plot 
png(file = file_path_2, width = 20, height = 15, units = 'cm', res = 300)
# Set size of the plot
par(mar = c(4.5, 4.5, 1.5, 6))  # set margins for the bottom, left, top and right sides of the plot
    
matplot(t(sl1.3[1:420, 2:8]), type = "l", lty = c(rep(1, 10), rep(2, 10), rep(3, 10)), col = farben,
             ylab = "Proportion of weekly traffic volume", lwd = 2, cex.lab = 1.4,
        cex.axis = 1.2, 
          xlab= "Day of the week",  main = "Removed flow patterns in SED from 0,001 to 0,0024", # set title of the plot
          ylim = c(0, 0.4), cex.main = 1.4, xaxt = "n" ) # Prevent x-axis from being drawn
axis(1, at = 1:7, labels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),  xlab = "Day of week", cex.axis =1.4)

    # set labels for legend
    legend_labels <- as.character(sl1.3$Kurzname)
    
    if (length(legend_labels) > 0) {
      # place legend in the top right of the plot
      legend("topright", legend = Bedarfsganglinien, col = farben, lty = c(rep(1, 10), rep(2, 10), rep(3, 10)),
             title = "ID", cex = 1.2, xpd = TRUE, inset = c(-0.197, 0), lwd = 2)    }
    dev.off()
    rm(sl1.3, Bedarfsganglinien, Bedarfsgraphik)
    
    
### Plot number of counting stations per minimal amount of daily values over all days of the week ###

# Determine the day of the week with the lowest amount of data values for each counting station
# and count the number of counting stations per amount of data values
# --> to determine the minimal value of data values necesary for a counting station to enter the analysis
    
#  Count minimal amount of daily values over all days of the week
Mindesanzahl <- Zaehlstellen %>%
  group_by(Zaehlstelle) %>%
  reframe(count = min(count))

Mindesanzahl <- Mindesanzahl %>%
  group_by(count) %>%
  reframe(Anzahl = n())
''
# set path of plot to save in folder
file_path <- file.path(  "C:/Leon/Studium/Master/Masterarbeit/Rscripte_Masterarbeit_Leon_Weyandt clean/Clustering/figures")
# set name and size of plot
png(file = paste0(file_path, "/", "minimal_amount_of_data_values_per_day_of_the_week.png"), 
      width = 40, height = 30, units = 'cm', res= 300)

   # Create plot 
  matplot(
    x = Mindesanzahl$count,
    y = Mindesanzahl$Anzahl,
    type = "l",
    lty = 1, lwd = 4, cex.lab = 1.6,
    cex.axis = 1.4, cex.main = 1.6, col = "black",
    main = "Amount of counting stations dependent on the minimal amount of data values per day of the week",
    ylab = "Amount of counting stations",
    xlab = "Minimal amount of data values per day of the week",
    xaxt = "n"  # Prevent x-axis from being drawn
      )
  axis(1, at = seq(0, max(Mindesanzahl$count), by = 5), cex.axis = 1.4)
  
  # Set plot size
  par(mar = c(3.5, 4.5,2.5, 1.5))  # set margins for the bottom, left, top and right sides of the plot
 
  dev.off()
  

rm(file_path, file_path_2, Mindesanzahl, SL_Entfernungen, Name, legend_labels) # remove not needed data
