##################################################################################
#########################  Cluster analysis method 1 #############################
##################################################################################

### Method 1: Same dates of the count data is used for all counting stations ###

{ 
  # Load data and remove counting stations which are not used in the analysis
  Wochenganglinie <- subset(Alle_summiert, Kurzname != "DZS_140")
  Wochenganglinie <- subset(Wochenganglinie, Kurzname != "DZS_071b") 

  ### Join count and weather data ###
  Wochenganglinie$x <- sub(".*_(.*?)_.*", "\\1", Wochenganglinie$Zaehlstelle) # create field for joining the data sets
  Wetterdaten_taeglich$x <- Wetterdaten_taeglich$Zaehlstelle # create field for joining the data sets
  
  # Join count and weather data
  library(dplyr)
  Wochenganglinie <- Wochenganglinie %>%
    left_join(Wetterdaten_taeglich, by = c("x", "Datum"))
  
  # Rename columns
  names(Wochenganglinie)[names(Wochenganglinie) == "Zaehlstelle.x"] <- "Zaehlstelle"
  names(Wochenganglinie)[names(Wochenganglinie) == "Zaehlstelle.y"] <- "Zaehlstelle_Kurzname"
  
  Wochenganglinie <- subset(Wochenganglinie, select= -x) # delete not needed column
  
  ##########################
  ### Filtering the data ###
  ##########################
  
  Wochenganglinie <- subset(Wochenganglinie, !is.na(Zaehlung)) # remove rows without count data 
  
  ### Remove vacation days, holidays and days between holidays and weekends ###
  Wochenganglinie <- subset(Wochenganglinie, Tagtyp != "Feiertag" & Tagtyp != "Brückentag")
  Wochenganglinie <- subset(Wochenganglinie, Tagtyp != "Ferientag")
  
  ############################################
  ######### Filter by  weather data ##########
  ############################################
  
  ###  Average daily temperature of over 10°C ###
    Wochenganglinie <- subset(Wochenganglinie, Temperatur_C >= 10      ) 
  Wochenganglinie <- subset(Wochenganglinie, !is.na(Temperatur_C))  # remove rows without data for average daily temperature
  
  ### Wind speed (not filtered) ###
  # Wochenganglinie <- subset(Wochenganglinie, Wind_kmh <=17.99182) # 17.99182
  # Wochenganglinie <- subset(Wochenganglinie, !is.na(Wind_kmh))
  
  ### Humidity (not filtered)  ###
  # Wochenganglinie <- subset(Wochenganglinie, Luftfeuchtigkeit_Prozent <= 80.29895)
  # Wochenganglinie <- subset(Wochenganglinie, !is.na(Luftfeuchtigkeit_Prozent))
  
  ### Snow fall ###
  Wochenganglinie <- subset(Wochenganglinie, Schnee_cm == 0) # remove days with snow fall
  
  #####################################################################################
  ### Remove outlier with unexpected high or low count data with "Adjusted Boxplot" ###
  #####################################################################################
  
  # Load package if required
  if(!require('mrfDepth')) {
    install.packages('mrfDepth')
    library('mrfDepth')
  }
  
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

   # Use function of Adjusted Boxplot on data set to remove outlier
   Wochenganglinie <- Wochenganglinie %>%
   group_by(Zaehlstelle, Wochentag)%>%
   mutate(Zaehlung_gefiltert = remove_outliers_medcouple(Zaehlung))
   Wochenganglinie <- subset(Wochenganglinie, !is.na(Zaehlung_gefiltert))
 
  ##############################################
  ### Filter by the median of the count data ###
  ############################################## 
  
  ### Remove counting stations with a median of counts under 10 over the whole count data ###
   
  # Calculate median of the count data for each counting station (over the whole count data)
  library(dplyr)
  Nullwerte <- Wochenganglinie %>%
    group_by(Zaehlstelle) %>%
    summarise(
      Median = median(Zaehlung))
  
  Nullwerte_Ausreißer <- Nullwerte # copy data
  Nullwerte <- subset(Nullwerte, Median > 10)  # remove counting stations with a median under 10 over all days of the week
  Nullwerte_Zaehlstellen <- data.frame(Zaehlstelle = unique(Nullwerte$Zaehlstelle))
  
  Wochenganglinie <- subset(Wochenganglinie, Zaehlstelle %in% Nullwerte_Zaehlstellen$Zaehlstelle ) # remove counting stations which not fulfill the criteria
  
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
  Wochentage_Zaehlstellen <- subset(Wochentage_Zaehlstellen, count == 7)
  Wochenganglinie <- subset(Wochenganglinie, Zaehlstelle %in% Wochentage_Zaehlstellen$Zaehlstelle)
  }

Tageszahl <- Wochenganglinie %>%
  group_by(Datum) %>%
  reframe(Anzahl = n())

DZS_ZAHL <- Wochenganglinie %>%
  group_by(Zaehlstelle) %>%
  reframe(Anzahl = n())

#######################################################################################
### Determine highest amount of counting stations with count data on the same dates ###
#######################################################################################

# Copy data for analysis
Hilfsdataframe<- Wochenganglinie 
Hilfsdataframe2 <- Wochenganglinie
Zählstellen4 <- Wochenganglinie

ergebnisse <- data.frame(Datum = character(), Anzahl = numeric()) # set data frame for results

# Loop to find the highest number of counting stations with count data on the same dates 
for (i in 1:300) { 

  # filter counting stations which have count data on all so far determined dates
  Tagesdatensatz <- subset(Hilfsdataframe, Zaehlstelle %in% Zählstellen4$Zaehlstelle) 
  
  # determine number of counting stations per date
  Tageszahl <- Tagesdatensatz %>%
    group_by(Datum) %>%
    summarize(Anzahl = n())
  
  Tageszahl <- subset(Tageszahl, !Datum %in% ergebnisse$Datum) # remove days which are already in the result
  
  # Find date with highest amount of counting stations
  Max_Tageszahl <- Tageszahl[Tageszahl$Anzahl == max(Tageszahl$Anzahl, na.rm = TRUE), ]
  Max_Tageszahl <- Max_Tageszahl[1:1,] 
  
  # extract counting stations which have count data at this date 
  Zählstellen4 <- subset(Hilfsdataframe2, Datum %in% Max_Tageszahl$Datum) 
  
  # remove counting stations which don't have a value on this date
  Hilfsdataframe2 <- subset(Hilfsdataframe2, Zaehlstelle %in% Zählstellen4$Zaehlstelle)
  
  # Add the day and the amount counting stations to the data frame "ergebnissw"
  ergebnisse <- rbind(ergebnisse, Max_Tageszahl)
}

ergebnisse$Reihennummer <- 1:300 # add column with row number 
order1 <- c("Reihennummer", "Anzahl", "Datum") # set order
ergebnisse <- ergebnisse[, order1] # change order of columns 

rm(Hilfsdataframe, Zählstellen4, Hilfsdataframe2) # remove not needed data

################
### PLOTTING ###
################

library(ggplot2)

# Create plot
ggplot(ergebnisse, aes(x = as.numeric(Anzahl), y = as.numeric(Reihennummer))) +
  geom_line(linetype = "solid", size = 1.2, color = "black") +
  labs(title = "Number of counting stations and shared counting dates", 
       x = "Number of counting stations", 
       y = "Number of shared counting dates") +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        legend.text = element_text(size=13),
        title = element_text(size=13),
        axis.title = element_text(size=13),
        axis.text = element_text(size=11),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.75),
        axis.line = element_line(color = "black"))+
        scale_y_continuous(limits = c(0, 320), breaks = seq(0, 320, by = 50))+
        scale_x_continuous(limits = c(0, 470), breaks = seq(0, 470, by = 50))+
        geom_segment(aes(x = 0, xend = 362, y =  50, yend = 50), linetype = "dashed", color = "black", size = 0.8) +
        geom_segment(aes(x = 362, xend = 362, y = 0 , yend = 50),
        linetype = "dashed", color = "black", size = 0.8) +
        geom_text(x = 365, y = 60, label = "362 counting stations\
            50 shared dates", vjust = -0.5, hjust = 0.5, size= 3.75, color = "black")

# Save plot as PNG
ggsave(file = "C:/Leon/Studium/Master/Masterarbeit/Rscripte_Masterarbeit_Leon_Weyandt clean/Clustering/figures/counting_stations_with_shared_dates.png",
       width = 17, height = 10.5, units = "cm", dpi = 300)

# remove not needed data
rm(  i,  order1,  ergebnisse, remove_outliers_medcouple, Max_Tageszahl, Tagesdatensatz, Tageszahl, Wochentage_Ausreißer,
   Wochentage_Zaehlstellen, Nullwerte_Ausreißer, Wochentage, DZS_ZAHL, Wochenganglinie )


