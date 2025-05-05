################################################
#########  Analyze daily weather data  #########
################################################

Bearbeitungstabelle <- Wetterdaten_taeglich # Create copy of data set to work on

####################
#### Clean data ####
####################

Bearbeitungstabelle <- subset(Bearbeitungstabelle, !is.na(Bearbeitungstabelle$DZS)) # Remove days without count data

Bearbeitungstabelle$Wochentag <- weekdays(Bearbeitungstabelle$Datum) # get days of the week from date value

# Remove outliers with adjusted boxplot
if(!require('mrfDepth')) {
  install.packages('mrfDepth')
  library('mrfDepth')
}

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
library(dplyr)
Bearbeitungstabelle <- Bearbeitungstabelle %>%
  group_by(Zaehlstelle, Wochentag)%>%
  mutate(Zaehlung_gefiltert = remove_outliers_medcouple(DZS))

Bearbeitungstabelle <- subset(Bearbeitungstabelle, !is.na(Zaehlung_gefiltert)) # remove outliers (got put to NA with funktion "remove_outliers_medcouple")

# Set 0 to float 0.0
#Bearbeitungstabelle$Regen_mm[Bearbeitungstabelle$Regen_mm == 0.0] <- 0
#Bearbeitungstabelle$Temperatur_C[Bearbeitungstabelle$Temperatur_C == 0] <- 0.0
#Bearbeitungstabelle$Luftfeuchtigkeit_Prozent[Bearbeitungstabelle$Luftfeuchtigkeit_Prozent == 0] <- 0.0
#Bearbeitungstabelle$Wind_kmh[Bearbeitungstabelle$Wind_kmh == 0] <- 0.0

###########################
###########################
######## Plotting #########
###########################
###########################

###############################################################
### Relationship average daily temperature ~ traffic volume ###
###############################################################

library(dplyr)

#### Average daily counts for each counting station ####
Durchschnitt_Temp <- Bearbeitungstabelle %>%
  group_by(Zaehlstelle) %>%
  summarise(Durchschnitt_Gesamt = mean(as.numeric(DZS)))

# Select column "Temperatur_C"
Bearbeitungstabelle_select <- select(Bearbeitungstabelle, Temperatur_C, DZS,  Zaehlstelle)

# Join table "Durchschnitt_Temp" with table "Bearbeitungstabelle_select" through the column "Zaehlstelle"
Durchschnitt_Temp <- left_join(Durchschnitt_Temp,
                          Bearbeitungstabelle_select, by = "Zaehlstelle")
as.numeric(Durchschnitt_Temp$DZS)  # set data type

# Ratio: a daily count value of a counting station / average count of one counting station
Durchschnitt_Temp$Verhältnis <- as.numeric(Durchschnitt_Temp$DZS)/as.numeric(Durchschnitt_Temp$Durchschnitt_Gesamt)
Durchschnitt_Temp$RoundedVerhältnis <- round(Durchschnitt_Temp$Verhältnis, digits = 2) # round
Durchschnitt_Temp <- subset(Durchschnitt_Temp, !is.na(Temperatur_C)) # remove days without data for daily average temperature

# Set temperature ranges
Durchschnitt_Temp$Temperaturbereich <-ifelse(Durchschnitt_Temp$Temperatur_C > -10 & Durchschnitt_Temp$Temperatur_C <= -5, "-10 to -5",
                                        ifelse(Durchschnitt_Temp$Temperatur_C > -5 & Durchschnitt_Temp$Temperatur_C <= 0, "-5 to 0",
                                          ifelse(Durchschnitt_Temp$Temperatur_C > 0 & Durchschnitt_Temp$Temperatur_C <= 5, "0 to 5",
                                            ifelse(Durchschnitt_Temp$Temperatur_C > 5 & Durchschnitt_Temp$Temperatur_C <= 10, "5 to 10",
                                              ifelse(Durchschnitt_Temp$Temperatur_C > 10 & Durchschnitt_Temp$Temperatur_C <= 15, "10 to 15",
                                                ifelse(Durchschnitt_Temp$Temperatur_C > 15 & Durchschnitt_Temp$Temperatur_C <= 20, "15 to 20",
                                                  ifelse(Durchschnitt_Temp$Temperatur_C > 20 & Durchschnitt_Temp$Temperatur_C <= 25, "20 to 25",
                                                    ifelse(Durchschnitt_Temp$Temperatur_C > 25 & Durchschnitt_Temp$Temperatur_C <= 30, "25 to 30",
                                                      ifelse(Durchschnitt_Temp$Temperatur_C > 30, "30 to 35","ah")))))))))

# Set factor levels
factor_levels <- c( "-10 to -5", "-5 to 0", "0 to 5", "5 to 10", "10 to 15", "15 to 20", "20 to 25", "25 to 30", "30 to 35", "ah")

# Apply factor levels
Durchschnitt_Temp$Temperaturbereich <- factor(Durchschnitt_Temp$Temperaturbereich, levels = factor_levels)

# Create boxplot: average daily temperature ~ traffic volume
library(ggplot2)
plottemp <-  ggplot(data = Durchschnitt_Temp, aes(x = Temperaturbereich , 
                                                   y = as.numeric(RoundedVerhältnis)))+
  labs(title="Standardized traffic volume dependent on the average daily temperature",
       x = "Average daily temperature in °C", y = "Standardized traffic volume")+
  geom_boxplot(linewidth = 0.5 )+
  scale_y_continuous(breaks = seq(0, 12, 0.25), labels = function(x) ifelse(x %% 0.5 == 0, as.character(x), ""))+
  theme_classic() +
  theme(
    plot.title = element_text(size = 20, face = "bold", margin = margin(b = 10)), # set bottom margin
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 20),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    plot.margin = margin(t = 10, unit = "pt")  # set top margin
    ) +
   theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) +
  #                   sec.axis = sec_axis(trans = ~./1.17   , breaks = seq(0, 1, by = 0.25),
  #                                       name = "adjusted to 100 percent"))+
    coord_cartesian(ylim = c(0, 7))

# Set folder to save the plot 
file_path <- file.path("C:/Leon/Studium/Master/Masterarbeit/Rscripte_Masterarbeit_Leon_Weyandt clean/Clustering/weather_figures", "Boxplot_average_daily_temperature.png")

# Save plot
ggsave(plottemp, file = file_path, dpi = 800, width = 13.5, height =7.5 , units = "in")

rm(TEMPERATUR, plottemp, Durchschnitt_Temp) # remove not needed data

######################################################################
######## Relationship amount of snow fall ~ traffic volume ###########
######################################################################

library(dplyr)

#### Average daily counts at one counting station ####
Durchschnitt_Schnee <- Bearbeitungstabelle%>%
  group_by(Zaehlstelle) %>%
  summarise(Durchschnitt_Gesamt = mean(as.numeric(DZS)))

# Select column "Schnee_cm"
Bearbeitungstabelle_select <- select(Bearbeitungstabelle, Schnee_cm, DZS,  Zaehlstelle)

# Join table "Durchschnitt_Schnee" with table "Bearbeitungstabelle_select" through the column "Zaehlstelle"
Durchschnitt_Schnee <- left_join(Durchschnitt_Schnee,
                                 Bearbeitungstabelle_select, by = "Zaehlstelle")
as.numeric(Durchschnitt_Schnee$DZS)

# Ratio: a daily count value of a counting station / average count of one counting station
Durchschnitt_Schnee$Verhältnis <- as.numeric(Durchschnitt_Schnee$DZS)/as.numeric(Durchschnitt_Schnee$Durchschnitt_Gesamt)
Durchschnitt_Schnee$RoundedVerhältnis <-round(Durchschnitt_Schnee$Verhältnis, digits = 2) # round
rm(Bearbeitungstabelle_select) # delete not needed table
Durchschnitt_Schnee <- subset(Durchschnitt_Schnee, !is.na(Schnee_cm)) # remove days without data to amount of snow fall

# Set ranges for amount of snow fall
Durchschnitt_Schnee$Menge <-  ifelse(Durchschnitt_Schnee$Schnee_cm > 0 & Durchschnitt_Schnee$Schnee_cm <= 1, "0-1",
                                     ifelse(Durchschnitt_Schnee$Schnee_cm > 1 & Durchschnitt_Schnee$Schnee_cm <= 2, "1-2",
                                            ifelse(Durchschnitt_Schnee$Schnee_cm > 2 & Durchschnitt_Schnee$Schnee_cm <= 3, "2-3",
                                                   ifelse(Durchschnitt_Schnee$Schnee_cm > 3 & Durchschnitt_Schnee$Schnee_cm <= 4, "3-4",
                                                          ifelse(Durchschnitt_Schnee$Schnee_cm > 4 & Durchschnitt_Schnee$Schnee_cm <= 5, "4-5",
                                                                 #ifelse(Durchschnitt_Schnee$Schnee_cm > 25 & Durchschnitt_Schnee$Schnee_cm <= 30, "25-30",
                                                                 #ifelse(Durchschnitt_Schnee$Schnee_cm > 30 & Durchschnitt_Schnee$Schnee_cm <= 35, "30-35",
                                                                 ifelse(Durchschnitt_Schnee$Schnee_cm >5, ">5","0"))))))

# set factors
Durchschnitt_Schnee$Menge <- factor(Durchschnitt_Schnee$Menge, levels = c("0","0-1", "1-2", "2-3", "3-4","4-5", ">5"))

# Create boxplot amount of snow fall ~ traffic volume
library(ggplot2)
plotschnee <- ggplot(data = Durchschnitt_Schnee, aes(x = Menge , 
                                                   y = as.numeric(RoundedVerhältnis)))+
  geom_boxplot()+
   labs(title="Standardized traffic volume dependent on the amount of snow fall", 
       x = "Amount of snow fall in cm", y = "Standardized traffic volume")+
  scale_y_continuous(breaks = seq(0, 12, 0.25), labels = function(x) ifelse(x %% 0.5 == 0, as.character(x), ""))+
  
  theme_classic() +
  theme(
    plot.title = element_text(size = 20, face = "bold", margin = margin(b = 10)), # set bottom margin
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 20),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    plot.margin = margin(t = 10, unit = "pt")  # set top margin
    ) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))+
    coord_cartesian(ylim = c(0, 7))

# Set folder for saving the plot
file_path <- file.path("C:/Leon/Studium/Master/Masterarbeit/Rscripte_Masterarbeit_Leon_Weyandt clean/Clustering/weather_figures", "Boxplot__of_snow_fall.png")

# Save plot
ggsave(plotschnee, file = file_path, dpi = 800, width = 13.5, height =7.5 , units = "in")

rm(plotschnee, Durchschnitt_Schnee) # remove not needed data

#####################################################
###### Relationship humidity ~ traffic volume #######
#####################################################

library(dplyr)

#### Average daily counts at one counting station ####
Durchschnitt_Feucht <- Bearbeitungstabelle%>%
  group_by(Zaehlstelle) %>%
  summarise(Durchschnitt_Gesamt = mean(as.numeric(DZS)))

# Select column "Luftfeuchtigkeit_Prozent" 
Bearbeitungstabelle_select <- select(Bearbeitungstabelle, Luftfeuchtigkeit_Prozent , DZS,  Zaehlstelle )

# Join table "Durchschnitt_Feucht" with table "Bearbeitungstabelle_select" through the column "Zaehlstelle"

Durchschnitt_Feucht <- left_join(Durchschnitt_Feucht, Bearbeitungstabelle_select, by = "Zaehlstelle")
as.numeric(Durchschnitt_Feucht$DZS)

# Ratio: a daily count value of a counting station / average count of one counting station
Durchschnitt_Feucht$Verhältnis <- as.numeric(Durchschnitt_Feucht$DZS)/as.numeric(Durchschnitt_Feucht$Durchschnitt_Gesamt)
Durchschnitt_Feucht$RoundedVerhältnis <- round(Durchschnitt_Feucht$Verhältnis, digits = 2) # round
Durchschnitt_Feucht <- subset(Durchschnitt_Feucht, !is.na(Luftfeuchtigkeit_Prozent)) # remove days without data for humidity
rm(Bearbeitungstabelle_select) # remove not needed table

# Set range of values for humidity
Durchschnitt_Feucht$Menge <-  ifelse(Durchschnitt_Feucht$Luftfeuchtigkeit_Prozent > 26 & Durchschnitt_Feucht$Luftfeuchtigkeit_Prozent <= 40, "27-40",
                                    ifelse(Durchschnitt_Feucht$Luftfeuchtigkeit_Prozent > 40 & Durchschnitt_Feucht$Luftfeuchtigkeit_Prozent <= 50, "40-50",
                                           ifelse(Durchschnitt_Feucht$Luftfeuchtigkeit_Prozent >50 & Durchschnitt_Feucht$Luftfeuchtigkeit_Prozent <= 60, "50-60",
                                                  ifelse(Durchschnitt_Feucht$Luftfeuchtigkeit_Prozent >60 & Durchschnitt_Feucht$Luftfeuchtigkeit_Prozent <= 70, "60-70",
                                                         ifelse(Durchschnitt_Feucht$Luftfeuchtigkeit_Prozent >70 & Durchschnitt_Feucht$Luftfeuchtigkeit_Prozent <= 80, "70-80",
                                                                ifelse(Durchschnitt_Feucht$Luftfeuchtigkeit_Prozent >80 & Durchschnitt_Feucht$Luftfeuchtigkeit_Prozent <= 90, "80-90",
                                                                       ifelse(Durchschnitt_Feucht$Luftfeuchtigkeit_Prozent >90, "90-100","0-27")))))))

# set factors
Durchschnitt_Feucht$Menge <- factor(Durchschnitt_Feucht$Menge, 
                                    levels = c("27-40", "40-50", "50-60", "60-70", "70-80", "80-90","90-100"))

# Create boxplot: humidity ~ traffic volume
library(ggplot2)
plotfeucht <- ggplot(data = Durchschnitt_Feucht, aes(x = Menge , 
                                                     y = as.numeric(RoundedVerhältnis)))+
  geom_boxplot()+
  labs(title="Standardized traffic volume dependent on the humidity", 
  x = "Humidity in %", y = "Standardized traffic volume")+
  scale_y_continuous(breaks = seq(0, 12, 0.25), labels = function(x) ifelse(x %% 0.5 == 0, as.character(x), ""))+
  theme_classic() +
  theme(
    plot.title = element_text(size = 20, face = "bold", margin = margin(b = 10)), # set bottom margin
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 20),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    plot.margin = margin(t = 10, unit = "pt")  # set top margin
      ) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))+ 
  coord_cartesian(ylim = c(0, 7))

# Set folder for saving the plot
file_path <- file.path("C:/Leon/Studium/Master/Masterarbeit/Rscripte_Masterarbeit_Leon_Weyandt clean/Clustering/weather_figures", "Boxplot_humidity.png")

# Save plot
ggsave(plotfeucht, file = file_path, dpi = 800, width = 13.5, height =7.5 , units = "in")

rm(plotfeucht, Durchschnitt_Feucht) # remove not needed data

###############################################################
########  Relationship wind speed ~ traffic volumen ###########
###############################################################

library(dplyr)

#### Average daily counts at one counting station ####
Durchschnitt_Wind <- Bearbeitungstabelle %>%
  group_by(Zaehlstelle) %>%
  summarise(Durchschnitt_Gesamt = mean(as.numeric(DZS)))

library(dplyr)

# Spalte "Wind_kmh" aus der Bearbeitungstabelle auswählen
Bearbeitungstabelle_select <- select(Bearbeitungstabelle, Wind_kmh, DZS,  Zaehlstelle)

# Join table "Durchschnitt_Wind" with "Bearbeitungstabelle_select" through the column "Zaehlstelle"

Durchschnitt_Wind <- left_join(Durchschnitt_Wind,Bearbeitungstabelle_select, by = "Zaehlstelle")

as.numeric(Durchschnitt_Wind$DZS)
class(Durchschnitt_Wind$DZS)

# Ratio: a daily count value of a counting station / average count of one counting station
Durchschnitt_Wind$Verhältnis <- as.numeric(Durchschnitt_Wind$DZS)/as.numeric(Durchschnitt_Wind$Durchschnitt_Gesamt)

Durchschnitt_Wind$RoundedVerhältnis <-  round(Durchschnitt_Wind$Verhältnis, digits = 2) # round
rm(Bearbeitungstabelle_select) # remove not needed table
Durchschnitt_Wind <- subset(Durchschnitt_Wind, !is.na(Wind_kmh)) # remove days without data - wind speed

# Set ranges of wind speed
Durchschnitt_Wind$Menge <-  ifelse(Durchschnitt_Wind$Wind_kmh > 0 & Durchschnitt_Wind$Wind_kmh <= 5, "0-5",
                                    ifelse(Durchschnitt_Wind$Wind_kmh >5 & Durchschnitt_Wind$Wind_kmh <= 10, "5-10",
                                           ifelse(Durchschnitt_Wind$Wind_kmh >10 & Durchschnitt_Wind$Wind_kmh <= 15, "10-15",
                                                  ifelse(Durchschnitt_Wind$Wind_kmh >15 & Durchschnitt_Wind$Wind_kmh <= 20, "15-20",
                                                         ifelse(Durchschnitt_Wind$Wind_kmh >20 & Durchschnitt_Wind$Wind_kmh <= 25, "20-25",
                                                                 ifelse(Durchschnitt_Wind$Wind_kmh >25 & Durchschnitt_Wind$Wind_kmh <= 30, "25-30",
                                     ifelse(Durchschnitt_Wind$Wind_kmh >30, ">30","kein Wind")))))))

# set factors
Durchschnitt_Wind$Menge <- factor(Durchschnitt_Wind$Menge, levels = c("kein Wind", "0-5", "5-10", "10-15",
                                                                          "15-20", "20-25","25-30",">30"))

# Create boxplot  Windgeschwindigkeit - Verkehrsstärke
library(ggplot2)
plotwind <- ggplot(data = Durchschnitt_Wind, aes(x = Menge, y = as.numeric(RoundedVerhältnis)))+
  geom_boxplot()+
  labs(title="Standardized traffic volume dependent on the wind speed", 
  x = "Wind speed in km/h", y = "Standardized traffic volume")+
  scale_y_continuous(breaks = seq(0, 12, 0.25), labels = function(x) ifelse(x %% 0.5 == 0, as.character(x), ""))+
  theme_classic() +
  theme(
    plot.title = element_text(size = 20, face = "bold", margin = margin(b = 10)), # set bottom margin
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 20),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    plot.margin = margin(t = 10, unit = "pt")  # set top margin
  ) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))+ #  scale_y_continuous(breaks = seq(0, 5, by = 0.25),
  #                    sec.axis = sec_axis(trans = ~./1.17   , breaks = seq(0, 1, by = 0.25),
  #                                       name = "adjusted to 100 percent"))+
  coord_cartesian(ylim = c(0, 7))

# Set folder for saving the plot
file_path <- file.path("C:/Leon/Studium/Master/Masterarbeit/Rscripte_Masterarbeit_Leon_Weyandt clean/Clustering/weather_figures", "Boxplot_wind_speed.png")

# Save plot
ggsave(plotwind, file = file_path, dpi = 800, width = 13.5, height =7.5 , units = "in")

rm(plotwind, Durchschnitt_Wind)

##################################################################
########  Relationship amount of rain ~ traffic volumen ##########
##################################################################

library(dplyr)

#### Average daily counts at one counting station ####
Durchschnitt_Regen <- Bearbeitungstabelle %>%
  group_by(Zaehlstelle) %>%
  summarise(Durchschnitt_Gesamt = mean(as.numeric(DZS)))

library(dplyr)

# Select column "Regen_mm"
Bearbeitungstabelle_select <- select(Bearbeitungstabelle,Regen_mm, DZS,  Zaehlstelle )

# Join table "Durchschnitt_Regen" with table "Bearbeitungstabelle_select" through the column "Zaehlstelle"

Durchschnitt_Regen <- left_join(Durchschnitt_Regen,  Bearbeitungstabelle_select, by = "Zaehlstelle")
as.numeric(Durchschnitt_Regen$DZS)

# Remove days without data
Durchschnitt_Regen <- subset(Durchschnitt_Regen, !is.na(Regen_mm))
Durchschnitt_Regen <- subset(Durchschnitt_Regen, !is.na(DZS)) 

# Ratio: a daily count value of a counting station / average count of one counting station
Durchschnitt_Regen$Verhältnis <- as.numeric(Durchschnitt_Regen$DZS)/as.numeric(Durchschnitt_Regen$Durchschnitt_Gesamt)
Durchschnitt_Regen$RoundedVerhältnis <- round(Durchschnitt_Regen$Verhältnis, digits = 2) # round
rm(Bearbeitungstabelle_select) # remove not needed table

# Set ranges of amount of rain
Durchschnitt_Regen$Menge <-  ifelse(Durchschnitt_Regen$Regen_mm > 0 & Durchschnitt_Regen$Regen_mm <= 5, "0-5",
                                     ifelse(Durchschnitt_Regen$Regen_mm > 5 & Durchschnitt_Regen$Regen_mm <= 10, "5-10",
                                      ifelse(Durchschnitt_Regen$Regen_mm > 10 & Durchschnitt_Regen$Regen_mm <= 15, "10-15",
                                     ifelse(Durchschnitt_Regen$Regen_mm > 15 & Durchschnitt_Regen$Regen_mm <= 20, "15-20",
                                      ifelse(Durchschnitt_Regen$Regen_mm > 20 & Durchschnitt_Regen$Regen_mm <= 25, "20-25",
                                       #ifelse(Durchschnitt_Regen$Regen_mm > 25 & Durchschnitt_Regen$Regen_mm <= 30, "25-30",
                                       #ifelse(Durchschnitt_Regen$Regen_mm > 30 & Durchschnitt_Regen$Regen_mm <= 35, "30-35",
                                ifelse(Durchschnitt_Regen$Regen_mm >25, ">25","0"))))))

# set factors
Durchschnitt_Regen$Menge <- factor(Durchschnitt_Regen$Menge, levels = c("0","0-5", "5-10", "10-15", "15-20",
                                                                        "20-25", ">25"))

# Create boxplot: amount of rain ~ traffic volume
library(ggplot2)
plotregen <- ggplot(data = Durchschnitt_Regen, aes(x = Menge , y = as.numeric(RoundedVerhältnis)))+
  geom_boxplot()+
  labs(title="Standardized traffic volume dependent on the amount of rain", 
  x = "Amount of rain in mm", y = "Standardized traffic volume")+
  scale_y_continuous(breaks = seq(0, 12, 0.25), labels = function(x) ifelse(x %% 0.5 == 0, as.character(x), ""))+
  theme_classic() +
  theme(
    plot.title = element_text(size = 20, face = "bold", margin = margin(b = 10)), # set bottom margin
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 20),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    plot.margin = margin(t = 10, unit = "pt")  # set top margin
  ) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))+ #  scale_y_continuous(breaks = seq(0, 5, by = 0.25),
 #                    sec.axis = sec_axis(trans = ~./1.17   , breaks = seq(0, 1, by = 0.25),
  #                                       name = "adjusted to 100 percent"))+
    coord_cartesian(ylim = c(0, 7))

# Set folder for saving the plot
file_path <- file.path("C:/Leon/Studium/Master/Masterarbeit/Rscripte_Masterarbeit_Leon_Weyandt clean/Clustering/weather_figures", "Boxplot_amount_of_rain.png")

# Save plot
ggsave(plotregen, file = file_path, dpi = 800, width = 13.5, height =7.5 , units = "in")

# remove not needed data
rm(plotregen, Durchschnitt_Regen, Bearbeitungstabelle, remove_outliers_medcouple, factor_levels, file_path) 
