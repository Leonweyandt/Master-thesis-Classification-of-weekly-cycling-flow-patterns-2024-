##########################################################################
############## Load excel files of the daily weather data ################
##########################################################################

# Data path (has to be adjusted)
Wetterdaten_Taeglich <- "C:\\Users\\leonw\\OneDrive - Hochschule RheinMain\\Rscripte_Masterarbeit_Leon_Weyandt\\Clustering\\Excel\\Wetterdaten_Taeglich"

# List excel files
excel_dateien <- list.files(path = Wetterdaten_Taeglich, pattern = ".xlsx", full.names = TRUE)

########## Create list #############

tabellen_liste <- list() # Create empty list to save the data of the excel files

# Loop through excel files and read the weather data, skip first 2 rows and use line 3 as caption
library(readxl)
for (datei in excel_dateien) {
  daten <- read_excel(datei, skip = 2)
  daten <- daten[, 1:12]
  tabellen_liste[[tools::file_path_sans_ext(basename(datei))]] <- daten # Save data in list
}

###  Extract names of the counting stations and prepare them for prefixes of the weather ###

DZS_NAMEN_LISTE <- list()   # Create empty list for the names of the counting stations

# Loop through excel files and read them
for (datei in excel_dateien) {
  daten <- read_excel(datei, skip = 2)
  
  # Get names of counting stations of excel files
  zaehlstellenname <- daten[2, 13, drop = TRUE] 
  
  DZS_NAMEN_LISTE[[tools::file_path_sans_ext(basename(datei))]] <- zaehlstellenname
}

DZS_Namen_vector <- character(0) # Create empty vector to save the names of the counting stations

# Extract names of counting stations
for (i in seq_along(DZS_NAMEN_LISTE)) {
  DZS_Namen_vector <- c(DZS_Namen_vector, DZS_NAMEN_LISTE[[i]])
}

# Create data Frame with the names of the counting stations
DZS_Namen <- data.frame(DZS_Namen = DZS_Namen_vector)

### Format names of counting stations ###

# Get first 4 characters of the names of the counting stations
library(dplyr)
DZS_Namen$DZS_Namen <- substr(DZS_Namen$DZS_Namen, 1, 4)

# Function to remove the last character if its a number
removeLastDigitIfNumber <- function(text) {
  if (grepl("[0-9]$", text)) {
    return(substr(text, 1, nchar(text) - 1))
  } else {
    return(text)
  }
}

# Use function on the column "DZS_Namen" 
DZS_Namen$DZS_Namen <- sapply(DZS_Namen$DZS_Namen, removeLastDigitIfNumber)

## Create data set with daily weather data ("Wetterdaten_taeglich") ##

Wetterdaten_taeglich <- data.frame() # Create empty data frame for the weather data

# Loop through the tables in list
for (dateiname in names(tabellen_liste)) {
  tabelle <- tabellen_liste[[dateiname]]
  Wetterdaten_taeglich <- rbind(Wetterdaten_taeglich, tabelle) # Add data of the tables to the data frame
}

## Repeat the name of the counting station depending on the length of data of the corresponding excel file ##

laengen <- sapply(tabellen_liste, function(df) nrow(df)) # Determine length of the excel files

DZS_Namen$Multiplikator <- data.frame(Länge = laengen) # Create data frame with the column "Länge" and save the lengths there

DZS_Namen$Multiplikator <- as.numeric(unlist(DZS_Namen$Multiplikator)) # Extract the values and set them to numeric values

DATEN_ZEILE_ZÄHLSTELLEN <- data.frame(
  Name = rep(DZS_Namen$DZS_Namen, DZS_Namen$Multiplikator))

colnames(Wetterdaten_taeglich)[colnames(Wetterdaten_taeglich) == "Zeit"] <- "Datum" # Rename column "Zeit" to "Datum" 


####### Load values from the list  #############

Wetterdaten_taeglich$Zaehlstelle <- DATEN_ZEILE_ZÄHLSTELLEN$Name

## Set data types (numeric and date) ##

Wetterdaten_taeglich$Datum <- as.Date(as.numeric(Wetterdaten_taeglich$Datum) , origin = "1899-12-30")

Wetterdaten_taeglich$DZS <- as.numeric(Wetterdaten_taeglich$DZS)

Wetterdaten_taeglich$`Temperatur (°C)` <- as.numeric(Wetterdaten_taeglich$`Temperatur (°C)`)

Wetterdaten_taeglich$`Min. Temperatur (°C)` <- as.numeric(Wetterdaten_taeglich$`Min. Temperatur (°C)`)

Wetterdaten_taeglich$`Max. Temperatur (°C)` <- as.numeric(Wetterdaten_taeglich$`Max. Temperatur (°C)`)

Wetterdaten_taeglich$`Luftfeuchtigkeit (%)` <- as.numeric(Wetterdaten_taeglich$`Luftfeuchtigkeit (%)`)

Wetterdaten_taeglich$`Wolken (%)` <- as.numeric(Wetterdaten_taeglich$`Wolken (%)`)

Wetterdaten_taeglich$`Gefühlte Temperatur (°C)` <- as.numeric(Wetterdaten_taeglich$`Gefühlte Temperatur (°C)`)

Wetterdaten_taeglich$`Regen (mm)` <- as.numeric(Wetterdaten_taeglich$`Regen (mm)`)

Wetterdaten_taeglich$`Schnee (cm)` <- as.numeric(Wetterdaten_taeglich$`Schnee (cm)`)

Wetterdaten_taeglich$`Wind (km/h)` <- as.numeric(Wetterdaten_taeglich$`Wind (km/h)`)


# Change column names
Wetterdaten_taeglich$Zaehlstelle <- gsub("750\\.", "750", Wetterdaten_taeglich$Zaehlstelle)

colnames(Wetterdaten_taeglich)[colnames(Wetterdaten_taeglich) == "Temperatur (°C)"] <- "Temperatur_C"
colnames(Wetterdaten_taeglich)[colnames(Wetterdaten_taeglich) == "Luftfeuchtigkeit (%)"] <- "Luftfeuchtigkeit_Prozent"
colnames(Wetterdaten_taeglich)[colnames(Wetterdaten_taeglich) == "Regen (mm)"] <- "Regen_mm"
colnames(Wetterdaten_taeglich)[colnames(Wetterdaten_taeglich) == "Wind (km/h)"] <- "Wind_kmh"
colnames(Wetterdaten_taeglich)[colnames(Wetterdaten_taeglich) == "Schnee (cm)"] <- "Schnee_cm"

# remove not needed columns
Wetterdaten_taeglich <- subset(Wetterdaten_taeglich, select = - c(`Min. Temperatur (°C)`, `Max. Temperatur (°C)`, `Wolken (%)`, `Symbol Wetter`))

# Remove not needed data
rm(daten, DATEN_ZEILE_ZÄHLSTELLEN, DZS_Namen, DZS_NAMEN_LISTE, DZS_Namen_vector, datei, dateiname,
   i, laengen, Wetterdaten_Taeglich, tabelle, tabellen_liste, excel_dateien, removeLastDigitIfNumber)
