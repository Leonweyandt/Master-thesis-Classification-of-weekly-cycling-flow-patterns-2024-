###########################################################
###########  Load data of the counting station ############
###########################################################

### Add excel file with count data (paths have to be adjusted) ###
library(readxl)
Alle_Tagesdaten_Alle  <- "C:\\Users\\leonw\\OneDrive - Hochschule RheinMain\\Rscripte_Masterarbeit_Leon_Weyandt\\Clustering\\Excel\\202311116_Alle_Richtungsdaten_unvalid and valid.xlsx"
Alle <- data.frame(read_excel(Alle_Tagesdaten_Alle , skip=3)) # read excel file, skip first 3 rows
 
### Add excel file with general information to counting stations (paths have to be adjusted)  ###
Zusatzinfos  <- "C:\\Users\\leonw\\OneDrive - Hochschule RheinMain\\Rscripte_Masterarbeit_Leon_Weyandt\\Clustering\\Excel\\Alle_Grundinfos.xlsx"
DZS_Zusatzinfos <-data.frame(read_excel(Zusatzinfos , skip=0)) # read excel file without skipping rows

### Renaming fields of data frame "DZS_Zusatzinfos" ###

# Find index where the field "Name..279." is "Steinbacher Straße" 
index <- which(DZS_Zusatzinfos$Name..279. == "Steinbacher Straße")
# Changing value 
DZS_Zusatzinfos$Name..279.[index] <- "999"
DZS_Zusatzinfos$Urspr..Zählstellenname[index] <- "Steinbacher Straße"

# Find index where the field "Name..279." is "Leipziger Straße"
index <- which(DZS_Zusatzinfos$Name..279. == "Leipziger Straße")
# Changing value 
DZS_Zusatzinfos$Name..279.[index] <- "750"
DZS_Zusatzinfos$Urspr..Zählstellenname[index] <- "Leipziger Straße"

# Set prefix "DZS_" in the data frame "DZS_Zusatzinfo"
DZS_Zusatzinfos$Name..279. <- paste0("DZS_", DZS_Zusatzinfos$Name..279.)

# change column name to "Kurzname"
colnames(DZS_Zusatzinfos)[colnames(DZS_Zusatzinfos) == "Name..279."] <- "Kurzname"  

rm(Zusatzinfos, i, index) # remove not needed data

### Replace characters that cant be used in R ###
library(dplyr)

# replace . with _
Alle <- Alle %>%
  rename_all(~ gsub("\\.", "_", .))

# replace Ä with ae 
Alle <- Alle %>%
  rename_all(~ gsub("ä", "ae", .))

# replace space with _ 
Alle <- Alle %>%
  rename_all(~ gsub(" ", "_", .))

# remove brackets ()
Alle <- Alle %>%
  rename_all(~ gsub(")", "", .))
Alle <- Alle %>%
  rename_all(~ gsub("\\(", "", .))

# Remove slash /
Alle <- Alle %>%
  rename_all(~ gsub("/", "", .))


### Set the prefix "DZS_" for the column names of the counting stations in the data frame "Alle" ###

# Find out the number of columns in data frame
anzahl_spalten <- ncol(Alle)

# Loop and rename columns
for (i in 2:anzahl_spalten) {
  colnames(Alle)[i] <- gsub("^X", "DZS_", colnames(Alle)[i]) }

rm(i, anzahl_spalten) # Remove not needed data

####### Write counting station in rows instead of columns ########

# Transforming the columns starting with "DZS_" from wide to long format
library(tidyr)
Alle <- Alle %>%
  pivot_longer(cols = starts_with("DZS_"), 
               names_to = "Zaehlstelle", 
               values_to = "Zaehlung")

Alle <- subset(Alle,   !is.na(Zaehlung)) # remove NA-values

# Create column "Kurzname" with a short name for the counting stations
Alle$Kurzname <- sub("^([^_]*_[^_]*).*", "\\1", Alle$Zaehlstelle) 

# Create columns with different time formats
Alle$Jahr       <- format(Alle$Kanalname, "%Y") # year
Alle$Monat      <- format(Alle$Kanalname, "%m") # month
Alle$Tag        <- format(Alle$Kanalname, "%d") # day
Alle$Wochentag  <- weekdays(Alle$Kanalname)      # weekday
Alle$Datum      <- format(Alle$Kanalname, "%Y-%m-%d")  # date
Alle <- subset(Alle, select= - Kanalname)              # remove column "Kanalname" 
library(lubridate)
Alle$Kalenderwoche <- week(Alle$Datum)                # calendar week
Alle$Datum <- as.Date(Alle$Datum)                     # convert date to date format

######## Create column "Tagtyp" with different day types ########

Alle$Datum <- as.Date(Alle$Datum)

# Create column "Tagtyp" with the day types weekdays ("Werktag"), saturday and sunday
Alle$Tagtyp <- ifelse(weekdays(Alle$Datum) %in% c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag"), "Werktag", weekdays(Alle$Datum))

# Check if date is on a holiday ("Feiertag")
Alle$Tagtyp[Alle$Datum %in% Feiertage$Datum] <- "Feiertag"

# Check if date is on a days between holidays and weekends ("Brückentag")
Alle$Tagtyp[Alle$Datum %in% Brückentage$Datum] <- "Brückentag"

# Check if date is in a vacation period ("Ferientag")
Alle$Tagtyp[Alle$Datum %in% Ferien$Datum] <- "Ferientag"

# Add general information from the file "DZS_Zusatzinfos"
Alle <- left_join(Alle, select(DZS_Zusatzinfos, Kurzname, Domain, Art), by = "Kurzname")

rm(DZS_Zusatzinfos) # remove not needed data


## Find counting stations with several canals and join them  ##

{
  Alle1 <- subset(Alle, !Zaehlstelle %in% c("DZS_354__Bike_IN_","DZS_354__Bike_OUT_")) #  remove counting stations without data
  Zaehlstellenanzahl <-data.frame(Zaehlstelle = unique(Alle1$Zaehlstelle)) # count number of counting stations
  
  # get unique names of counting stations
  Alle_rueber <- Alle1[!duplicated(Alle1$Zaehlstelle), ]
  Zaehlstellenanzahl$Kurzname <- Alle_rueber$Kurzname
  
  # determine number of canals per counting station
  library(dplyr)
    multibleQuerschnitte <- Alle_rueber %>% 
    group_by(Kurzname) %>%
    reframe(Anzahl_Querschnitte = n()/2,
            Zaehlstelle = Zaehlstelle)
  
  multibleQuerschnitte <- subset(multibleQuerschnitte, Anzahl_Querschnitte > 1) # counting stations with several canals
  Alle_nichtmult <-subset(Alle, !Kurzname %in% multibleQuerschnitte$Kurzname)  # counting stations with one canal
  
  Alle_mult <- subset(Alle, Kurzname %in% multibleQuerschnitte$Kurzname) # counting stations with several canals
  Alle_mult <- merge(Alle_mult, multibleQuerschnitte, by= c("Kurzname", "Zaehlstelle"))
  
  # Rename canals to later join the canals which belong to one counting station
  
  Alle_mult$Zaehlstelle[Alle_mult$Zaehlstelle == "DZS_119a_Fahrraeder_Richtung_Frankfurt_Zentrum"] <- "DZS_119a_Fahrraeder_Richtung_Frankfurt_Zentrum_1"
  Alle_mult$Zaehlstelle[Alle_mult$Zaehlstelle == "DZS_119a_Fahrraeder_Frankfurt_Zentrum"] <- "DZS_119a_Fahrraeder_Richtung_Frankfurt_Zentrum_2"
  
  Alle_mult$Zaehlstelle[Alle_mult$Zaehlstelle == "DZS_900_Channel_1_IN"] <- "DZS_900_Channel_1_IN_1"
  Alle_mult$Zaehlstelle[Alle_mult$Zaehlstelle == "DZS_900_Channel_3_IN"] <- "DZS_900_Channel_1_IN_2"
  Alle_mult$Zaehlstelle[Alle_mult$Zaehlstelle == "DZS_900_Channel_2_OUT"] <- "DZS_900_Channel_2_OUT_1"
  Alle_mult$Zaehlstelle[Alle_mult$Zaehlstelle == "DZS_900_Channel_4_OUT"] <- "DZS_900_Channel_2_OUT_2"
  
  Alle_mult$Zaehlstelle[Alle_mult$Zaehlstelle == "DZS_125_Fahrraeder_Richtung_Eilhausen_Kohlgrund__Bad_Arolsen_"] <- "DZS_125_Fahrraeder_Richtung_Eilhausen_Kohlgrund__Bad_Arolsen_1"
  Alle_mult$Zaehlstelle[Alle_mult$Zaehlstelle == "DZS_125__Bike_IN____198"] <- "DZS_125_Fahrraeder_Richtung_Eilhausen_Kohlgrund__Bad_Arolsen_2"
  Alle_mult$Zaehlstelle[Alle_mult$Zaehlstelle == "DZS_125__Bike_IN____199"] <- "DZS_125_Fahrraeder_Richtung_Eilhausen_Kohlgrund__Bad_Arolsen_3"
  Alle_mult$Zaehlstelle[Alle_mult$Zaehlstelle == "DZS_125__Bike_IN____200"] <- "DZS_125_Fahrraeder_Richtung_Eilhausen_Kohlgrund__Bad_Arolsen_4"
  Alle_mult$Zaehlstelle[Alle_mult$Zaehlstelle == "DZS_125_Fahrraeder_Richtung_Bad_Arolsen"] <- "DZS_125_Fahrraeder_Richtung_Bad_Arolsen_1"
  Alle_mult$Zaehlstelle[Alle_mult$Zaehlstelle == "DZS_125__Bike_OUT____201"] <- "DZS_125_Fahrraeder_Richtung_Bad_Arolsen_2"
  Alle_mult$Zaehlstelle[Alle_mult$Zaehlstelle == "DZS_125__Bike_OUT____202"] <- "DZS_125_Fahrraeder_Richtung_Bad_Arolsen_3"
  Alle_mult$Zaehlstelle[Alle_mult$Zaehlstelle == "DZS_125__Bike_OUT____203"] <- "DZS_125_Fahrraeder_Richtung_Bad_Arolsen_4"
  
  Alle_mult$Zaehlstelle[Alle_mult$Zaehlstelle == "DZS_139_Fahrraeder_Richtung_Hattersheim_am_Main"] <- "DZS_139_Fahrraeder_Richtung_Hattersheim_am_Main_1"
  Alle_mult$Zaehlstelle[Alle_mult$Zaehlstelle == "DZS_139__Bike_IN____350"] <- "DZS_139_Fahrraeder_Richtung_Hattersheim_am_Main_2"
  Alle_mult$Zaehlstelle[Alle_mult$Zaehlstelle == "DZS_139__Bike_IN____351"] <- "DZS_139_Fahrraeder_Richtung_Hattersheim_am_Main_3"
  Alle_mult$Zaehlstelle[Alle_mult$Zaehlstelle == "DZS_139__Bike_IN____352"] <- "DZS_139_Fahrraeder_Richtung_Hattersheim_am_Main_4"
  Alle_mult$Zaehlstelle[Alle_mult$Zaehlstelle == "DZS_139_Fahrraeder_Richtung_Kriftel"] <- "DZS_139_Fahrraeder_Richtung_Kriftel_1"
  Alle_mult$Zaehlstelle[Alle_mult$Zaehlstelle == "DZS_139__Bike_OUT____353"] <- "DZS_139_Fahrraeder_Richtung_Kriftel_2"
  Alle_mult$Zaehlstelle[Alle_mult$Zaehlstelle == "DZS_139__Bike_OUT____354"] <- "DZS_139_Fahrraeder_Richtung_Kriftel_3"
  Alle_mult$Zaehlstelle[Alle_mult$Zaehlstelle == "DZS_139__Bike_OUT____355"] <- "DZS_139_Fahrraeder_Richtung_Kriftel_4"
  
  Alle_mult$Zaehlstelle[Alle_mult$Zaehlstelle == "DZS_574_Fahrraeder_Richtung_Klinikum_Wetzlar"] <- "DZS_574_Fahrraeder_Richtung_Klinikum_Wetzlar_1"
  Alle_mult$Zaehlstelle[Alle_mult$Zaehlstelle == "DZS_574__Bike_IN_"] <- "DZS_574_Fahrraeder_Richtung_Klinikum_Wetzlar_2"
  Alle_mult$Zaehlstelle[Alle_mult$Zaehlstelle == "DZS_574_Fahrraeder_Richtung_Zentrum__Wetzlar_"] <- "DZS_574_Fahrraeder_Richtung_Zentrum__Wetzlar_1"
  Alle_mult$Zaehlstelle[Alle_mult$Zaehlstelle == "DZS_574__Bike_OUT_"] <- "DZS_574_Fahrraeder_Richtung_Zentrum__Wetzlar_2"
  
  Alle_mult$Zaehlstelle[Alle_mult$Zaehlstelle == "DZS_999_Steinbacher_Straße_Ri__Steinbach"] <- "DZS_999_Steinbacher_Straße_Ri__Steinbach_1"
  Alle_mult$Zaehlstelle[Alle_mult$Zaehlstelle == "DZS_999_Steinbacher_Straße_Ri__Schwalbach"] <- "DZS_999_Steinbacher_Straße_Ri__Schwalbach_1"
  Alle_mult$Zaehlstelle[Alle_mult$Zaehlstelle == "DZS_999_Steinbacher_Straße__Bike_IN_"] <- "DZS_999_Steinbacher_Straße_Ri__Steinbach_2"
  Alle_mult$Zaehlstelle[Alle_mult$Zaehlstelle == "DZS_999_Steinbacher_Straße__Bike_OUT_"] <- "DZS_999_Steinbacher_Straße_Ri__Schwalbach_2"
  
  # Function to remove the last part of the name of the counting stations which is separated with an underscore _ 
    extract_parts_before_last <- function(text) {
    parts <- unlist(strsplit(text, "_"))   # Split strings at the underscore _
    parts_before_last <- head(parts, -1)    # Extract all parts except the last 
    result <- paste(parts_before_last, collapse = "_")  # Join the parts with underscore _
    return(result)                         # Give back the altered string
    }
  
  Alle_mult <- Alle_mult %>%
    mutate(Zaehlstelle = sapply(Zaehlstelle, extract_parts_before_last))
  
  # Determine the number of canals for each day
  Alle_mult <- Alle_mult %>%
    group_by(Zaehlstelle, Datum) %>%
    mutate(Tagesbestimmen = n())}

  # Filter data by the days in which all canals have data
  Alle_mult <- subset(Alle_mult, Tagesbestimmen == Anzahl_Querschnitte) 

  # Sum data of all canals in one direction of a counting station   
  Alle_mult <- Alle_mult %>%
    group_by(Zaehlstelle, Datum) %>%
    mutate(Zaehlung = sum(Zaehlung)) %>%
    distinct()

  # remove of not needed columns
  Alle_mult<- subset(Alle_mult, select = -c(Anzahl_Querschnitte, Tagesbestimmen))
  
  # join data with counting stations with one or several canals
  Alle_summiert <- rbind(Alle_mult, Alle_nichtmult)

# Remove counting stations, which can't be used in the cluster analysis
Alle_summiert <- subset(Alle_summiert, !Kurzname %in% c("DZS_514a","DZS_514b","DZS_140a","DZS_140b"))
Alle_summiert <- subset(Alle_summiert, !Zaehlstelle %in% c("DZS_354__Bike_OUT_", "DZS_354__Bike_IN_"))

# Create "ID" for counting station which is used later in the cluster analysis #
Zaehlstellenanzahl_ID <- data.frame(Zaehlstelle = unique(Alle_summiert$Zaehlstelle)) # identify unique names of counting stations
Alle_rueber <- Alle_summiert[!duplicated(Alle_summiert$Zaehlstelle), ] # remove duplicates
Zaehlstellenanzahl_ID$Kurzname <- Alle_rueber$Kurzname # add "Kurzname" (short name) of the counting stations to data set

Zaehlstellenanzahl_ID<- Zaehlstellenanzahl_ID%>%  # create ID 
  arrange(Kurzname) %>%
  mutate(ID = row_number())

Zaehlstellenanzahl_ID$ID <- sprintf("%03d", Zaehlstellenanzahl_ID$ID) # format of ID 

# remove not needed data
rm(Alle_mult, Alle_nichtmult, Alle_rueber, Alle1, multibleQuerschnitte, Alle, Zaehlstellenanzahl, extract_parts_before_last)
