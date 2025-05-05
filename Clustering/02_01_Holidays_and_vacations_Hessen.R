#######################################################
################ Holidays and vacations ###############
#######################################################

### Enter dates of holidays in Germany for the years 2022 and 2023 ###

feiertage <- list(
  "01.01.2022" = "Neujahrstag",
  "15.04.2022" = "Karfreitag",
  "18.04.2022" = "Ostermontag",
  "01.05.2022" = "Tag der Arbeit",
  "26.05.2022" = "Christi Himmelfahrt",
  "06.06.2022" = "Pfingstmontag",
  "16.06.2022" = "Fronleichnam",
  "03.10.2022" = "Tag der Deutschen Einheit",
  "25.12.2022" = "1. Weihnachtstag",
  "26.12.2022" = "2. Weihnachtstag",
  "01.01.2023" = "Neujahr",
  "07.04.2023" = "Karfreitag",
  "10.04.2023" = "Ostermontag",
  "01.05.2023" = "Tag der Arbeit",
  "18.05.2023" = "Christi Himmelfahrt",
  "29.05.2023" = "Pfingstmontag",
  "08.06.2023" = "Fronleichnam",
  "03.10.2023" = "Tag der Deutschen Einheit",
  "25.12.2023" = "1. Weihnachtstag",
  "26.12.2023" = "2. Weihnachtstag")

# Convert list to data frame
Feiertage <- data.frame(
  Datum = as.Date(names(feiertage), format = "%d.%m.%Y"),
  Feiertag = unlist(feiertage))

library(lubridate)
Feiertage$Wochentag <- weekdays(Feiertage$Datum) # find out the day of the week through the date

rm(feiertage) # remove not needed data

## Enter dates of the days between holidays and weekends (mondays and fridays) for the years 2022 and 2023 ##

Brückentage <- list(
  "02.10.2023" = "Tag der Deutschen Einheit",
  "27.05.2022" = "Christi Himmelfahrt",
  "17.06.2022" = "Fronleichnam",
  "19.05.2023" = "Christi Himmelfahrt",
  "09.06.2022" = "Fronleichnam")
  
# Convert list to data frame
Brückentage <- data.frame(
  Datum = as.Date(names(Brückentage), format = "%d.%m.%Y"),
  Feiertag = unlist(Brückentage))

library(lubridate)
Brückentage$Wochentag <- weekdays(Brückentage$Datum) # find out the day of the week through the date 


######### Vacation periods in Hessen for the years 2022 and 2023  #########

# Create data frame with the start and end dates and the names of the vacation periods 
ferienzeiträume <- data.frame(
  Startdatum = as.Date(c(
    "2022-01-01", "2022-03-09", "2022-07-23", "2022-10-22", "2022-12-22",
    "2023-04-01", "2023-07-22", "2023-10-21", "2023-12-27")),
  Enddatum = as.Date(c("2022-01-09", "2022-03-24", "2022-09-04", "2022-10-30", "2023-01-06",
    "2023-04-23", "2023-08-31", "2023-10-29", "2023-12-31")),
  Ferienbeschreibung = c( "Winterferien 2022", "Osterferien 2022", "Sommerferien 2022", "Herbstferien 2022", "Winterferien 2022/23",
    "Osterferien 2023", "Sommerferien 2023", "Herbstferien 2023", "Winterferien 2023/24"))

# Create data frame for vacation periods
Ferien <- data.frame(
  Datum = character(),
  Ferienbeschreibung = character(),
  stringsAsFactors = FALSE)

# Write all days of each vacation period into the table
for (i in 1:nrow(ferienzeiträume)) {
  Ferien <- rbind(Ferien, data.frame(
    Datum = seq(ferienzeiträume$Startdatum[i], ferienzeiträume$Enddatum[i], by = "1 day"),
    Ferienbeschreibung = ferienzeiträume$Ferienbeschreibung[i])) }

Ferien$Datum <- as.Date(Ferien$Datum) # Set to date format

rm(ferienzeiträume, i) # remove not needed data
