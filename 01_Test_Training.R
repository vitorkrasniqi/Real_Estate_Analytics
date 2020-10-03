
# Die benötigten Librarys werden importiert
library(readr)
library(tidyverse)

# Die benötigten Daten werden importiert

training <- read_csv("data/training.csv")


X_test <- read_csv("data/X_test.csv")







# Notiz an uns
# > dim(training)
# [1] 35000    44
# > dim(X_test)
# [1] 15000    43
# > dim(PLZO_CSV_LV95)
# [1] 4135    9
# > dim(Regionalportrait_original_2020)
# [1] 2218   42











training <- as.data.frame(training)
X_test <- as.data.frame(X_test)

set_factor <- c( "balcony",  "basement" , "bath_tube" ,"cabletv", "cheminee"  , "elevator", "first_time"  ,"furnished",  
                 "home_type", "kids_friendly","laundry","minergie","new_building","newly_built","oldbuilding",
                 "oven","parking_indoor","parking_outside","playground","pool","raised_groundfloor","rooms","sale","street"     
                 ,"terrace","topstorage","veranda","wheelchair", "building_plot",  "ceiling", "quiet", "sunny" , "municipality") 


# Sämtliche True False Spalten mit 1 0 ersetzen
training$building_plot <- as.integer(as.logical(training$building_plot))
training$ceiling <- as.integer(as.logical(training$ceiling))
training$quiet<- as.integer(as.logical(training$quiet))
training$sunny <- as.integer(as.logical(training$sunny))





# Sämtliche True False Spalten mit 1 0 ersetzen
X_test$building_plot <- as.integer(as.logical(X_test$building_plot))
X_test$ceiling <- as.integer(as.logical(X_test$ceiling))
X_test$quiet<- as.integer(as.logical(X_test$quiet))
X_test$sunny <- as.integer(as.logical(X_test$sunny))

# NAs mit 0 ersetzen
X_test[is.na(X_test)] = 0
training[is.na(training)] = 0





#  Nun kann die Umwandlung in Factor stattfinden
training[,set_factor] <- lapply(training[,set_factor], factor) 
X_test[,set_factor] <- lapply(X_test[,set_factor], factor) 





# Den vorhergesagten Wert als Zahl umwandeln!
training$price <- as.numeric(gsub("\\CHF", "", training$price) )

# Room as Numeric

training$rooms <- as.numeric(training$rooms )




# Room as Numeric

X_test$rooms <- as.numeric(X_test$rooms )



training$sale <- NULL
X_test$sale<- NULL

# Überprüfung
dim(X_test)
dim(training)











