########################################################### Trainingsdaten!

#  Area Usable kann gar nicht grösser sein als area!


rette_so_viel_area_wie_moeglich <- subset(training_daten, area_useable > area)
rette_so_viel_area_wie_moeglich[, c("area", "area_useable")] <- rette_so_viel_area_wie_moeglich[, c( "area_useable","area")]
dim(rette_so_viel_area_wie_moeglich)

training_daten <- subset(training_daten, area_useable <= area)
training_daten <- rbind(training_daten,rette_so_viel_area_wie_moeglich )
dim(training_daten)





# Area 0 kann nicht sein. Entfernen! Manchmal mehr Zimmer als Quadratmeter, korrigieren!
training_daten <- subset(training_daten, area != 0)
dim(training_daten)
Wohnungen_korrigieren <- subset(training_daten, area <= rooms)
Wohnungen_korrigieren <- subset(Wohnungen_korrigieren, price < 900000)
Wohnungen_korrigieren
Wohnungen_korrigieren[, c("area", "rooms")] <- Wohnungen_korrigieren[, c("rooms","area" )]
Wohnungen_korrigieren

dim(training_daten)
dim(Wohnungen_korrigieren)
training_daten <- rbind(Wohnungen_korrigieren, training_daten)
dim(training_daten)







# Unnötige Spalten entfernen!
# trainingsset vorbereiten
training_daten$Kantonskürzel <-   droplevels(training_daten)$Kantonskürzel

varialbes_delete<-c("address","sale","area_useable","date","date_available","municipality","street","Ortschaftsname","Gemeindename")
#training data set
training_daten[,varialbes_delete]<-NULL






# Mehr als 23 Zimmer m¨ssen wir wegnehmen
# Mehr als 500 Quadratemeter AREA nehmen wir weg
# Wohnungen mit 4,11 nehmen wir auch weg
training_daten <- subset(training_daten, training_daten$area < 499)
training_daten <- subset(training_daten, training_daten$rooms < 23)
training_daten <- subset(training_daten, training_daten$rooms != c(4,11))
dim(training_daten)  # 32488 




training_daten <- subset(training_daten, price >= 50000)
training_daten <- subset(training_daten, area >= 15)
dim(training_daten)
 






########################################################## Test Daten

#  Area Usable kann gar nicht grösser sein als area!


rette_so_viel_area_wie_moeglich <- subset(test_daten, area_useable > area)
rette_so_viel_area_wie_moeglich[, c("area", "area_useable")] <- rette_so_viel_area_wie_moeglich[, c( "area_useable","area")]
dim(rette_so_viel_area_wie_moeglich)

test_daten <- subset(test_daten, area_useable <= area)
test_daten <- rbind(test_daten,rette_so_viel_area_wie_moeglich )
dim(test_daten)



Wohnungen_korrigieren <- subset(test_daten, area <= rooms)

Wohnungen_korrigieren
Wohnungen_korrigieren[, c("area", "rooms")] <- Wohnungen_korrigieren[, c("rooms","area" )]
Wohnungen_korrigieren

dim(test_daten)
dim(Wohnungen_korrigieren)
test_daten <- rbind(Wohnungen_korrigieren, test_daten)


test_daten[,varialbes_delete]<-NULL



