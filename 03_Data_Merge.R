

# AB hier Datenmerge!


PLZ_bereinigt <- droplevels(PLZ_bereinigt)  

# Test_daten Set mergen!
test_daten <- inner_join(X_test,PLZ_bereinigt,  by = c("zipcode" = "PLZ")) 
test_daten <- test_daten[!duplicated(test_daten$id), ]
dim(test_daten)
# Trainings_Datenset mergen
training_daten <- inner_join(training,PLZ_bereinigt,  by = c("zipcode" = "PLZ")) 
training_daten <- training_daten[!duplicated(training_daten$id), ]
dim(training_daten)# Wir haben hier 4 Einheiten verloren. Damit können wir leben!


# Datenframe umwandeln

test_daten <- as.data.frame(test_daten)
training_daten <- as.data.frame(training_daten)

summary(test_daten)
dim(test_daten)
summary(training_daten)
dim(training_daten)



training_daten <- training_daten[, colMeans(is.na(training_daten)) <= .00000000000000001]
test_daten <- test_daten[, colMeans(is.na(test_daten)) <= .00000000000000001]

dim(training_daten)


# Überprüfen ob ebi beiden die selben Columns gedropped wurden!
colnames(training_daten[,1:30]) == colnames(test_daten[,1:30])
colnames(training_daten[,32:100]) == colnames(test_daten[,31:99])

dim(test_daten)

dim(training_daten)

# Unnötige Spalten entfernen
training_daten$Sprache <- NULL
training_daten$Zusatzziffer  <- NULL
# Unnötige Spalten entfernen
test_daten$Sprache <- NULL
test_daten$Zusatzziffer  <- NULL

# Die Files speichern
getwd()





test_daten <- test_daten[!duplicated(test_daten$id), ]








# HIer alles was mal wichtig war, aber nicht mehr wichtig ist.

# Wird nicht mehr benötigt
# # Bemerkt das wir Probleme mit der Region Biel haben. Im urpsrünglichen Datenset wurde Biel angepasst (Dort war die PLZ 2502 und haben dies mit 2500 überschrieben)
# view(test_daten  %>% filter(is.na(Kantonskürzel) == TRUE))
# 
# # Problem mit der Postleitzahl Luzern! Auch hier im Datenset Luzern hinzugefügt. 
# view(training_daten %>% filter(is.na(Kantonskürzel) == TRUE))



# überprüfen ob es duplikatte gibt. Wird nicht mehr benötigt
# 
# view(test_daten %>% group_by(id) %>% filter(n() > 1))


# View(PLZO_CSV_LV95)
# 
# # Im Prinzip könnten wir hier diese Tabelle normalisieren. Jedoch wird dies hier nicht berücksichtigt.
# 
# # Dafür benötigen wir eine saubere Postleitzahl! Wurde festgestellt, dass die Postleitzahlen korrekt sind
# str(X_test$zipcode)
# str(training$zipcode)
# summary(is.na(X_test$zipcode))
# summary(is.na(training$zipcode))

# 
# 
# 
# view(test_daten  %>% filter(is.na(Kantonskürzel) == TRUE))

# Untersuchen, ob der trainingsdatenset wirklich nur eine Spalte mehr hat  
# length(colnames(training))
# length(colnames(X_test))
# 
# colnames(training)
# colnames(X_test)




