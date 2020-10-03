# Librarys 
library(data.table)
library(mltools)


test_daten <- test_daten[!duplicated(test_daten$id), ]

dim(training_daten)
dim(test_daten)


################################################################ training Daten
# Feature engineering!



# One hot Encoder anwenden für Wohnungstyp!
training_daten$home_type <- as.factor(training_daten$home_type)
Wohnungstyp <- one_hot(as.data.table(training_daten$home_type))
Wohnungstyp$V1_Wohnung <- NULL
Wohnungstyp$V1_Studio <- NULL


Wohnungstyp$v1_studio<-ifelse(training_daten$rooms == 1, 1, 0)
Wohnungstyp

training_daten <- cbind(training_daten, Wohnungstyp)
# training_daten$home_type <- NULL

####################################################### AREA

#  Das gleiche mit Area!
training_daten <- training_daten %>%
  mutate(Immo_Area = cut(area, breaks = seq(0, 500, by = 25)))


# One hot Encoding für Area!
Onehot_Area <- one_hot(as.data.table(training_daten$Immo_Area))
colnames(Onehot_Area)
training_daten <- cbind(training_daten, Onehot_Area)



################################################### Floors
# One hot Encoding für floors!
training_daten$floors <- as.factor(training_daten$floors)
Onehot_floor <- one_hot(as.data.table(training_daten$floors))
colnames(Onehot_floor)
Onehot_floor$V1_18 <- NULL
Onehot_floor$V1_21<- NULL
Onehot_floor$V1_22<- NULL
Onehot_floor$V1_29<- NULL


training_daten <- cbind(training_daten, Onehot_floor)


#################################################### Time
# Zeit Feat. Engineering

training_daten$year2014 <-ifelse(training_daten$year<=2014, 1, 0)
training_daten$year2017<-ifelse(training_daten$year>=2017, 1, 0)

training_daten$year2017<-ifelse(training_daten$year>=2017, 1, 0)
summary(training_daten$year_built)

training_daten$year_built[training_daten$year_built == 0] <- NA

training_daten$year_built1900<-ifelse(training_daten$year_built >=1400 & training_daten$year_built <=1920, 1, 0)
training_daten$year_built1920<-ifelse(training_daten$year_built>=1920 & training_daten$year_built<=1950, 1, 0)
training_daten$year_built1950<-ifelse(training_daten$year_built>=1950 & training_daten$year_built<=1970, 1, 0)
training_daten$year_built1970<-ifelse(training_daten$year_built>=1970 & training_daten$year_built<=1980, 1, 0)
training_daten$year_built1980<-ifelse(training_daten$year_built>=1980 & training_daten$year_built<=1990 , 1, 0)
training_daten$year_built1990<-ifelse(training_daten$year_built>=1990 & training_daten$year_built<=2005 , 1, 0)
training_daten$year_built2005<-ifelse(training_daten$year_built>=2005& training_daten$year_built<=2010, 1, 0)
training_daten$year_built2010<-ifelse(training_daten$year_built>=2010 & training_daten$year_built<=2015, 1, 0)
training_daten$year_built2015<-ifelse(training_daten$year_built>=2015 & training_daten$year_built<=2018, 1, 0)
training_daten$year_built2018<-ifelse(training_daten$year_built>=2018, 1, 0)

training_daten[is.na(training_daten)] = 0

training_daten$Kantonskürzel <-   droplevels(training_daten)$Kantonskürzel

############################## erster Subset
training_daten <- subset(training_daten, price >= 10000)



# 
# # Feature Engineering: Zusätzliche Spalten erstellen! Für Area
# # Ich eill Area abfangen!
# training_daten$area100<-ifelse(training_daten$area<=100, 1, 0)
# training_daten$area140<-ifelse(training_daten$area>=140, 1, 0)
# training_daten$area200<-ifelse(training_daten$area>=200, 1, 0)
# training_daten$area300<-ifelse(training_daten$area>=300, 1, 0)
# training_daten$area400<-ifelse(training_daten$area>=400, 1, 0)
# training_daten$area500<-ifelse(training_daten$area>=500, 1, 0)
# training_daten$area600<-ifelse(training_daten$area>=600, 1, 0)

##############################################rooms
# Etwa gleich viel 
training_daten$rooms4<-ifelse(training_daten$rooms<=4, 1, 0)
training_daten$rooms6<-ifelse(training_daten$rooms==6 , 1, 0)
training_daten$rooms79<-ifelse(training_daten$rooms== 7 | 9 , 1, 0)
training_daten$rooms8<-ifelse(training_daten$rooms==8, 1, 0)
training_daten$rooms10<-ifelse(training_daten$rooms==10, 1, 0)
training_daten$rooms12<-ifelse(training_daten$rooms== 11| 12, 1, 0)
training_daten$rooms13<-ifelse(training_daten$rooms== 13, 1, 0)
training_daten$rooms141516<-ifelse(training_daten$rooms== 14| 15| 16, 1, 0)
training_daten$rooms1718<-ifelse(training_daten$rooms== 17| 18, 1, 0)
training_daten$rooms19above<-ifelse(training_daten$rooms>= 19, 1, 0)



# Billige Häuser schauen wir uns nicht an!

training_daten <- subset(training_daten, price >= 10000)
training_daten



################################################################# regionaler Vergleich
# Regionale Vergleiche aufstellen!
rm(max, min, mean,median)
max <- aggregate(price ~ zipcode, data = training_daten, max)
colnames(max) <- c("zipcode", "max")
min <- aggregate(price ~ zipcode, data = training_daten, min)
colnames(min) <- c("zipcode", "min")
median <- aggregate(price ~ zipcode, data = training_daten, median)
colnames(median) <- c("zipcode", "median")
mean <- aggregate(price ~ zipcode, data = training_daten, mean)
colnames(mean) <- c("zipcode", "mean")

mean$Sehr_teurer_Durch<-ifelse(mean$mean  >= 2000000, 1, 0)
mean$teurer_Durch <- ifelse(mean$mean >= 1700000  & mean$mean < 2000000, 1, 0)
mean$hoher_Durch <- ifelse(mean$mean >= 1300000  & mean$mean < 1700000, 1, 0)
mean$mehrals_Durch<- ifelse(mean$mean >= 900000 & mean$mean < 1300000 , 1, 0)
mean$kleinerals_Durch<- ifelse(mean$mean >= 700000 & mean$mean < 900000 , 1, 0)
mean$guenst_Durch<- ifelse(mean$mean >= 500000 & mean$mean < 700000 , 1, 0)
mean$kleiner_guenst_Durch<- ifelse(mean$mean >= 300000 & mean$mean < 500000 , 1, 0)
mean$sehr_guenst_Durch<- ifelse(mean$mean <= 300000  , 1, 0)

median$Sehr_teurer_Median<-ifelse(median$median  >= 2000000, 1, 0)
median$teurer_Median <- ifelse(median$median >= 1700000  & median$median < 2000000, 1, 0)
median$hoher_Median <- ifelse(median$median >= 1300000  & median$median < 1700000, 1, 0)
median$mehrals_Median<- ifelse(median$median >= 900000 & median$median < 1300000 , 1, 0)
median$kleinerals_Median<- ifelse(median$median >= 700000 & median$median < 900000 , 1, 0)
median$guenst_Median<- ifelse(median$median >= 500000 & median$median < 700000 , 1, 0)
median$kleiner_guenst_Median<- ifelse(median$median >= 300000 & median$median < 500000 , 1, 0)
median$sehr_guenst_Median<- ifelse(median$median <= 300000  , 1, 0)


# Nun die erstellten Spalten kombinieren

left <- left_join(training_daten,min, by = c("zipcode" = "zipcode"))
left <- left_join(left,max, by = c("zipcode" = "zipcode"))
left <- left_join(left,median, by = c("zipcode" = "zipcode"))
left <- left_join(left,mean, by = c("zipcode" = "zipcode"))

dim(training_daten)

left <- subset(left, left$price * area * rooms  < median* area* rooms * 1.5)


training_daten <- left

dim(training_daten)


################################################################# Update
#  Nochmals regionaler Vergleich aufstellen!

rm(max, min, mean,median)
max <- aggregate(price ~ zipcode, data = training_daten, max)
colnames(max) <- c("zipcode", "max")
min <- aggregate(price ~ zipcode, data = training_daten, min)
colnames(min) <- c("zipcode", "min")
median <- aggregate(price ~ zipcode, data = training_daten, median)
colnames(median) <- c("zipcode", "median")
mean <- aggregate(price ~ zipcode, data = training_daten, mean)
colnames(mean) <- c("zipcode", "mean")

mean$Sehr_teurer_Durch<-ifelse(mean$mean  >= 2000000, 1, 0)
mean$teurer_Durch <- ifelse(mean$mean >= 1700000  & mean$mean < 2000000, 1, 0)
mean$hoher_Durch <- ifelse(mean$mean >= 1300000  & mean$mean < 1700000, 1, 0)
mean$mehrals_Durch<- ifelse(mean$mean >= 900000 & mean$mean < 1300000 , 1, 0)
mean$kleinerals_Durch<- ifelse(mean$mean >= 700000 & mean$mean < 900000 , 1, 0)
mean$guenst_Durch<- ifelse(mean$mean >= 500000 & mean$mean < 700000 , 1, 0)
mean$kleiner_guenst_Durch<- ifelse(mean$mean >= 300000 & mean$mean < 500000 , 1, 0)
mean$sehr_guenst_Durch<- ifelse(mean$mean <= 300000  , 1, 0)

median$Sehr_teurer_Median<-ifelse(median$median  >= 2000000, 1, 0)
median$teurer_Median <- ifelse(median$median >= 1700000  & median$median < 2000000, 1, 0)
median$hoher_Median <- ifelse(median$median >= 1300000  & median$median < 1700000, 1, 0)
median$mehrals_Median<- ifelse(median$median >= 900000 & median$median < 1300000 , 1, 0)
median$kleinerals_Median<- ifelse(median$median >= 700000 & median$median < 900000 , 1, 0)
median$guenst_Median<- ifelse(median$median >= 500000 & median$median < 700000 , 1, 0)
median$kleiner_guenst_Median<- ifelse(median$median >= 300000 & median$median < 500000 , 1, 0)
median$sehr_guenst_Median<- ifelse(median$median <= 300000  , 1, 0)



# Die alten durchschnitte entfernen!
colnames(training_daten)

colnames(training_daten[, 1:162])


training_daten <- training_daten[, 1:162]

left <- left_join(training_daten,min, by = c("zipcode" = "zipcode"))
left <- left_join(left,max, by = c("zipcode" = "zipcode"))
left <- left_join(left,median, by = c("zipcode" = "zipcode"))
left <- left_join(left,mean, by = c("zipcode" = "zipcode"))

training_daten <- left

dim(training_daten )



training_daten <- training_daten[, 1:162]

dim(training_daten)
dim(test_daten)
###################### tuere Region
median_ordered_teuer <- median[,1:2]
median_ordered_teuer  <- median_ordered_teuer  %>% arrange(desc(median_ordered_teuer$median))
median_ordered_teuer  <- median_ordered_teuer [1:50,]

median_ordered_sehr_sehr_teuer <- median_ordered_teuer [1:5,]
median_ordered_sehr_teuer <- median_ordered_teuer [6:15,]
median_ordered_teuer <- median_ordered_teuer [16:50,]

median_ordered_sehr_sehr_teuer$preisstatus <- "Sehr Sehr Teuer"
median_ordered_sehr_teuer$preisstatus <- "Sehr Teuer"
median_ordered_teuer$preisstatus <- "Teuer"

median_ordered_teuer  <- rbind(median_ordered_sehr_sehr_teuer,median_ordered_sehr_teuer,median_ordered_teuer)
median_ordered_teuer$median <- NULL

training_daten <- left_join(training_daten, median_ordered_teuer , by = c("zipcode" = "zipcode"))

a <- training_daten
training_daten$preisstatus[is.na(training_daten$preisstatus)] <- "None"

###################### günstige Region
median_ordered_guenstig <- median[,1:2]
median_ordered_guenstig  <- median_ordered_guenstig  %>% arrange(median_ordered_guenstig$median)
median_ordered_guenstig  <- median_ordered_guenstig[1:400,]

median_ordered_sehr_sehr_guenstig <- median_ordered_guenstig[1:5,]
median_ordered_sehr_guenstig <- median_ordered_guenstig[6:56,]
median_ordered_guenstig <- median_ordered_guenstig[57:400,]

median_ordered_sehr_sehr_guenstig$preisstatus_ <- "Sehr Sehr guenstig"
median_ordered_sehr_guenstig$preisstatus_ <- "Sehr guenstig"
median_ordered_guenstig$preisstatus_ <- "guenstig"

median_ordered_guenstig <- rbind(median_ordered_sehr_sehr_guenstig,median_ordered_sehr_guenstig,median_ordered_guenstig)
median_ordered_guenstig$median <- NULL

training_daten <- left_join(training_daten, median_ordered_guenstig , by = c("zipcode" = "zipcode"))

colnames(training_daten)

training_daten$preisstatus_[is.na(training_daten$preisstatus_)] <- "None"




############################################## One hot Encoder mit den güenstigen & teuren Regionen
str(training_daten)
training_daten$preisstatus <- as.factor(training_daten$preisstatus)
training_daten$preisstatus_ <- as.factor(training_daten$preisstatus_)

Preistatuts_teuer <- one_hot(as.data.table(training_daten$preisstatus))
Preistatuts_guenstig <- one_hot(as.data.table(training_daten$preisstatus_))


training_daten <- cbind(training_daten, Preistatuts_teuer,Preistatuts_guenstig )
dim(training_daten)
training_daten$V1_None <- NULL
# training_daten$id <- NULL


dim(training_daten)
dim(test_daten)


#################### One hot encoder für Kanton
training_daten$Kantonskürzel <- as.factor(training_daten$Kantonskürzel)
Kanton <- one_hot(as.data.table(training_daten$Kantonskürzel))

training_daten <- cbind(training_daten, Kanton )

# training_daten$id <- NULL













# Weitere Feat.Engineering kennzahlen
training_daten$floors <- as.numeric(training_daten$floors)




training_daten$arearooms <- (training_daten$area/ training_daten$rooms)+ 1

training_daten$arearooms_floors <- NULL

training_daten$arearooms_leer <- ( training_daten$floors/ training_daten$area) + 1
training_daten$flaeche <- (training_daten$`Gesamtfläche in km²`/ training_daten$area ) + 1
training_daten$flaeche_ <- (training_daten$`Veränderung in ha` / training_daten$area  ) + 1
training_daten$flaeche__ <- (training_daten$`Durchschnittliche Haushaltsgrösse in Personen` / training_daten$area ) + 1

training_daten$flaeche___ <- (training_daten$`Ausländer in %` / training_daten$area ) + 1
training_daten$flaeche____ <- (training_daten$`0-19 Jahre` / training_daten$area ) + 1
training_daten$flaeche_____ <- (training_daten$`20-64 Jahre` / training_daten$area) + 1
training_daten$flaeche______ <- (training_daten$`65 Jahre und mehr` / training_daten$area) + 1
training_daten$flaeche_______ <- (training_daten$`Rohe Sterbeziffer`/ training_daten$area ) + 1
training_daten$flaeche________ <- (training_daten$`Unproduktive Fläche in %`/ training_daten$area ) + 1



training_daten$roomsrooms_leer <- ( training_daten$floors/ training_daten$rooms) + 1
training_daten$rooms_Rooms <- (training_daten$`Gesamtfläche in km²`/ training_daten$rooms ) + 1
training_daten$rooms_ <- (training_daten$`Veränderung in ha` / training_daten$rooms) + 1
training_daten$rooms__ <- (training_daten$`Durchschnittliche Haushaltsgrösse in Personen` / training_daten$rooms ) + 1

training_daten$rooms___ <- (training_daten$`Ausländer in %` / training_daten$rooms ) + 1
training_daten$rooms____ <- (training_daten$`0-19 Jahre` / training_daten$rooms ) + 1
training_daten$rooms_____ <- (training_daten$`20-64 Jahre` / training_daten$rooms) + 1
training_daten$rooms______ <- (training_daten$`65 Jahre und mehr` / training_daten$rooms) + 1
training_daten$rooms_______ <- (training_daten$`Rohe Sterbeziffer`/ training_daten$rooms ) + 1
training_daten$rooms________ <- (training_daten$`Unproduktive Fläche in %`/ training_daten$rooms ) + 1



################################################## test Daten










# One hot Encoder anwenden für Wohnungstyp!
test_daten$home_type <- as.factor(test_daten$home_type)
Wohnungstyp <- one_hot(as.data.table(test_daten$home_type))
Wohnungstyp$V1_Wohnung <- NULL
Wohnungstyp$V1_Studio <- NULL


Wohnungstyp$v1_studio<-ifelse(test_daten$rooms == 1, 1, 0)
Wohnungstyp

test_daten <- cbind(test_daten, Wohnungstyp)
# test_daten$home_type <- NULL

dim(training_daten)
dim(test_daten)
####################################################### AREA

#  Das gleiche mit Area!
test_daten <- test_daten %>%
  mutate(Immo_Area = cut(area, breaks = seq(0,  2000, by = 25)))

# test_daten[is.na(test_daten$Immo_Area )] <- "[(100,125]"

# One hot Encoding für Area!
Onehot_Area <- one_hot(as.data.table(test_daten$Immo_Area))

test_daten <- cbind(test_daten, Onehot_Area)

dim(training_daten)
dim(test_daten)

################################################### Floors
# One hot Encoding für floors!
test_daten$floors <- as.factor(test_daten$floors)
Onehot_floor <- one_hot(as.data.table(test_daten$floors))


colnames(Onehot_floor)
Onehot_floor$V1_25 <- NULL



test_daten <- cbind(test_daten, Onehot_floor)






dim(training_daten)

dim(test_daten)

# Wir bemerken hier 4 Zeilen unterschiede weggen Floor hot code vector( Beide datensets haben unterschiedliche Floors)






#################################################### Time
# Zeit Feat. Engineering

test_daten$year2014 <-ifelse(test_daten$year<=2014, 1, 0)
test_daten$year2017<-ifelse(test_daten$year>=2017, 1, 0)

test_daten$year2017<-ifelse(test_daten$year>=2017, 1, 0)
summary(test_daten$year_built)

test_daten$year_built[test_daten$year_built == 0] <- NA

test_daten$year_built1900<-ifelse(test_daten$year_built >=1400 & test_daten$year_built <=1920, 1, 0)
test_daten$year_built1920<-ifelse(test_daten$year_built>=1920 & test_daten$year_built<=1950, 1, 0)
test_daten$year_built1950<-ifelse(test_daten$year_built>=1950 & test_daten$year_built<=1970, 1, 0)
test_daten$year_built1970<-ifelse(test_daten$year_built>=1970 & test_daten$year_built<=1980, 1, 0)
test_daten$year_built1980<-ifelse(test_daten$year_built>=1980 & test_daten$year_built<=1990 , 1, 0)
test_daten$year_built1990<-ifelse(test_daten$year_built>=1990 & test_daten$year_built<=2005 , 1, 0)
test_daten$year_built2005<-ifelse(test_daten$year_built>=2005& test_daten$year_built<=2010, 1, 0)
test_daten$year_built2010<-ifelse(test_daten$year_built>=2010 & test_daten$year_built<=2015, 1, 0)
test_daten$year_built2015<-ifelse(test_daten$year_built>=2015 & test_daten$year_built<=2018, 1, 0)
test_daten$year_built2018<-ifelse(test_daten$year_built>=2018, 1, 0)

test_daten[is.na(test_daten)] = 0

test_daten$Kantonskürzel <-   droplevels(test_daten)$Kantonskürzel


dim(test_daten)
dim(training_daten)


# 
# # Feature Engineering: Zusätzliche Spalten erstellen! Für Area
# # Ich eill Area abfangen!
# test_daten$area100<-ifelse(test_daten$area<=100, 1, 0)
# test_daten$area140<-ifelse(test_daten$area>=140, 1, 0)
# test_daten$area200<-ifelse(test_daten$area>=200, 1, 0)
# test_daten$area300<-ifelse(test_daten$area>=300, 1, 0)
# test_daten$area400<-ifelse(test_daten$area>=400, 1, 0)
# test_daten$area500<-ifelse(test_daten$area>=500, 1, 0)
# test_daten$area600<-ifelse(test_daten$area>=600, 1, 0)

##############################################rooms
# Etwa gleich viel 
test_daten$rooms4<-ifelse(test_daten$rooms<=4, 1, 0)
test_daten$rooms6<-ifelse(test_daten$rooms==6 , 1, 0)
test_daten$rooms79<-ifelse(test_daten$rooms== 7 | 9 , 1, 0)
test_daten$rooms8<-ifelse(test_daten$rooms==8, 1, 0)
test_daten$rooms10<-ifelse(test_daten$rooms==10, 1, 0)
test_daten$rooms12<-ifelse(test_daten$rooms== 11| 12, 1, 0)
test_daten$rooms13<-ifelse(test_daten$rooms== 13, 1, 0)
test_daten$rooms141516<-ifelse(test_daten$rooms== 14| 15| 16, 1, 0)
test_daten$rooms1718<-ifelse(test_daten$rooms== 17| 18, 1, 0)
test_daten$rooms19above<-ifelse(test_daten$rooms>= 19, 1, 0)



dim(training_daten)
dim(test_daten)





###################### tuere Region


test_daten <- left_join(test_daten, median_ordered_teuer , by = c("zipcode" = "zipcode"))

a <- test_daten
test_daten$preisstatus[is.na(test_daten$preisstatus)] <- "None"

###################### günstige Region


test_daten <- left_join(test_daten, median_ordered_guenstig , by = c("zipcode" = "zipcode"))

test_daten$preisstatus_[is.na(test_daten$preisstatus_)] <- "None"




############################################## One hot Encoder mit den güenstigen & teuren Regionen
str(test_daten)
test_daten$preisstatus <- as.factor(test_daten$preisstatus)
test_daten$preisstatus_ <- as.factor(test_daten$preisstatus_)

Preistatuts_teuer <- one_hot(as.data.table(test_daten$preisstatus))
Preistatuts_guenstig <- one_hot(as.data.table(test_daten$preisstatus_))


test_daten <- cbind(test_daten, Preistatuts_teuer,Preistatuts_guenstig )
dim(test_daten)
test_daten$V1_None <- NULL
# test_daten$id <- NULL

training_daten$`V1_Sehr Sehr guenstig` <- NULL
dim(training_daten)
dim(test_daten)

colnames(training_daten)
colnames(test_daten)


colnames(training_daten[,25:168]) == colnames(test_daten[,24:167])



#################### One hot encoder für Kanton
test_daten$Kantonskürzel <- as.factor(test_daten$Kantonskürzel)
Kanton <- one_hot(as.data.table(test_daten$Kantonskürzel))

test_daten <- cbind(test_daten, Kanton )
# 
# test_daten$id <- NULL
colnames(test_daten)
colnames(training_daten)

dim(training_daten)
dim(test_daten)


# Weitere Feat.Engineering kennzahlen
test_daten$floors <- as.numeric(test_daten$floors)




test_daten$arearooms <- (test_daten$area/ test_daten$rooms)+ 1

test_daten$arearooms_floors <- NULL

test_daten$arearooms_leer <- ( test_daten$floors/ test_daten$area) + 1
test_daten$flaeche <- (test_daten$`Gesamtfläche in km²`/ test_daten$area)  + 1
test_daten$flaeche_ <- (test_daten$`Veränderung in ha` / test_daten$area ) + 1
test_daten$flaeche__ <- (test_daten$`Durchschnittliche Haushaltsgrösse in Personen` / test_daten$area )+ 1

test_daten$flaeche___ <- (test_daten$`Ausländer in %` / test_daten$area) + 1
test_daten$flaeche____ <- (test_daten$`0-19 Jahre` / test_daten$area )+ 1
test_daten$flaeche_____ <- (test_daten$`20-64 Jahre` / test_daten$area )+ 1
test_daten$flaeche______ <- (test_daten$`65 Jahre und mehr` / test_daten$area)+ 1
test_daten$flaeche_______ <- (test_daten$`Rohe Sterbeziffer`/ test_daten$area) + 1
test_daten$flaeche________ <- (test_daten$`Unproduktive Fläche in %`/ test_daten$area) + 1



test_daten$roomsrooms_leer <- ( test_daten$floors/ test_daten$rooms) + 1
test_daten$rooms_Rooms <- (test_daten$`Gesamtfläche in km²`/ test_daten$rooms)  + 1
test_daten$rooms_ <- (test_daten$`Veränderung in ha` / test_daten$rooms  )+ 1
test_daten$rooms__ <- (test_daten$`Durchschnittliche Haushaltsgrösse in Personen` / test_daten$rooms )+ 1

test_daten$rooms___ <- (test_daten$`Ausländer in %` / test_daten$rooms )+ 1
test_daten$rooms____ <- (test_daten$`0-19 Jahre` / test_daten$rooms) + 1
test_daten$rooms_____ <- (test_daten$`20-64 Jahre` / test_daten$rooms)+ 1
test_daten$rooms______ <- (test_daten$`65 Jahre und mehr` / test_daten$rooms)+ 1
test_daten$rooms_______ <- (test_daten$`Rohe Sterbeziffer`/ test_daten$rooms )+ 1
test_daten$rooms________ <- (test_daten$`Unproduktive Fläche in %`/ test_daten$rooms) + 1




# rooms_______    rooms________


dim(training_daten)
dim(test_daten)




##################################### als XLS speichern

# install.packages("dataframes2xls")
# library(dataframes2xls)
# write.csv(training_daten, "training_daten_clean_tidy.csv")
# write.csv(test_daten, "test_daten_clean_tidy.csv")
# 












# 
# 
# 
# dim(training_daten)
# 
# 
# training_daten$mi
# 
# a <- lm(price ~., training_daten)
# summary(a)
# dim(training_daten)
# 
# 
# colnames(training_daten)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# b <- lm(price ~ area + balcony + basement + bath_tube + building_plot + 
#           cabletv + ceiling + cheminee + elevator + first_time + floors + 
#           furnished + kids_friendly + laundry + minergie + new_building + 
#           newly_built + oldbuilding + oven + parking_indoor + parking_outside + 
#           playground + pool + quiet + raised_groundfloor + rooms + 
#           sunny + terrace + topstorage + veranda + wheelchair + year + 
#           year_built + zipcode + Einwohner + `Veränderung in % 2010-2018` + 
#           `Bevölkerungs-dichte pro km²` + `Ausländer in %` + `0-19 Jahre` + 
#           `20-64 Jahre` + `65 Jahre und mehr` + `Rohe Heiratssziffer` + 
#           `Rohe Scheidungsziffer` + `Rohe Geburtenziffer` + `Rohe Sterbeziffer` + 
#           `Anzahl Privathaushalte` + `Durchschnittliche Haushaltsgrösse in Personen` + 
#           `Gesamtfläche in km²` + `Siedlungsfläche in %` + `Veränderung in ha` + 
#           `Landwirtschafts-fläche in %` + `Veränderung in ha_1` + `Wald und Gehölze in %` + 
#           `Unproduktive Fläche in %` + `Leerwohnungs-ziffer` + `Neu gebaute Wohnungen pro 1000 Einwohner` + 
#           V1_Attika + V1_Dachwohnung + Einwohner.in.1000 + Veränderung.in.. + 
#           pro.km² + X0.19 + X20.64 + X65.und.mehr + Ausländer.in.. + 
#           Rohe.Heiratsziffer + Rohe.Scheidungsziffer + Rohe.Geburtenziffer + 
#           Rohe.Sterbeziffer + Anzahl.Privathaushalte.in.1000 + Durchschnittliche.Haushaltsgrösse.in.Personen + 
#           Siedlungsflächen.in.. + Veränderung.in...1 + Landwirtschaftsflächen.in.. + 
#           Veränderung.in...2 + Wald.und.Gehölze.in.. + Unproduktive.Flächen.in.. + 
#           Nettoerwerbsquote..15.64.Jährige..2. + Arbeitslosenquote..gemäss.SECO. + 
#           Bruttoinlandprodukt.pro.Einwohner.in.Fr. + Veränderung.des.kantonalen.BIP.in.. + 
#           Beschäftigte..Total.in.1000 + arearooms + arearooms_leer + 
#           flaeche + flaeche_ + flaeche__ + flaeche___ + flaeche____ + 
#           flaeche_____ + flaeche______ + flaeche_______ + flaeche________ + 
#           roomsrooms_leer + rooms_ + rooms__ + rooms___ + rooms____ + 
#           rooms_____ + rooms______ + rooms_______ + rooms________ + 
#           year2014 + year2017 + year_built1900 + year_built1920 + year_built1950 + 
#           year_built1970 + year_built1980 + year_built1990 + year_built2005 + 
#           year_built2010 + year_built2015 + year_built2018, lefti)
# 
# 
# 
# summary(b)
# 
# 
# 
# 
# a <- lm(price ~ ., training_daten)
# summary(a)
# training_daten
# 
# 
# step <- step(a, direction = "forward")
# 
# 
# 
# 
# 
# 
# 
# 



