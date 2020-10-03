# ############################################################### Open Data

# Die Adressdaten sind hier zu unsauber. Ich hole mir Hilfe von OpenData und lade mir folgendes Datenset herunter

PLZO_CSV_LV95 <- read_delim("data/OpenData/PLZO_CSV_LV95/PLZO_CSV_LV95.csv", 
                            ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))



# # Deutsch
# Regionalportrait_original_2020 <- read_delim("data/Regionalportrait_original_2020.csv", ";", escape_double = FALSE, trim_ws = TRUE)
# 
# 

Regionalportraet_Namen_english <- read_delim("data/Weitere RegionalportraitsEN_FR/Regionalportraet_Namen_english.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# 
# Regionalportraet_Namen_franz <- read_delim("data/Weitere RegionalportraitsEN_FR/Regionalportraet_Namen_franz.csv", ";", escape_double = FALSE, trim_ws = TRUE)


# Diese Spalten werden von diesem Datensatz nicht benötigt

PLZO_CSV_LV95$E <- NULL
PLZO_CSV_LV95$N <- NULL
PLZO_CSV_LV95$`BFS-Nr` <- NULL


# ALle als Dataframe umwandeln
PLZO_CSV_LV95 <- as.data.frame(PLZO_CSV_LV95)
PLZ_bereinigt <- PLZO_CSV_LV95[!duplicated(PLZO_CSV_LV95$PLZ),]



# Daten verbinden
PLZ_bereinigt <- left_join(PLZ_bereinigt,Regionalportraet_Namen_english, by = c("Gemeindename" = "Gemeindename"))

#  Ab hier haben wir herausgefunden, dass das englishe oder franz. Datenset besser ist!




# Welche Daten müssen bereinigt werden?
# Diese Daten müssen bereinigt werden, da hier viele NAs
NA_Data <- PLZ_bereinigt[!complete.cases(PLZ_bereinigt$Einwohner),]








# Diese Daten sind mehrheitlich sauber
PLZ_bereinigt <- PLZ_bereinigt[complete.cases(PLZ_bereinigt$Einwohner),]
dim(PLZ_bereinigt)

# Welche Spalten sind rein? Will nur die reinen Spalten berücksichtigen!
PLZ_bereinigt <- PLZ_bereinigt[, colMeans(is.na(PLZ_bereinigt)) <= .00000000000000001]
dim(PLZ_bereinigt)


Colnames <- colnames(PLZ_bereinigt)

# Jetzt will ich Dataframe nur mit relevanten Spalten!
NA_Data <- NA_Data[Colnames]


#######################################################################################################
#Datenbereinigung                                                                                     #
#######################################################################################################

# Schritt 1 Datentypen richtig formatieren
str(PLZ_bereinigt)

PLZ_bereinigt$Einwohner <- as.numeric(sub("’","",  PLZ_bereinigt$Einwohner, fixed = TRUE))
PLZ_bereinigt$`Bevölkerungs-dichte pro km²` <- as.numeric(sub("’","", PLZ_bereinigt$`Bevölkerungs-dichte pro km²`,fixed = TRUE))
PLZ_bereinigt$`Anzahl Privathaushalte` <- as.numeric(sub("’","", PLZ_bereinigt$`Anzahl Privathaushalte`,fixed = TRUE))
PLZ_bereinigt$`Beschäftigte total` <- as.numeric(sub("’","",PLZ_bereinigt$`Beschäftigte total`,fixed = TRUE))
PLZ_bereinigt$`im 1. Sektor`<- as.numeric(sub("’","",PLZ_bereinigt$`im 1. Sektor`,fixed = TRUE))
PLZ_bereinigt$`im 2. Sektor`<- as.numeric(sub("’","",PLZ_bereinigt$`im 2. Sektor`,fixed = TRUE))
PLZ_bereinigt$`im 3. Sektor`<- as.numeric(sub("’","",PLZ_bereinigt$`im 3. Sektor`,fixed = TRUE))
PLZ_bereinigt$`Arbeitsstätten total`<- as.numeric(sub("’","",PLZ_bereinigt$`Arbeitsstätten total`,fixed = TRUE))
PLZ_bereinigt$`im 1. Sektor_1`<- as.numeric(sub("’","",PLZ_bereinigt$`im 1. Sektor_1`,fixed = TRUE))
PLZ_bereinigt$`im 2. Sektor_1`<- as.numeric(sub("’","",PLZ_bereinigt$`im 2. Sektor_1`,fixed = TRUE))
PLZ_bereinigt$`im 3. Sektor_1`<- as.numeric(sub("’","",PLZ_bereinigt$`im 3. Sektor_1`,fixed = TRUE))
PLZ_bereinigt$`Sozialhilfequote 3)`<- as.numeric(PLZ_bereinigt$`Sozialhilfequote 3)`)




PLZ_bereinigt$Ortschaftsname <- as.factor(PLZ_bereinigt$Ortschaftsname)
PLZ_bereinigt$Gemeindename <- as.factor(PLZ_bereinigt$Gemeindename)
PLZ_bereinigt$Kantonskürzel <- as.factor(PLZ_bereinigt$Kantonskürzel)
PLZ_bereinigt$Sprache<- as.factor(PLZ_bereinigt$Sprache)


str(PLZ_bereinigt)


# Das gleiche für den NA Datenset

NA_Data$Einwohner <- as.numeric(sub("’","",  NA_Data$Einwohner, fixed = TRUE))
NA_Data$`Bevölkerungs-dichte pro km²` <- as.numeric(sub("’","", NA_Data$`Bevölkerungs-dichte pro km²`,fixed = TRUE))
NA_Data$`Anzahl Privathaushalte` <- as.numeric(sub("’","", NA_Data$`Anzahl Privathaushalte`,fixed = TRUE))
NA_Data$`Beschäftigte total` <- as.numeric(sub("’","",NA_Data$`Beschäftigte total`,fixed = TRUE))
NA_Data$`im 1. Sektor`<- as.numeric(sub("’","",NA_Data$`im 1. Sektor`,fixed = TRUE))
NA_Data$`im 2. Sektor`<- as.numeric(sub("’","",NA_Data$`im 2. Sektor`,fixed = TRUE))
NA_Data$`im 3. Sektor`<- as.numeric(sub("’","",NA_Data$`im 3. Sektor`,fixed = TRUE))
NA_Data$`Arbeitsstätten total`<- as.numeric(sub("’","",NA_Data$`Arbeitsstätten total`,fixed = TRUE))
NA_Data$`im 1. Sektor_1`<- as.numeric(sub("’","",NA_Data$`im 1. Sektor_1`,fixed = TRUE))
NA_Data$`im 2. Sektor_1`<- as.numeric(sub("’","",NA_Data$`im 2. Sektor_1`,fixed = TRUE))
NA_Data$`im 3. Sektor_1`<- as.numeric(sub("’","",NA_Data$`im 3. Sektor_1`,fixed = TRUE))
NA_Data$`Sozialhilfequote 3)`<- as.numeric(NA_Data$`Sozialhilfequote 3)`)
str(NA_Data)



NA_Data$Ortschaftsname <- as.factor(NA_Data$Ortschaftsname)
NA_Data$Gemeindename <- as.factor(NA_Data$Gemeindename)
NA_Data$Kantonskürzel <- as.factor(NA_Data$Kantonskürzel)
NA_Data$Sprache<- as.factor(NA_Data$Sprache)
str(NA_Data)



# Durch die untige Notitz habe ich herausgefunden, dass diese Geminden in DIESER Arbeit wichtig sind.

Vergessene_Gemeinden <- c("Bevaix","Gorgier","St-Aubin-Sauges")

Vergessene_Gemeinden <- subset(NA_Data,Ortschaftsname == Vergessene_Gemeinden)
dim(Vergessene_Gemeinden)

# Hier habe ich NAs, diese will ich mit dem Mittelwert ersetzen! Kantonaler Mittelwert versteht sich.
Vergessene_Gemeinden[3,7:37]
Vergessene_Gemeinden_untersuchung <- Vergessene_Gemeinden[,1:6]

Daten_Frame_Mittelwert <- subset(PLZ_bereinigt,Kantonskürzel == "NE" )
Daten_Frame_Mittelwert <- subset(Daten_Frame_Mittelwert, Ortschaftsname != Vergessene_Gemeinden)
Daten_Frame_Mittelwert <- Daten_Frame_Mittelwert[,7:37]
dim(Daten_Frame_Mittelwert)

Mittel_Werte_fuer_vergessene_Gemeinden <- colMeans(Daten_Frame_Mittelwert)
typeof(Mittel_Werte_fuer_vergessene_Gemeinden)
Mittel_Werte_fuer_vergessene_Gemeinden <- as.data.frame(Mittel_Werte_fuer_vergessene_Gemeinden)
Mittel_Werte_fuer_vergessene_Gemeinden <- as.data.frame(t(Mittel_Werte_fuer_vergessene_Gemeinden))

rownames(Mittel_Werte_fuer_vergessene_Gemeinden) <- c()

str(Mittel_Werte_fuer_vergessene_Gemeinden)

Vergessene_Gemeinden[,7:37] <- Mittel_Werte_fuer_vergessene_Gemeinden

Vergessene_Gemeinden_untersuchung <- as.data.frame(Vergessene_Gemeinden_untersuchung)


Vergessene_Gemeinden <- cbind(Vergessene_Gemeinden_untersuchung, Mittel_Werte_fuer_vergessene_Gemeinden)


dim(PLZ_bereinigt)
PLZ_bereinigt <- rbind(Vergessene_Gemeinden, PLZ_bereinigt)


#######################################################################################################Jetzt können wir endlich den Datenset verbinden!
# rm(PLZ_bereinigt)








# Weitere Datensätze hinzufügen!

# Steuern für Kanton

Kanton_steuer <- read.csv("~/UniLu/Applied Machine Learning/Project/Immo_Analytics/data/Kanton_steuer.csv", sep=";")
colnames(Kanton_steuer) <- c("Kanton", "Kantondurch100k")
PLZ_bereinigt <- left_join(PLZ_bereinigt,Kanton_steuer, by = c("Kantonskürzel"="Kanton" ))


# Steuern für Gemeinde

Gemeinde_Steuern <- read_delim("data/Gemeinde_Steuern.csv", ";", escape_double = FALSE, trim_ws = TRUE)
Gemeinde_Steuern$X7 <- NULL

colnames(Gemeinde_Steuern) <- c("Kanton", "Gemeinde", "Salar12_5k", "Salar100k", "Salar500k", "salardurchKanton100k")

PLZ_bereinigt <- inner_join(PLZ_bereinigt, Gemeinde_Steuern, by = c("Gemeindename"= "Gemeinde" ))
str(PLZ_bereinigt)


# Datens$tze für Kantonsinformationen

canton_vergleich <- read.csv("~/UniLu/Applied Machine Learning/Project/Immo_Analytics/data/canton_vergleich.csv", sep=";")
PLZ_bereinigt <- left_join(PLZ_bereinigt ,canton_vergleich, by = c("Kantonskürzel"="Canton" ))


colnames(Gemeinde_Steuern)
PLZ_bereinigt$s









































# 
# 
# 
# # Notiz: So wurde das passende Dataset gewählt: 
# 
# 
# # Hier konnte ich untersuchen, welche Regionalporträt (de, en, fr) die wenigsten NA aufzeigt beim Joinen!
# # Daten verbinden
# PLZ_bereinigt_1 <- left_join(PLZ_bereinigt,Regionalportrait_original_2020 , by = c("Gemeindename" = "Gemeindename")) 
# 
# 
# # Daten verbinden
# PLZ_bereinigt_2 <- left_join(PLZ_bereinigt,Regionalportraet_Namen_english , by = c("Gemeindename" = "Gemeindename")) 
# 
# 
# # Daten verbinden
# PLZ_bereinigt_3 <- left_join(PLZ_bereinigt,Regionalportraet_Namen_franz, by = c("Gemeindename" = "Gemeindename")) 
# 
# #  Ab hier haben wir herausgefunden, dass das englishe oder franz. Datenset besser ist!
# 
# 
# 
# 
# # Welche Daten müssen bereinigt werden?
# # Diese Daten müssen bereinigt werden, da hier viele NAs
# NA_Data <- PLZ_bereinigt[!complete.cases(PLZ_bereinigt$Einwohner),]
# 
# 
# NA_Data_1 <- PLZ_bereinigt_1[!complete.cases(PLZ_bereinigt_1$Einwohner),]
# NA_Data_2 <- PLZ_bereinigt_2[!complete.cases(PLZ_bereinigt_2$Einwohner),]
# NA_Data_3 <- PLZ_bereinigt_3[!complete.cases(PLZ_bereinigt_3$Einwohner),]
# dim(NA_Data_1)
# # Viel weniger NA daten. Viele dieser Daten sind für nichts, kann ich löschen. aber warte kontrollieren nochmals
# dim(NA_Data_2)
# dim(NA_Data_3)
# 
# 
# #  Ab hier beginnt die Kontrolle
# Trainig_Daten_NA_join_1 <- inner_join(PLZ_bereinigt_1, training, by = c("PLZ" = "zipcode"))
# Trainig_Daten_NA_join_2 <- inner_join(PLZ_bereinigt_2, X_test, by = c("PLZ" = "zipcode"))
# dim(Trainig_Daten_NA_join_1)
# dim(Trainig_Daten_NA_join_2)
# Trainig_Daten_NA_join_1 <- left_join(PLZ_bereinigt_1, training, by = c("PLZ" = "zipcode"))
# Trainig_Daten_NA_join_2 <- left_join(PLZ_bereinigt_2, training, by = c("PLZ" = "zipcode"))
# summary(is.na(Trainig_Daten_NA_join_1$Einwohner))
# 
# summary(is.na(Trainig_Daten_NA_join_2$Einwohner))
# 
# Trainig_Daten_NA_join_2_which_geminde <- Trainig_Daten_NA_join_2[!complete.cases(Trainig_Daten_NA_join_2$Einwohner),]
# Trainig_Daten_NA_join_2_which_geminde
# 
# 
# # Diese Regionen sind im test und trainigsdaten wichtig! Über diese Regionen haben wir zu wenige Informationen!
# 
# # Diese Regionen sind Betroffen von der Säuberung
# # 
# # 13986          Bevaix 2022            0 La Grande-Béroche            NE      fr      <NA>                         NA                        <NA>             NA
# # 13987          Bevaix 2022            0 La Grande-Béroche            NE      fr      <NA>                         NA                        <NA>             NA
# # 13988          Bevaix 2022            0 La Grande-Béroche            NE      fr      <NA>                         NA                        <NA>             NA
# # 13989         Gorgier 2023            0 La Grande-Béroche            NE      fr      <NA>                         NA                        <NA>             NA
# # 13990 St-Aubin-Sauges 2024            0 La Grande-Béroche            NE      fr      <NA>                         NA                        <NA>             NA
# # 13991 St-Aubin-Sauges 2024            0 La Grande-Béroche            NE      fr      <NA>                         NA                        <NA>             NA
# # 13992 St-Aubin-Sauges 2024            0 La Grande-Béroche            NE      fr      <NA>                         NA                        <NA>             NA
# # 13993 St-Aubin-Sauges 2024            0 La Grande-Béroche            NE      fr      <NA>                         NA                        <NA>             NA
# # 13994 St-Aubin-Sauges 2024            0 La Grande-Béroche            NE      fr      <NA>                         NA                        <NA>             NA
# # 13995 St-Aubin-Sauges 2024            0 La Grande-Béroche            NE      fr      <NA>                         NA                        <NA>             NA
# # 13996 St-Aubin-Sauges 2024            0 La Grande-Béroche            NE      fr
