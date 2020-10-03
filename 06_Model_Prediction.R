dim(training_daten)
dim(test_daten)
library(caret)
library(MASS)

library(modelr)
library(broom)

#








######################################################## Training und Test Daten erstellen

# Set seed
set.seed(42)

# Zeilenordnung randomisieren 
rows <- sample(nrow(training_daten))

# Randomly order data
shuffled_training_daten <- training_daten <- training_daten[rows, ]



# Stärke des Splits definieren. 80 / 20
split <- round(nrow(shuffled_training_daten) * .80)

# Train Daten
train <- shuffled_training_daten[1:split, ]

# Create test
test <- shuffled_training_daten[(split + 1):nrow(shuffled_training_daten), ]

#################################################################################################################




############################################################################################# Einfache Regression
#  Hier mal eine einfache Regression
# Zuerst haben wir hier eine multivariate Regression ohne grossen Aufwand aufgestellt.
# Fit lm model: model
model <- lm(price ~ area, train)

# Predict on full data: p
p <- predict(model, train)

# Compute errors: error
error <- p -  train[["price"]]

# Calculate RMSE
sqrt(mean((error)^2))


#erster Train / Test prediction vergleich



# Fit lm model on train: model
model <- lm(price ~ area, train)

# Predict on test: p
p <- predict(model, test)



BIC(model)
AIC(model)
train_evaluation_ <- data.frame(
  R2 = rsquare(model, data = train),
  RMSE = rmse(model, data = train),
  MAE = mae(model, data = train)
)
train_evaluation_

# Compute errors: error
error <- p - test[["price"]]

# Calculate RMSE
sqrt(mean(error^2))



# Einfache Regression mit Caret
set.seed(120)
model <- train(
  price ~ area , 
  training_daten,
  method = "lm",
  trControl = trainControl(
    method = "cv", 
    number = 5,
    verboseIter = TRUE
  )
)

summary(model)

Einf_Regression <- data.frame(
  R2 = rsquare(model, data = train),
  RMSE = rmse(model, data = train),
  MAE = mae(model, data = train)
)
Einf_Regression

# R2     RMSE      MAE
# 1 0.3757154 335417.4 243490.7







############################################################################################## Forumla schreiben

formula <- price ~ area *Salar500k * `Ausländer in %` + balcony + basement + bath_tube + 
  building_plot + cabletv + ceiling + cheminee + elevator + 
  first_time + floors*Salar500k + furnished + kids_friendly * Salar12_5k  + laundry + 
  minergie + new_building * Salar12_5k+ newly_built + oldbuilding + oven + 
  parking_indoor*Salar500k  + parking_outside + playground + pool + quiet*Salar500k  + 
  raised_groundfloor + rooms * `Ausländer in %` + sunny*Salar500k  + terrace + topstorage + 
  veranda + wheelchair + year + year_built * Salar12_5k+ Einwohner* Salar12_5k + `Veränderung in % 2010-2018` + 
  `Bevölkerungs-dichte pro km²` * Salar12_5k  + `0-19 Jahre` * Salar12_5k + `20-64 Jahre` + 
  `65 Jahre und mehr`* `Ausländer in %` + `Rohe Heiratssziffer` + `Rohe Scheidungsziffer`*Salar500k  + 
  `Rohe Geburtenziffer` + `Rohe Sterbeziffer` + `Anzahl Privathaushalte`* `Ausländer in %` + 
  `Durchschnittliche Haushaltsgrösse in Personen`* `Ausländer in %` + `Gesamtfläche in km²` * `Ausländer in %`+ 
  `Siedlungsfläche in %` * Salar12_5k *Salar500k  +  `Wald und Gehölze in %` + `Unproduktive Fläche in %` + 
  `Leerwohnungs-ziffer` * Salar12_5k  + `Neu gebaute Wohnungen pro 1000 Einwohner` * `Ausländer in %`+ 
  Kantondurch100k + Salar12_5k + Salar100k + Salar500k + salardurchKanton100k + 
  Einwohner.in.1000 + Veränderung.in.. + pro.km² + X0.19 + 
  X20.64 + X65.und.mehr + Ausländer.in.. + Rohe.Heiratsziffer *Salar500k  + 
  Rohe.Scheidungsziffer + Rohe.Geburtenziffer + Rohe.Sterbeziffer + 
  Anzahl.Privathaushalte.in.1000 + Durchschnittliche.Haushaltsgrösse.in.Personen + 
  Siedlungsflächen.in..*Salar500k + Veränderung.in...1 *Salar500k+ Landwirtschaftsflächen.in..*Salar500k + 
  Veränderung.in...2 + Wald.und.Gehölze.in.. + Unproduktive.Flächen.in.. + 
  Nettoerwerbsquote..15.64.Jährige..2.* `Ausländer in %` + Arbeitslosenquote..gemäss.SECO. + 
  Bruttoinlandprodukt.pro.Einwohner.in.Fr. + Veränderung.des.kantonalen.BIP.in.. + 
  Beschäftigte..Total.in.1000 + V1_Attika *Salar500k + V1_Dachwohnung *Salar500k + 
  V1_Ferienwohnung*Salar500k  + V1_Loft + V1_Maisonette + V1_Terrassenwohnung* `Ausländer in %` *Salar500k  + 
  v1_studio  + `V1_(0,25]`* `Ausländer in %` + `V1_(25,50]`* `Ausländer in %` + `V1_(50,75]`* `Ausländer in %` + 
  `V1_(75,100]`* `Ausländer in %` + `V1_(100,125]`* `Ausländer in %` + `V1_(125,150]`* `Ausländer in %` + `V1_(150,175]` + 
  `V1_(175,200]`* `Ausländer in %` + `V1_(200,225]` * `Ausländer in %`+ `V1_(225,250]` + `V1_(250,275]` * `Ausländer in %` + 
  `V1_(275,300]` * `Ausländer in %`+ `V1_(300,325]` * `Ausländer in %`+ `V1_(325,350]` + `V1_(350,375]` * `Ausländer in %` + 
  `V1_(375,400]`* `Ausländer in %` + `V1_(400,425]`* `Ausländer in %` + `V1_(425,450]`* `Ausländer in %` + `V1_(450,475]` * `Ausländer in %`+ 
  `V1_(475,500]`* `Ausländer in %`+ year_built1900 + year_built1920 + 
  year_built1950*Salar500k  + year_built1970 + year_built1980 + year_built1990 + 
  year_built2005 *Salar500k + year_built2010 + year_built2015 + year_built2018*Salar500k  + 
  rooms4* `Ausländer in %`  + rooms79 * `Ausländer in %` + rooms8 * `Ausländer in %`+ rooms10 + rooms12 * `Ausländer in %`+ 
  rooms13 + rooms141516 + rooms1718 + preisstatus + 
  preisstatus_ + `V1_Sehr Sehr Teuer` + `V1_Sehr Teuer` + V1_Teuer *Salar500k  + 
  V1_guenstig + V1_None + `V1_Sehr guenstig` + V1_AG + V1_AI + 
  V1_AR* `Ausländer in %` *Salar500k + V1_BE * `Ausländer in %`*Salar500k + V1_BL* `Ausländer in %` + V1_BS + V1_FR * `Ausländer in %`+ V1_GE* `Ausländer in %` + V1_GL + V1_GR + 
  V1_JU + V1_LU * `Ausländer in %` + V1_NE + V1_NW * `Ausländer in %`+ V1_OW + V1_SG + V1_SH + V1_SO + 
  V1_SZ + arearooms + arearooms_leer + flaeche + flaeche_ + 
  flaeche__ * `Ausländer in %`+ flaeche___ + flaeche____ * Salar12_5k + flaeche_____ * Salar12_5k + flaeche______ + 
  flaeche_______ + flaeche________ + roomsrooms_leer*Salar500k  + 
  rooms__ + rooms___ * `Ausländer in %`+ rooms____ * `Ausländer in %`+ rooms_____* `Ausländer in %` + rooms______* `Ausländer in %` + 
   area:`Ausländer in %` + `Ausländer in %`:`Bevölkerungs-dichte pro km²` + 
  `Ausländer in %`:`0-19 Jahre`*Salar500k  + `Ausländer in %`:`20-64 Jahre` *Salar500k + 
  `Ausländer in %`:`65 Jahre und mehr` * `Ausländer in %`*Salar500k  + `Ausländer in %`:`Gesamtfläche in km²` + 
  `Ausländer in %`:`Siedlungsfläche in %`*Salar500k  + `Ausländer in %`:`Leerwohnungs-ziffer`*Salar500k  + 
  `Ausländer in %`:Kantondurch100k*Salar500k  + `Ausländer in %`:Salar12_5k + 
  `Ausländer in %`:Salar100k + `Ausländer in %`:Salar500k + 
  `Ausländer in %`:Anzahl.Privathaushalte.in.1000 + `Ausländer in %`:V1_Attika + 
  `Ausländer in %`:V1_Ferienwohnung *Salar500k + `Ausländer in %`:`V1_(75,100]` *Salar500k + 
  `Ausländer in %`:`V1_(100,125]` + `Ausländer in %`:`V1_(425,450]` + 
  `Ausländer in %`:`V1_(450,475]`*Salar500k  + `Ausländer in %`:V1_4 *Salar500k + 
  `Ausländer in %`*`V1_Sehr Teuer` + `Ausländer in %`*V1_LU + 
  `Ausländer in %`*V1_NE *Salar500k 



########################################################################### Multivariate Regression mit training/test und Base R
train_mlm <- lm(formula = formula, data = train)
summary(train_mlm)


test_mlm <- lm(formula = formula, data = test)
summary(test_mlm )
#  Model evaluation

train_evaluation <- data.frame(
  R2 = rsquare(train_mlm, data = train),
  RMSE = rmse(train_mlm, data = train),
  MAE = mae(train_mlm, data = train)
)
train_evaluation

AIC(train_mlm)
BIC(train_mlm)

# > train_evaluation
# R2     RMSE      MAE
# 1 0.7714852 202931.8 145522.4
# > 
#   > AIC(train_mlm)
# [1] 603134.5
# > BIC(train_mlm)
# [1] 605071.2
# 






test_evaluation <- data.frame(
  R2 = rsquare(test_mlm, data = train),
  RMSE = rmse(test_mlm, data = train),
  MAE = mae(test_mlm, data = train)
)

test_evaluation
AIC(test_mlm)
BIC(test_mlm)

# 
# > test_evaluation
# R2     RMSE      MAE
# 1 0.7515395 211603.4 151486.3
# > AIC(test_mlm)
# [1] 151172.2
# > BIC(test_mlm)
# [1] 152766.8




# weitere Berechnungen
#Train / Test prediction vergleich

# Predict on test: p
p <- predict(train_mlm, test)
# Compute errors: error
error <- p - test[["price"]]

# Calculate RMSE
sqrt(mean(error^2))




# Multivariate Regression mit Cross Validation: 10 x %
set.seed(120)
model <- train(
  formula , 
  training_daten,
  method = "lm",
  trControl = trainControl(
    method = "cv", 
    number = 5,
    verboseIter = TRUE
  )
)



Multivairate_verfahren <- data.frame(
  R2 = rsquare(model, data = train),
  RMSE = rmse(model, data = train),
  MAE = mae(model, data = train)
)

Multivairate_verfahren

# R2     RMSE      MAE
# 1 0.7706733 203292.1 145614.5




############################################################################## Massnahmen für Model selection
# Linear Regression with Stepwise Selection
Linear_Regression_with_Stepwise_Selection <- train(formula,
                                                  tuneLength = 1,
                                                  data = training_daten, 
                                                  method = "leapSeq",
                                                  trControl = trainControl(
                                                    method = "cv",
                                                    number = 5, 
                                                    
                                                    verboseIter = TRUE
                                                  )
)

# RMSE      Rsquared   MAE     
# 375942.4  0.2212433  275725.3

#  linear Regression with Backwards Selection: Nicht so gute Werte
#  Benötigt zu viel Power
Linear_Regression_with_Backward_Selection <- train(formula,
                                                   tuneLength = 1,
                                                   data = training_daten, 
                                                   method = "leapBackward",
                                                   trControl = trainControl(
                                                     method = "cv",
                                                     number = 5, 
                                                     
                                                     verboseIter = TRUE
                                                   )
)

# Berechnung mit anderen Librarys

step.model <- train(formula, data = train,
                    method = "leapForward", 
                    trControl = train.control)

p.model <- stepAIC(test_mlm, direction = "both", 
                   trace = TRUE)
step <- step(test_mlm, k = 2)

train.control <- trainControl(method = "cv", number = 2)










########################################################################################### bagging tree

# install.packages("ipred")
library(ipred)
set.seed(120)
bagging_tree <- bagging(formula = formula, 
                        data = train, k = 5,
             nbagg = 100,  
             coob = TRUE
             )





summary(bagging_tree)
# 
# Bagging regression trees with 100 bootstrap replications 
# 
# Call: bagging.data.frame(formula = formula, data = train, k = 5, nbagg = 100, 
#                          coob = TRUE)
# 
# Out-of-bag estimate of root mean squared error:  248992 



##############################################################################################################################################
# Random forest
# Fit random forest: model

# 
set.seed(120)
Random_Forest_Model <- train(formula,
                             tuneLength = 1,
                             data = training_daten, 
                             method = "ranger",
                             trControl = trainControl(
                               method = "cv",
                               number = 5, 
                               
                               verboseIter = TRUE
                             )
)




Random_Forest_Model$results


# mtry min.node.size  splitrule     RMSE  Rsquared      MAE   RMSESD RsquaredSD    MAESD
# 1   16             5   variance 165088.2 0.8508399 109291.8 5175.007 0.01029209 2123.548
# 2   16             5 extratrees 166891.6 0.8485871 111771.3 5067.013 0.00969643 2043.601

Random_Forest_Model$results
plot(Random_Forest_Model)







################################################################################################ RIdge Lasso Elastic Net

#Caret-Package---
X<- model.matrix(formula,
                 data=training_daten)

Y<-training_daten$price



#train additionally: tunegrid.
set.seed(120)
cv_glmnet <-train(
  x=X,
  y=Y,
  method = "glmnet",
  preProcess = c("zv", "center", "scale"),
  trControl = trainControl(method = "cv", number = 5),
  tuneLength = 10)

cv_glmnet

#Plotting the results
cv_glmnet$bestTune
ggplot(cv_glmnet)

#  Unser bestes Elastic Net (Ridge Lasso) hat folgende Parameter
cv_glmnet$bestTune
# alpha   lambda
# 11   0.2 125.4076


# Somit ist das beste Resultat von Ridge lasso
# alpha  lambda       RMSE      Rsquared   MAE 
# 0.2       125.4076  208238.1  0.7617587  148637.4

#predict sales price on training data
pred<-predict(cv_glmnet,tr)

#compute RMSE of transformed predicted
RMSE(pred, Y)


#VIP---
library(vip)
#Variante I
vip(cv_glmnet, num_features=50, bar=FALSE)
#Variante II
varImp(cv_glmnet)

# glmnet variable importance
# 
# only 20 most important variables shown (out of 275)
# 
# Overall
# area                                                              100.00
# `Ausländer in %`:Salar100k                                         88.43
# area:Salar500k                                                     71.58
# `Ausländer in %`:Kantondurch100k                                   70.30
# Salar500k                                                          59.89
# Salar500k:Veränderung.in...1                                       52.41
# `Ausländer in %`:Nettoerwerbsquote..15.64.Jährige..2.              49.61
# area:`Ausländer in %`                                              45.52
# `Ausländer in %`:`Durchschnittliche Haushaltsgrösse in Personen`   42.91
# `Ausländer in %`:`Gesamtfläche in km²`                             38.96
# V1_GE                                                              38.94
# Salar500k:Salar12_5k:`Siedlungsfläche in %`                        38.65
# `Ausländer in %`:`65 Jahre und mehr`                               37.22
# Salar500k:`Ausländer in %`:Kantondurch100k                         34.74
# `Bevölkerungs-dichte pro km²`                                      34.58
# Salar500k:V1_BE                                                    34.12
# Salar12_5k:`Siedlungsfläche in %`                                  32.27
# Salar500k:Rohe.Heiratsziffer                                       29.20
# `Ausländer in %`:`Siedlungsfläche in %`                            28.51
# Salar500k:`Ausländer in %`                                         28.50
# 

cv_glmnet[4]
cv_glmnet[6]
cv_glmnet[12]

