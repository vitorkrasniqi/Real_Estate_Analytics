# Final Prediction on unseen Data

library(dplyr)
prediction_final <- predict(Random_Forest_Model, newdata = test_daten)
prediction_final
test_daten_ <- test_daten
test_daten_$price <- prediction_final


test_daten_$price

colnames(test_daten_)


test_daten_ <- test_daten_[,c("id", "price")]
test_daten_ 

summary(test_daten_$price)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 76848  467684  668151  748624  936195 3135512
str(test_daten_)
test_daten_ <- test_daten_[order(test_daten_$id),]
head(test_daten_)

summary(test_daten_)

# id            price        
# Min.   :    1   Min.   :  76848  
# 1st Qu.: 3751   1st Qu.: 467684  
# Median : 7500   Median : 668151  
# Mean   : 7500   Mean   : 748624  
# 3rd Qu.:11250   3rd Qu.: 936195  
# Max.   :15000   Max.   :3135512  


write.csv(test_daten_, "final_prediction.csv", row.names=FALSE)