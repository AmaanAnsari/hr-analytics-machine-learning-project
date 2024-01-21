library(randomForest)

args_do_downsampling <- T
source("src/utils/data_preprocessing.R")
source("src/utils/evaluation.R")


start_time <- Sys.time()

randomForest.fit <- randomForest(
  formula    = target ~ .,
  data       = train,
  mtry       = 5,                            # ~Wurzel(12) -> 3,464
  ntree      = 2,                          #Anzahl der Bäume
  importance = FALSE,                          #Relevanz der Variablen wird berechnet
  na.action = na.omit
)

pred_values_test <- predict(randomForest.fit, newdata = test, type="response")
evaluate_model(y_pred = pred_values_test, y_true = test$target)

pred_values_train <- predict(randomForest.fit, type="response")
evaluate_model(y_pred = pred_values_train, y_true = train$target)

# Relevanz der Variablen
importance(randomForest.fit, type = 2)
write.csv2(importance(randomForest.fit, type = 2), "C:\\xxTemp\\advanced_ml_RData\\randomForest_single.csv")
varImpPlot(randomForest.fit, type = 2)

# time end
end_time <- Sys.time()
end_time - start_time

save_workspace("C:\\xxTemp\\advanced_ml_RData\\randomForest_single.RData")