library(dplyr)
library(e1071)
library(stats)

args_do_downsampling <- F
source("src/utils/data_preprocessing.R")
source("src/utils/evaluation.R")

start_time <- Sys.time()

weight_ones <- 0.5
svm.fit.linear <- svm(
  formula = target~.,
  data    = train,
  kernel  = "radial",
  class.weights = c("0" = 1 - weight_ones, "1" = weight_ones),
  gamma = 4*0.05^2,
  cost = 4
)

# pred_values <- predict(svm.fit.linear, test)
# evaluate_model(y_pred = pred_values, y_true = test$target)

pred_values_test <- predict(svm.fit.linear, newdata = test)
evaluate_model(y_pred = pred_values_test, y_true = test$target)

pred_values_train <- predict(svm.fit.linear)
evaluate_model(y_pred = pred_values_train, y_true = train$target)

varImp(svm.fit.linear)

save_workspace("C:\\xxTemp\\advanced_ml_RData\\svm_single.RData")


