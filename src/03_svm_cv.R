library(dplyr)
library(e1071)
library(stats)
library(caret)

args_do_downsampling <- F
source("src/utils/data_preprocessing.R")
source("src/utils/evaluation.R")
source("src/utils/parallel_processing.R")

start_parallel_processing()

# Define the 5-fold cross-validation control
ctrl <- caret::trainControl(method = "cv", number = 5, summaryFunction = calc_f1_score_arith_for_caret, allowParallel = T)
netz <- expand.grid(sigma = c(0.05, 0.1),
                    C = 2^(2:4),
                    Weight = c(seq(0.25, 1, 0.25), 0.125))


# Train the SVM model using 5-fold cross-validation
svm.fit <- caret::train(target ~ .,
                 data = train,
                 method = "svmRadialWeights",
                 trControl = ctrl,
                 tuneGrid = netz,
                 metric = "F1_lev1",
                 preProcess  = c("center", "scale"),
                 maximize = TRUE)

# Print the results of each fold
print(svm.fit)

pred_values_test <- predict(svm.fit, newdata = test)
evaluate_model(y_pred = pred_values_test, y_true = test$target)

pred_values_train <- predict(svm.fit)
evaluate_model(y_pred = pred_values_train, y_true = train$target)

varImp(svm.fit, scale = T)

end_parallel_processing()

save_workspace("C:\\xxTemp\\advanced_ml_RData\\svm_cv_F1_lev1_scaled.RData")