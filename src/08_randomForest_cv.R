library(randomForest)
library(dplyr)
library(e1071)
library(stats)
library(caret)

source("src/utils/data_preprocessing.R")
source("src/utils/evaluation.R")
source("src/utils/parallel_processing.R")

start_parallel_processing()

# Define the 5-fold cross-validation control
ctrl <- trainControl(method = "cv", number = 5, summaryFunction = calc_f1_score_arith_for_caret, allowParallel = T)
netz <- expand.grid(mtry = 1:13)

# remove constant vars
# constant_vars <- sapply(train, function(x) length(unique(x)) == 1)
# train <- select(train, -which(constant_vars))

# Train the SVM model using 5-fold cross-validation
randomForest.cv <- train(target ~ .,
                         data = train,
                         method = "rf",
                         trControl = ctrl,
                         tuneGrid = netz,
                         metric = "F1_lev1",
                         ntree = 1000, # ggf. 500
                         maximize = TRUE,
                         na.action = na.omit)

#stopCluster(cl)
print(randomForest.cv)
end_parallel_processing()

save_workspace("C:\\xxTemp\\advanced_ml_RData\\randomForest_cv_F1_lev1.RData")