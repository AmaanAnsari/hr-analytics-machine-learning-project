library(gbm)
library(caret)

source("src/utils/data_preprocessing.R")
source("src/utils/evaluation.R")
source("src/utils/parallel_processing.R")

start_parallel_processing()


#Grid for the Possible Hyper-Parameters
gbmGrid <-  expand.grid(interaction.depth = c(9, 11, 13), 
                        n.trees = seq(300, 700, 200),
                        shrinkage = c(0.001, 0.011, 0.021),
                        n.minobsinnode = 10)

ctrl <- trainControl(method = "cv", number = 5, summaryFunction = calc_f1_score_arith_for_caret, allowParallel = T)


gbmCV <- train(as.character(target) ~ ., 
                 data = train, 
                 method = "gbm", 
                 trControl = ctrl,
                 tuneGrid = gbmGrid,
                 metric = "F1_lev1")

gbmCV

# old code here
boosting.fit <- gbm(
  formula           = as.character(target) ~ .,
  data              = train,
  distribution      = "bernoulli",          
  n.trees           = 300,
  interaction.depth = 9,                  
  shrinkage         = 0.011
)

pred_values <- round(predict(boosting.fit, test, type="response"))

evaluate_model(y_pred = pred_values, y_true = test$target)
summary.gbm(boosting.fit)

end_parallel_processing()

save_workspace("hier pfad einfügen")
