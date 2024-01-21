library(rpart)
library(rpart.plot)

source("src/utils/data_preprocessing.R")
source("src/utils/evaluation.R")

tree.fit <- rpart(
  formula = target~.,
  data    = train
)

pred_values <- predict(tree.fit, test, type="class")

evaluate_model(y_pred = pred_values, y_true = test$target)

rpart.plot(tree.fit)

