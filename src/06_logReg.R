library(caret)

source("src/utils/data_preprocessing.R")
source("src/utils/evaluation.R")


# With caret (broken)
# logReg <- train(target ~ .,
#                  data = train,
#                  method = "glm",
#                  family = "binomial")

#non-caret
glm.logReg <- glm(
  formula = target ~ .,
  family  = binomial,
  data    = train
)

pred_values <- round(predict(glm.logReg, test, type   = "response"))

evaluate_model(y_pred = pred_values, y_true = test$target)
plot(glm)


