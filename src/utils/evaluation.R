library(MLmetrics)
library(tools)

evaluate_model <- function (y_pred, y_true) {
  print("Confusion Matrix: ")
  print(ConfusionMatrix(y_pred = y_pred, y_true = y_true))

  f1_for_1 <- F1_Score(y_pred = y_pred, y_true = y_true, positive = "1")
  f1_for_0 <- F1_Score(y_pred = y_pred, y_true = y_true, positive = "0")

  if(is.na(f1_for_1)) {
    f1_for_1 <- 0
  }
  if(is.na(f1_for_0)) {
    f1_for_0 <- 0
  }

  print(paste0("Accuracy: ", Accuracy(y_pred = y_pred, y_true = y_true)))

  print(paste0("Recall fuer 1: ", Recall(y_pred = y_pred, y_true = y_true, positive = "1")))
  print(paste0("Precision fuer 1: ", Precision(y_pred = y_pred, y_true = y_true, positive = "1")))

  print(paste0("Recall fuer 0: ", Recall(y_pred = y_pred, y_true = y_true, positive = "0")))
  print(paste0("Precision fuer 0: ", Precision(y_pred = y_pred, y_true = y_true, positive = "0")))

  print(paste0("F1_Score fuer 1: ", f1_for_1))
  print(paste0("F1_Score fuer 0: ", f1_for_0))

  print(paste0("Arit (combined) F1-Score: ", (f1_for_0 + f1_for_1) / 2))
  print(paste0("Harm (combined) F1-Score: ", 2 / ((1 / f1_for_0) +  (1/ f1_for_1))))
}

calc_f1_score_arit <- function(y_pred, y_true, positive_str="1", negative_str="0") {
  f1_for_1 <- F1_Score(y_pred = y_pred, y_true = y_true, positive = positive_str)
  f1_for_0 <- F1_Score(y_pred = y_pred, y_true = y_true, positive = negative_str)

  if(is.na(f1_for_1)) {
    f1_for_1 <- 0
  }
  if(is.na(f1_for_0)) {
    f1_for_0 <- 0
  }

  return((f1_for_0 + f1_for_1) / 2)
}

calc_f1_score_for_caret <- function(data, lev = NULL, model = NULL) {
  f1_val <- MLmetrics::F1_Score(y_pred = data$pred,
                                y_true = data$obs,
                                positive = lev[1])
  c(F1 = f1_val)
}

calc_f1_score_arith_for_caret <- function(data, lev = NULL, model = NULL) {

  f1_val_lev1 <- MLmetrics::F1_Score(y_pred = data$pred,
                                y_true = data$obs,
                                positive = "1")
  f1_val_lev0 <- MLmetrics::F1_Score(y_pred = data$pred,
                                y_true = data$obs,
                                positive = "0")

  acc <- MLmetrics::Accuracy(y_pred = data$pred, y_true = data$obs)

  #print(f1_val_lev1)
  #print(f1_val_lev0)
  if(is.na(f1_val_lev1)) {
    f1_val_lev1 <- 0
  }
  if(is.na(f1_val_lev0)) {
    f1_val_lev0 <- 0
  }
  #assert("Amaan TEST: both f1_scores should not be equal! check calc_f1_score_arith_for_caret function",f1_val_lev1 != f1_val_lev0)
  c(F1_arith = (f1_val_lev1 + f1_val_lev0) / 2, F1_lev1 = f1_val_lev1, F1_lev0 = f1_val_lev0, Accuracy = acc)
}

save_workspace <- function(filepath) {
  full_filename <- paste0(file_path_sans_ext(filepath), format(Sys.time(),'_%Y%m%d_%H-%M-%S'), '.', file_ext(filepath))
  save.image(full_filename)
}