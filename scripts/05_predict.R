# ============================================================
# 05_predict.R
# ------------------------------------------------------------
# Description: Functions to predict sales risk score, sales category,
#              customer pattern, and market area risk score using
#              trained models. Ensemble and risk score calculation included.
# ============================================================


#' Predict sales risk score using ensemble of quarterly models
#'
#' @description
#'   Generates predictions for sales risk score for new data.
#'   Predictions from each quarterly model are averaged (soft voting)
#'
#' @param models List of trained LightGBM models for each quarter
#' @param new_data New data to predict
#' @return Dataframe with id, ym_quarter, and ensemble sales risk score
predict_sales_risk_score <- function(models, new_data) {
  preds <- map(models, ~ predict(.x$model, new_data, type = "numeric")) %>%
    reduce(bind_cols)
  
  names(preds) <- paste0("pred_", seq_along(models))
  
  preds_ensemble <- preds %>%
    rowwise() %>%
    mutate(sales_risk_pred = mean(c_across(starts_with("pred_")), na.rm = TRUE)) %>%
    ungroup() %>%
    bind_cols(new_data %>% select(id, ym_quarter))
  
  return(preds_ensemble)
}


#' Predict sales amount category using ensemble of quarterly models
#'
#' @description
#'   Generates predictions for sales amount category for new data.
#'   Predictions from all models are averaged, then clipped to [1,6].
#'
#' @param models List of trained XGBoost models for each quarter
#' @param newdata New data to predict
#' @return Dataframe with median sales category, ym_quarter, and ensemble prediction
predict_sales_amt_cat <- function(models, newdata) {
  preds_all <- map(models, ~ predict(.x$model, newdata)) %>%
    reduce(bind_cols)
  
  preds_ensemble <- preds_all %>%
    mutate(ensemble = rowMeans(across(everything()))) %>%
    bind_cols(newdata %>% select(sales_amt_cat_median, ym_quarter)) %>%
    mutate(ensemble = pmax(pmin(ensemble, 6), 1))  # clipped to [1,6]
  
  return(preds_ensemble)
}


#' Predict customer pattern and calculate customer risk score
#'
#' @description
#'   1. Predict classes for each model (for ensemble majority vote)
#'   2. Predict probabilities for each model (for risk score calculation, soft voting)
#'   3. Compute ensemble class as row mode (hard voting) 
#'   4. Calculate risk score = sum(k * P_k)
#'   5. Scale risk score to [0,1]
#'
#' @param models List of trained LightGBM classification models
#' @param new_data New data to predict
#' @return Dataframe with id, ym_quarter, class probabilities, ensemble class,
#'         risk score, and scaled risk score
predict_cust_score <- function(models, new_data) {

  rowMode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  preds_class <- map(models, ~ predict(.x$model, new_data, type = "class")) %>%
    reduce(bind_cols)
  
  preds_prob <- map(models, ~ predict(.x$model, new_data, type = "prob")) %>%
    reduce(`+`) / length(models)
  
  ensemble_class <- apply(preds_class, 1, rowMode)
  
  prob_cols <- preds_prob %>% select(starts_with(".pred_"))
  
  class_levels <- gsub("^\\.pred_", "", names(prob_cols))  # "1_Growth_Extreme", ...
  class_index <- seq_along(class_levels)                   # 1:4
  
  risk_score <- as.numeric(as.matrix(prob_cols) %*% class_index)
  
  risk_score_scaled <- (risk_score - 1) / (4 - 1)
  
  results <- bind_cols(
    new_data %>% select(id, ym_quarter),
    prob_cols,
    tibble(
      ensemble_class = ensemble_class,
      risk_score = risk_score,
      risk_score_scaled = risk_score_scaled
    )
  )
  
  return(results)
}


#' Predict market area risk score
#'
#' @description
#'   Predicts market area classification and calculates a weighted
#'   risk score. Scales final risk score to [0,1].
#'
#' @param model Trained market area model
#' @param new_data New data to predict
#' @return Dataframe with scaled market area risk score
predict_mkt_score <- function(model, new_data){
  preds <- predict(model, new_data)
  probs <- predict(model, new_data, type = "prob")
  
  risk_weights <- c(4, 3, 1, 2)
  
  risk_score <- probs %>%
    mutate(
      risk_score = .pred_1 * risk_weights[1] +
        .pred_2 * risk_weights[2] +
        .pred_3 * risk_weights[3] +
        .pred_4 * risk_weights[4]
    ) %>%
    select(risk_score)
  
  area_min_max_scale <- function(x) {
    (x - 1) / (4 - 1)
  }
  
  area_final_scores <- area_min_max_scale(risk_score)
  return(area_final_scores)
}

