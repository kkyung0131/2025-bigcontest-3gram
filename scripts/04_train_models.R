# ============================================================
# 04_train_models.R
# ------------------------------------------------------------
# Description: Train final models for quarterly sales risk, sales level,
#              customer pattern, and market area using tidymodels.
#              Recipe is used for feature selection and dummy encoding.
# ============================================================


#' Train quarterly sales risk score models
#'
#' @description
#'   Trains a LightGBM regression model for sales risk score
#'   separately for each quarter in the training data.
#'
#' @param train_df Preprocessed training dataframe with sales risk score
#' @return List of fitted models for each quarter
train_quarterly_sales_risk_models <- function(train_df) {
  
  lgb_model <- boost_tree(
    learn_rate = 0.1,
    tree_depth = 6,
    min_n = 5,
    sample_size = 0.8,
    mtry = 35,
    trees = 500
  ) %>% 
    set_engine("lightgbm") %>%
    set_mode("regression")
  
  quarters <- sort(unique(train_df$ym_quarter))
  
  fit_quarter_model <- function(q) {
    training_q <- train_df %>% filter(ym_quarter == q)
    
    lgb_rec <- recipe(sales_risk_score ~ ., data = training_q) %>%
      update_role(
        id, ym_quarter, big_ind, dong, sales_amt_cat_mean, sales_amt_cat_std,
        sales_cnt_cat_mean, sales_cnt_cat_std, aov_cat_mean, aov_cat_std,
        ind_sales_amt_rat_mean, ind_sales_amt_rat_std, ind_sales_cnt_rat_mean,
        ind_sales_cnt_rat_std, 
        new_role = "ID"
      ) %>%
      step_dummy(all_nominal_predictors())  
    
    lgb_wflow <- workflow() %>%
      add_model(lgb_model) %>%
      add_recipe(lgb_rec)
    
    fit <- fit(lgb_wflow, training_q)
    
    list(quarter = q, model = fit)
  }
  models <- map(quarters, fit_quarter_model)
  #saveRDS(models, "my_sales_risk_models.rds")
}


#' Train quarterly sales level models
#'
#' @description
#'   Trains an XGBoost regression model for sales amount categories
#'   separately for each quarter in the training data.
#'
#' @param train_df Preprocessed training dataframe with sales_amt_cat_median
#' @return List of fitted models for each quarter
train_quarterly_sales_models <- function(train_df) {
  xgb_model <- boost_tree(
    trees = 1000,
    tree_depth = 6,
    learn_rate = 0.05,
    loss_reduction = 0,
    min_n = 2
  ) %>%
    set_engine("xgboost") %>% set_mode("regression")
  
  quarters <- sort(unique(train_df$ym_quarter))
  
  fit_quarter_model <- function(q) {
    training_q <- train_df %>% filter(ym_quarter == q)
    
    rf_rec <- recipe(sales_amt_cat_median ~ ., data = training_q) %>%
      update_role(
        id, ym_quarter, big_ind, dong,
        has_dlv_cat, is_brand, pop_all_mean, pop_all_std,
        sales_cnt_cat_mean,sales_cnt_cat_std,
        aov_cat_mean, aov_cat_std, ind_sales_amt_rat_mean, ind_sales_amt_rat_std,
        ind_sales_cnt_rat_mean, ind_sales_amt_rat_std,
        #ind_rank_rat_mean, ind_rank_rat_std,
        #unique_cust_cat_mean, unique_cust_cat_std,
        #ind_cancel_rat_mean, ind_cancel_rat_std,
        new_role = "ID"
      ) %>%
      step_dummy(all_nominal_predictors())
    
    rf_wflow <- workflow() %>% add_model(xgb_model) %>% add_recipe(rf_rec)
    fit <- fit(rf_wflow, training_q)
    
    list(quarter = q, model = fit)
  }
  
  models <- map(quarters, fit_quarter_model)
  #saveRDS(models, "my_sales_models.rds")
  return(models)
}



#' Train quarterly customer pattern classification models
#'
#' @description
#'   Trains a LightGBM classification model to predict 4-class customer patterns
#'   separately for each quarter in the training data.
#'
#' @param train_df Preprocessed training dataframe with cust_pattern factor
#' @return List of fitted models for each quarter with names
train_quarterly_cust_models <- function(train_df) {
 
  set.seed(2025)
  train_df$cust_pattern = as.factor(train_df$cust_pattern)
  quarters <- sort(unique(train_df$ym_quarter))
  
  lgb_model <- boost_tree(
    learn_rate = 0.1,
    tree_depth = 6,
    min_n = 5,
    sample_size = 0.8,
    mtry = 35,
    trees = 500
  ) %>% set_engine("lightgbm") %>% set_mode("classification")
  
  fit_quarter_model <- function(q) {
    training_q <- train_df %>% filter(ym_quarter == q)
    
    lgb_rec <- recipe(cust_pattern ~ ., data = training_q) %>%
      update_role(
        id, ym_quarter, re_cust_rat_mean, re_cust_rat_std,
        unique_cust_cat_mean, unique_cust_cat_std,
        has_dlv_cat, is_brand, pop_all_mean, pop_all_std,
        contains(c("office_num", "resid_num")),
        new_role = "ID"
      ) %>%
      step_dummy(all_nominal_predictors())
    
    lgb_wflow <- workflow() %>%
      add_model(lgb_model) %>%
      add_recipe(lgb_rec)
    
    fit <- fit(lgb_wflow, training_q)
    list(quarter = q, model = fit)
  }
  
  models <- map(quarters, fit_quarter_model)
  names(models) <- map_chr(models, "quarter")
  
  #saveRDS(models, "my_cust_models_4class.rds")
  return(models)
}


#' Train market area classification model
#'
#' @description
#'   Trains an XGBoost classification model for market area status
#'   using the entire training dataframe.
#'
#' @param train_df Preprocessed training dataframe with mkt_status
#' @return Fitted workflow object
train_quarterly_mkt_models <- function(train_df) {
  xgb_spec <- boost_tree(
    trees = 1000, mtry = 30, min_n = 10, learn_rate = 0.01) %>% 
    set_engine("xgboost") %>% 
    set_mode("classification")
  
  xgb_recipe <- recipe(mkt_status ~ ., data = train_df) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_naomit(all_predictors(), skip = T)
  
  xgb_wflow <- workflow() %>%
    add_model(xgb_spec) %>%
    add_recipe(xgb_recipe)
  
  xgb_fit <- fit(xgb_wflow, train_df)
  
  #saveRDS(xgb_fit, "my_mkt_models.rds")
  return(xgb_fit)
}

