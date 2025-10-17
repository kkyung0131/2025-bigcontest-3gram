# === 2025 BIGCONTEST Team. 3gram===
# customer model code
# ==================================

# --- load library & data ---
library(tidymodels)
library(tidyverse)
library(bonsai)   
library(dplyr)
library(zoo)
library(VIM)
library(lubridate)

train <- read.csv("./bigcon_train_id.csv")  # data after basic preprocessing
test <- read.csv("./bigcon_test_id.csv")


# --- missing handling function ---
fill_missing_proportion <- function(df, value_cols, id_col = "id", time_col = "ym",
                                    knn_cat_cols = c("big_ind", "dong", "ym"),
                                    norm = TRUE, k = 5) {
  df_copy <- df %>%
    arrange(.data[[id_col]], .data[[time_col]])
  
  # (1) linear filling
  df_copy[value_cols] <- df_copy %>%
    group_by(.data[[id_col]]) %>%
    mutate(across(all_of(value_cols),
                  ~ na.approx(., x = .data[[time_col]], na.rm = FALSE))) %>%
    ungroup() %>%
    select(all_of(value_cols))
  
  # (2) forward fill / backward fill 
  df_copy[value_cols] <- df_copy %>%
    group_by(.data[[id_col]]) %>%
    mutate(across(all_of(value_cols), ~ na.locf(., na.rm = FALSE))) %>%
    mutate(across(all_of(value_cols), ~ na.locf(., fromLast = TRUE, na.rm = FALSE))) %>%
    ungroup() %>%
    select(all_of(value_cols))
  
  # (3) knn imputation
  if (any(is.na(df_copy[value_cols]))) {
    df_copy[knn_cat_cols] <- lapply(df_copy[knn_cat_cols], as.factor)
    knn_data <- df_copy[, c(knn_cat_cols, value_cols)]
    knn_result <- kNN(knn_data, variable = value_cols,
                      dist_var = knn_cat_cols, k = k, imp_var = FALSE, numFun = mean)
    df_copy[value_cols] <- knn_result[value_cols]
  }
  
  # (4) adjust to sum to 100
  if (norm) {
    df_copy <- df_copy %>%
      rowwise() %>%
      mutate(total_val = sum(c_across(all_of(value_cols)), na.rm = TRUE),
             across(all_of(value_cols),
                    ~ ifelse(!is.na(total_val) & total_val > 0, . * 100 / total_val, .))) %>%
      select(-total_val) %>%
      ungroup()
  }
  return(df_copy)
}


# --- function to create a target variable ---
assign_cust_group <- function(df, train_ref = NULL) {
  
  # for train dataset
  if (is.null(train_ref)) {
    unique_cust_q2 <- quantile(df$unique_cust_cat_mean, 0.50, na.rm = TRUE)
    re_cust_q2     <- quantile(df$re_cust_rat_mean, 0.50, na.rm = TRUE)
  } else {
    # for test dataset
    unique_cust_q2 <- quantile(train_ref$unique_cust_cat_mean, 0.50, na.rm = TRUE)
    re_cust_q2     <- quantile(train_ref$re_cust_rat_mean, 0.50, na.rm = TRUE)
  }
  
  df <- df %>%
    mutate(
      cust_pattern = case_when(
        unique_cust_cat_mean < unique_cust_q2 & re_cust_rat_mean >= re_cust_q2 ~ "1_Growth",
        unique_cust_cat_mean < unique_cust_q2 & re_cust_rat_mean < re_cust_q2  ~ "3_Trial",
        unique_cust_cat_mean >= unique_cust_q2 & re_cust_rat_mean >= re_cust_q2 ~ "2_Loyal",
        unique_cust_cat_mean >= unique_cust_q2 & re_cust_rat_mean < re_cust_q2  ~ "4_AtRisk",
      )
    )
  
  return(df)
}


# --- preprocess pipeline ---
process_pipeline <- function(df, mh = TRUE, prefix = "train", 
                             train_ref = NULL, create_target = TRUE) {

  df <- df %>% mutate(ym = as.Date(ym))
  
  # variables to handle missing values
  vars1 <- c("m_20_under_rat", "m_30_rat", "m_40_rat", "m_50_rat", "m_60_over_rat",
             "f_20_under_rat", "f_30_rat", "f_40_rat", "f_50_rat", "f_60_over_rat")
  vars2 <- c("new_cust_rat")   
  vars3 <- c("resid_cust_rat", "office_cust_rat", "move_cust_rat")
  
  if (mh) {
    df <- fill_missing_proportion(df, vars1)
    df <- fill_missing_proportion(df, vars2, norm = FALSE)  
    df <- fill_missing_proportion(df, vars3)
  }
  
  # create summary variables 
  df <- df %>%
    mutate(m_rat = ifelse(is.na(m_rat),
                          m_20_under_rat + m_30_rat + m_40_rat + m_50_rat + m_60_over_rat, m_rat),
           f_rat = ifelse(is.na(f_rat),
                          f_20_under_rat + f_30_rat + f_40_rat + f_50_rat + f_60_over_rat, f_rat),
           age_20_under_rat = ifelse(is.na(age_20_under_rat),
                                     m_20_under_rat + f_20_under_rat, age_20_under_rat),
           age_30_rat = ifelse(is.na(age_30_rat), m_30_rat + f_30_rat, age_30_rat),
           age_40_rat = ifelse(is.na(age_40_rat), m_40_rat + f_40_rat, age_40_rat),
           age_50_rat = ifelse(is.na(age_50_rat), m_50_rat + f_50_rat, age_50_rat),
           age_60_over_rat = ifelse(is.na(age_60_over_rat),
                                    m_60_over_rat + f_60_over_rat, age_60_over_rat)) %>%
    select(-all_of(vars1))
  
  # quarterly aggregation
  df_quarter <- df %>%
    mutate(
      ym = as.Date(ym),
      year = lubridate::year(ym),
      month = lubridate::month(ym),
      quarter = lubridate::quarter(ym),
      ym_quarter = paste0(year, quarter)
    )
  
  exclude_cols <- c("has_dlv_cat", "is_brand", "big_ind", "dong",
                    "id", "ym", "year", "month", "quarter", "ym_quarter")
  
  cols <- df_quarter %>%
    select(-all_of(exclude_cols)) %>%
    names()
  
  df_quarter_summary <- df_quarter %>%
    group_by(id, ym_quarter, has_dlv_cat, is_brand, big_ind, dong) %>%
    summarise(
      across(all_of(cols),
             list(mean = ~if (length(.) > 0) mean(., na.rm = TRUE) else NA_real_,
                  std  = ~if (length(.) == 0) { NA_real_ } else if (length(.) == 1) { 0 } else { sd(., na.rm = TRUE) }),
             .names = "{.col}_{.fn}"),
      .groups = "drop") %>%
    mutate(ym_quarter = as.integer(ym_quarter))
  
  # preprocessed external data
  df_move  <- read.csv("./external/df_move.csv")
  df_office<- read.csv("./external/df_office.csv")
  df_oc    <- read.csv("./external/df_open&close.csv")
  df_pop   <- read.csv("./external/df_pop_quarter.csv")
  df_rent  <- read.csv("./external/df_rent.csv")
  df_resid <- read.csv("./external/df_resid.csv")
  
  df_merged <- df_quarter_summary %>%
    left_join(df_pop, by = c("ym_quarter", "dong")) %>%
    left_join(df_rent,  by = c("ym_quarter", "dong")) %>%
    left_join(df_office, by = c("ym_quarter", "dong")) %>%
    left_join(df_move, by = c("ym_quarter", "dong")) %>%
    left_join(df_resid, by = c("ym_quarter", "dong")) %>%
    left_join(df_oc, by = c("ym_quarter", "dong", "big_ind"))
  
  # transform data type & na.omit 
  if (!is.null(train_ref)) {
    df_merged <- df_merged %>%
      na.omit() %>%
      mutate(big_ind = factor(big_ind, levels = unique(train_ref$big_ind)),
             has_dlv_cat = as.factor(has_dlv_cat), 
             is_brand = as.factor(is_brand))
  } else {
    df_merged <- df_merged %>%
      na.omit() %>%
      mutate(big_ind = factor(big_ind),
             has_dlv_cat = as.factor(has_dlv_cat), 
             is_brand = as.factor(is_brand))
  }
  
  # creating target variables (for train dataset)
  if (create_target) {
    df_with_cust_pattern <- assign_cust_group(df_merged, train_ref = train_ref)
    df_with_cust_pattern$cust_pattern <- as.factor(df_with_cust_pattern$cust_pattern)
    #write.csv(df_with_cust_pattern, paste0(prefix, "_for_cust_pattern_4class.csv"), row.names = FALSE)
    return(df_with_cust_pattern)
  } else {
    # for test dataset
    return(df_merged)
  }
}


# --- data preprocessing ---
train_processed <- process_pipeline(train, mh = TRUE, prefix = "train", train_ref = NULL, create_target = TRUE)
test_processed <- process_pipeline(test, mh = FALSE, prefix = "test", train = train_processed, create_target = TRUE)


# --- model fitting by quarters ---
train_quarterly_models <- function(train_df) {
  
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
  return(models)
}
models <- train_quarterly_models(train_processed)  # my final models!


# --- prediction & scoring function ---
predict_ensemble_score <- function(models, test_df) {

  rowMode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  # quarterly prediction ensemble using majority vote
  preds_class <- map(models, ~ predict(.x$model, test_df, type = "class")) %>%
    reduce(bind_cols)
  ensemble_class <- apply(preds_class, 1, rowMode)
  
  # ensemble quarterly prediction probabilities using the mean
  preds_prob <- map(models, ~ predict(.x$model, test_df, type = "prob")) %>%
    reduce(`+`) / length(models)
  
  # calculate risk score: sum(k * P_k)
  prob_cols <- preds_prob %>% select(starts_with(".pred_"))
  
  class_levels <- gsub("^\\.pred_", "", names(prob_cols))  # "1_Growth_Extreme", ...
  class_index <- seq_along(class_levels)                   # 1:4
  
  risk_score <- as.numeric(as.matrix(prob_cols) %*% class_index)
  
  # 0~1 scaling 
  risk_score_scaled <- (risk_score - 1) / (4 - 1)
  
  results <- bind_cols(
    test_df %>% select(id, ym_quarter),
    prob_cols,
    tibble(
      ensemble_class = ensemble_class,
      risk_score = risk_score,
      risk_score_scaled = risk_score_scaled
    )
  )
  cust_score = risk_score_scaled
  
  return(c(results, cust_score))
}
preds_with_score <- predict_ensemble_score(models, test_processed) %>%
  bind_cols(test_processed$cust_pattern)


# --- final performance evaluation ---
metrics_ensemble <- metric_set(accuracy, f_meas)(preds_with_score, truth = cust_pattern, estimate = ensemble_class)
print(metrics_ensemble)

cm <- conf_mat(preds_with_score, truth = cust_pattern, estimate = ensemble_class)
autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Ensemble Confusion Matrix",
    x = "actual",
    y = "predict"
  ) +
  theme_minimal()


# --- train final model on the full dataset ---
df <- read.csv("./bigcon_all.csv")  # train + test dataset
df_processed <- process_pipeline_cust(df, mh = TRUE, prefix = "all", train_ref = NULL, create_target = NULL)
models <- train_quarterly_models(df_processed)


# --- final model saving ---
save_models <- function(models, path = "./models_list.rds") {
  saveRDS(models, path)
  invisible(path)
}
#save_models(models, "./my_cust_models.rds")




