# ============================================================
# 03_data_pipeline.R
# ------------------------------------------------------------
# Description: Data preprocessing pipeline for the training/test dataset,
#              including missing value handling and quarterly aggregation.
# ============================================================


#' Fill missing values in numeric columns
#'
#' @description
#'   Performs missing value imputation in several steps:
#'   1. Linear interpolation by time within each id
#'   2. Last observation carried forward and backward within each id
#'   3. KNN imputation using specified categorical columns if missing values remain after interpolation
#'   4. Optional normalization to make row sums 100
#'
#' @param df Input dataframe (only for train dataset)
#' @param value_cols Columns to impute
#' @param id_col ID column (default: "id")
#' @param time_col Time column (default: "ym")
#' @param knn_cat_cols Categorical columns for KNN distance (default: c("big_ind", "dong", "ym"))
#' @param norm Whether to normalize values so that row sums are 100 (default: TRUE)
#' @param k Number of neighbors for KNN imputation (default: 5)
#' @return Dataframe with missing values filled and optionally normalized
fill_missing_proportion <- function(df, value_cols, id_col = "id", time_col = "ym",
                                    knn_cat_cols = c("big_ind", "dong", "ym"),
                                    norm = TRUE, k = 5) {
  df_copy <- df %>%
    arrange(.data[[id_col]], .data[[time_col]])
  
  # 1. Linear interpolation간
  df_copy[value_cols] <- df_copy %>%
    group_by(.data[[id_col]]) %>%
    mutate(across(all_of(value_cols),
                  ~ na.approx(., x = .data[[time_col]], na.rm = FALSE))) %>%
    ungroup() %>%
    select(all_of(value_cols))
  
  # 2. Carry forward and backward imputation
  df_copy[value_cols] <- df_copy %>%
    group_by(.data[[id_col]]) %>%
    mutate(across(all_of(value_cols), ~ na.locf(., na.rm = FALSE))) %>%
    mutate(across(all_of(value_cols), ~ na.locf(., fromLast = TRUE, na.rm = FALSE))) %>%
    ungroup() %>%
    select(all_of(value_cols))
  
  # 3. KNN imputation if any NA remains
  if (any(is.na(df_copy[value_cols]))) {
    df_copy[knn_cat_cols] <- lapply(df_copy[knn_cat_cols], as.factor)
    knn_data <- df_copy[, c(knn_cat_cols, value_cols)]
    knn_result <- kNN(knn_data, variable = value_cols,
                      dist_var = knn_cat_cols, k = k, imp_var = FALSE, numFun = mean)
    df_copy[value_cols] <- knn_result[value_cols]
  }
  
  # 4. Optional normalization to sum to 100
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


#' Aggregate data by quarter
#'
#' @description
#'   Summarizes the data at the quarterly level.
#'   Computes mean and standard deviation for numeric columns.
#'   Optional median aggregation for sales_amt_cat.
#'
#' @param df_quarter Input dataframe with quarterly information
#' @param median Whether to compute median for sales_amt_cat (default: FALSE)
#' @return Quarterly aggregated dataframe
summarize_quarter <- function(df_quarter, median = FALSE) {
  
  if (!median) {
    exclude_cols <- c("has_dlv_cat", "is_brand", "big_ind", "dong",
                      "id", "ym", "year", "month", "quarter", "ym_quarter")
    
    cols <- df_quarter %>%
      select(-all_of(exclude_cols)) %>%
      names()
    
    df_quarter_summary <- df_quarter %>%
      group_by(id, ym_quarter, has_dlv_cat, is_brand, big_ind, dong) %>%
      summarise(
        across(all_of(cols),
               list(
                 mean = ~if (length(.) > 0) mean(., na.rm = TRUE) else NA_real_,
                 std  = ~if (length(.) == 0) NA_real_ else if (length(.) == 1) 0 else sd(., na.rm = TRUE)
               ),
               .names = "{.col}_{.fn}"),
        .groups = "drop"
      ) %>%
      mutate(ym_quarter = as.integer(ym_quarter))
    
  } else {
    exclude_cols <- c("has_dlv_cat", "is_brand", "big_ind", "dong", 
                      "id", "ym", "year", "month", "quarter", "ym_quarter", "sales_amt_cat")
    
    cols <- df_quarter %>%
      select(-all_of(exclude_cols)) %>%
      names()
    
    df_quarter_summary <- df_quarter %>%
      group_by(id, ym_quarter, has_dlv_cat, is_brand, big_ind, dong) %>%
      summarise(
        across(all_of(cols),
               list(
                 mean = ~if (length(.) > 0) mean(., na.rm = TRUE) else NA_real_,
                 std  = ~if (length(.) > 1) sd(., na.rm = TRUE) else NA_real_
               ),
               .names = "{.col}_{.fn}"),
        sales_amt_cat_median = median(sales_amt_cat, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  return(df_quarter_summary)
}


#' Common preprocessing pipeline
#'
#' @description
#'   Performs standard preprocessing for the dataset, including:
#'   1. Aggregation of missing male/female and age group ratios
#'   2. Quarterly date transformation (year, month, quarter, ym_quarter)
#'   3. Quarterly summarization using `summarize_quarter()`
#'   4. Merge with external datasets (move, office, resid, open/close, population, rent)
#'   5. Remove rows with remaining missing values
#'
#' @param df Input dataframe
#' @param median Whether to compute median for sales_amt_cat during quarterly summarization (default: FALSE)
#' @return Preprocessed dataframe merged with external datasets and ready for modeling
commom_process <- function(df, median = FALSE) {
  
  vars1 <- c("m_20_under_rat", "m_30_rat", "m_40_rat", "m_50_rat", "m_60_over_rat",
             "f_20_under_rat", "f_30_rat", "f_40_rat", "f_50_rat", "f_60_over_rat")
  
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
  
  df_quarter <- df %>%
    mutate(
      ym = as.Date(ym),
      year = lubridate::year(ym),
      month = lubridate::month(ym),
      quarter = lubridate::quarter(ym),
      ym_quarter = paste0(year, quarter)
    )
  
  df_quarter_summary <- summarize_quarter(df_quarter, median = median)
  
  # 외부 파일 로드 (사용환경에 맞춰 경로/파일 존재 확인)
  df_move  <- read.csv("./data/external/df_move.csv")
  df_office<- read.csv("./data/external/df_office.csv")
  df_oc    <- read.csv("./data/external/df_open&close.csv")
  df_pop   <- read.csv("./data/external/df_pop_quarter.csv")
  df_rent  <- read.csv("./data/external/df_rent.csv")
  df_resid <- read.csv("./data/external/df_resid.csv")
  
  df_merged <- df_quarter_summary %>%
    left_join(df_pop, by = c("ym_quarter", "dong")) %>%
    left_join(df_rent,  by = c("ym_quarter", "dong")) %>%
    left_join(df_office, by = c("ym_quarter", "dong")) %>%
    left_join(df_move, by = c("ym_quarter", "dong")) %>%
    left_join(df_resid, by = c("ym_quarter", "dong")) %>%
    left_join(df_oc, by = c("ym_quarter", "dong", "big_ind"))
  
  df_merged <- na.omit(df_merged)
  return(df_merged)
}



# ============================================================
# Sales Risk Score Pipeline
# ------------------------------------------------------------
# Description: Preprocessing and target creation pipeline for sales risk score modeling.
#              - Train data: missing value imputation enabled (mh = TRUE)
#              - Test data: missing value imputation disabled (mh = FALSE)
#              - New data: mh = FALSE / target not created
#              - Risk score model: median = FALSE / target created
#              - Sales category model: median = TRUE / target not created
# ============================================================


#' Assign sales risk score based on current and previous sales
#'
#' @description
#'   Calculates a sales risk score using two components:
#'   1. Risk from current sales level (normalized to [0,1])
#'   2. Risk from change in sales compared to previous period (normalized to [0,1])
#'   Final score is a weighted sum of the two components.
#'
#' @param df Dataframe processed through the sales pipeline (median = FALSE)
#' @param w1 Weight for current sales risk (default: 0.666)
#' @param w2 Weight for change in sales risk (default: 0.334)
#' @return Dataframe with new column `sales_risk_score`
assign_sales_risk_score <- function(df, w1 = 0.666, w2 = 0.334) {
  
  calculate_risk_score <- function(C, P, w1 = 0.666, w2 = 0.334) {
    risk_from_prediction <- (P - 1) / 5
    risk_from_change <- (P - C + 5) / 10
    
    total_score <- w1 * risk_from_prediction + w2 * risk_from_change
    return(total_score)
  }
  
  df <- df %>%
    arrange(id, ym_quarter) %>%
    group_by(id) %>%
    mutate(
      prev_sales = lag(sales_amt_cat_mean), 
      sales_risk_score = calculate_risk_score(prev_sales, sales_amt_cat_mean, w1, w2)
    ) %>%
    ungroup()
  
  # Remove first quarter (prev_sales NA)
  df <- df %>% filter(!is.na(sales_risk_score)) %>% select(-prev_sales)
  
  return(df)
}


#' Full processing pipeline for sales risk score
#'
#' @description
#'   Applies missing value imputation, common preprocessing, and optional
#'   target creation for sales risk score modeling.
#'
#' @param df Input dataframe
#' @param mh Whether to apply missing value handling (default: TRUE)
#' @param median Whether to summarize quarterly data using median (default: FALSE)
#' @param create_target Whether to create sales risk score (default: TRUE)
#' @param prefix Prefix for output file name (optional)
#' @param w1 Weight for current sales risk (default: 0.666)
#' @param w2 Weight for sales change risk (default: 0.334)
#' @return Preprocessed dataframe, with `sales_risk_score` if `create_target = TRUE`
process_pipeline_sales <- function(df, mh = TRUE, median = FALSE, 
                                   create_target = TRUE, prefix = "train",
                                   w1 = 0.666, w2 = 0.334) {
  
  df <- df %>%
    mutate(ym = as.Date(ym))
  
  vars1 <- c("m_20_under_rat", "m_30_rat", "m_40_rat", "m_50_rat", "m_60_over_rat",
             "f_20_under_rat", "f_30_rat", "f_40_rat", "f_50_rat", "f_60_over_rat")
  vars2 <- c("re_cust_rat", "new_cust_rat")   
  vars3 <- c("resid_cust_rat", "office_cust_rat", "move_cust_rat")
  
  if (mh) {
    df <- fill_missing_proportion(df, vars1)
    df <- fill_missing_proportion(df, vars2, norm = FALSE)
    df <- fill_missing_proportion(df, vars3)
  }
  
  df <- commom_process(df, median = median) %>% na.omit()
  
  if (create_target) {
    df_with_risk <- assign_sales_risk_score(df, w1 = w1, w2 = w2)
    #write.csv(df_with_risk, paste0(prefix, "_for_sales.csv"), row.names = FALSE)
    return(df_with_risk)
  } else {
    return(df)
  }
}



# ============================================================
# Customer Pattern Pipeline
# ------------------------------------------------------------
# Description: Preprocessing and target creation pipeline for customer group modeling.
#              - Train data: missing value imputation enabled (mh = TRUE)
#              - Test data: missing value imputation disabled (mh = FALSE)
#              - Target variable (`cust_pattern`) created if `create_target = TRUE`
# ============================================================


#' Assign customer pattern based on median split
#'
#' @description
#'   Creates a 4-class customer pattern (`cust_pattern`) based on:
#'   - unique customer category (`unique_cust_cat_mean`)
#'   - returning customer ratio (`re_cust_rat_mean`)
#'   Cut-off values are computed from the training reference if provided.
#'
#' @param df Dataframe processed through the customer pipeline
#' @param train_ref Optional dataframe to provide reference cut-offs (for test set)
#' @return Dataframe with new column `cust_pattern`
assign_cust_group <- function(df, train_ref = NULL) {
  
  # Determine median cut-offs
  if (is.null(train_ref)) {
    unique_cust_q2 <- quantile(df$unique_cust_cat_mean, 0.50, na.rm = TRUE)
    re_cust_q2     <- quantile(df$re_cust_rat_mean, 0.50, na.rm = TRUE)
  } else {
    unique_cust_q2 <- quantile(train_ref$unique_cust_cat_mean, 0.50, na.rm = TRUE)
    re_cust_q2     <- quantile(train_ref$re_cust_rat_mean, 0.50, na.rm = TRUE)
  }
  
  # Assign customer pattern
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


#' Full processing pipeline for customer group
#'
#' @description
#'   Applies missing value imputation, common preprocessing, variable conversion,
#'   and optional target creation for customer pattern modeling.
#'
#' @param df Input dataframe
#' @param mh Whether to apply missing value handling (default: TRUE)
#' @param median Whether to summarize quarterly data using median (default: FALSE)
#' @param prefix Prefix for output file name (optional)
#' @param train_ref Optional training reference for cut-off calculation (used for test set)
#' @param create_target Whether to create `cust_pattern` target variable (default: TRUE)
#' @return Preprocessed dataframe, with `cust_pattern` if `create_target = TRUE`
process_pipeline_cust <- function(df, mh = TRUE, median = FALSE,
                                  prefix = "train", train_ref = NULL, create_target = TRUE) {

  df <- df %>%
    mutate(ym = as.Date(ym))
  
  vars1 <- c("m_20_under_rat", "m_30_rat", "m_40_rat", "m_50_rat", "m_60_over_rat",
             "f_20_under_rat", "f_30_rat", "f_40_rat", "f_50_rat", "f_60_over_rat")
  vars2 <- c("new_cust_rat")   
  vars3 <- c("resid_cust_rat", "office_cust_rat", "move_cust_rat")
  
  if (mh) {
    df <- fill_missing_proportion(df, vars1)
    df <- fill_missing_proportion(df, vars2, norm = FALSE)
    df <- fill_missing_proportion(df, vars3)
  }
  
  df_merged <- commom_process(df, median = median)
  
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
  
  if (create_target) {
    df_with_cust_pattern <- assign_cust_group(df_merged, train_ref = train_ref)
    df_with_cust_pattern$cust_pattern <- as.factor(df_with_cust_pattern$cust_pattern)
    #write.csv(df_with_cust_pattern, paste0(prefix, "_for_cust.csv"), row.names = FALSE)
    return(df_with_cust_pattern)
  } else {
    return(df_merged)
  }
}



# ============================================================
# Market Area Pipeline
# ------------------------------------------------------------
# Description: Preprocessing and target creation pipeline for market area modeling.
#              - Creates 4-class clusters using K-means based on opening/closing stats
#              - Adds lag features, rolling statistics, and differences for selected top variables
# ============================================================

#' Assign market area cluster using K-means
#'
#' @description
#'   Performs K-means clustering on selected area variables to classify market areas
#'   into a specified number of clusters (default 4).
#'
#' @param df Input dataframe
#' @param centers Number of clusters (default: 4)
#' @param seed Random seed for reproducibility (default: 123)
#' @return Dataframe with new column `cluster`(=mkt_status) as factor
assign_cluster_area <- function(df, centers = 4, seed = 123) {
  
  # Select variables for clustering
  area_vars <- c("open_rat", "close_rat", "open_cnt", "close_cnt")
  area_cluster <- df %>% select(all_of(area_vars))
  
  # Scale variables
  area_cluster_scaled <- scale(area_cluster)
  
  # K-means clustering
  set.seed(seed)
  kmeans_result <- kmeans(area_cluster_scaled, centers = centers, nstart = 25)
  
  df$cluster <- factor(kmeans_result$cluster)
  
  return(df)
}


#' Full processing pipeline for market area model
#'
#' @description
#'   Applies missing value handling, common preprocessing, and optional cluster assignment.
#'   Additionally generates lag features, rolling statistics (mean & SD), and differences
#'   for top selected variables.
#'
#' @param df Input dataframe
#' @param mh Whether to apply missing value handling (default: TRUE)
#' @param median Whether to summarize quarterly data using median (default: FALSE)
#' @param prefix Prefix for output file (optional)
#' @param create_target Whether to create cluster target (default: TRUE)
#' @return Preprocessed dataframe with lag, rolling stats, and difference features
process_pipeline_mkt <- function(df, mh = TRUE, median = FALSE, 
                                 prefix = "train", create_target = TRUE) {
  
  df <- df %>%
    mutate(ym = as.Date(ym))
  
  vars1 <- c("m_20_under_rat", "m_30_rat", "m_40_rat", "m_50_rat", "m_60_over_rat",
             "f_20_under_rat", "f_30_rat", "f_40_rat", "f_50_rat", "f_60_over_rat")
  vars2 <- c("re_cust_rat", "new_cust_rat")   
  vars3 <- c("resid_cust_rat", "office_cust_rat", "move_cust_rat")
  
  if (mh) {
    df <- fill_missing_proportion(df, vars1)
    df <- fill_missing_proportion(df, vars2, norm = FALSE)
    df <- fill_missing_proportion(df, vars3)
  }
  
  df <- commom_process(df, median = median)
  
  if (create_target) {
    df_final <- assign_cluster_area(df)
    df_final$mkt_status <- as.factor(df_final$mkt_status)
    #write.csv(df_final, paste0(prefix, "_for_mkt.csv"), row.names = FALSE)
  } else {
    df_final <- df
  }
  
  df_final  <- df_final  %>% 
    select(-c(close_rat, open_cnt, close_cnt, open_rat))  
  
  # Define top variables for lag, rolling stats, and difference features
  top_variables <- c(
    "sim_store_cnt", "store_cnt", "franchise_cnt", "pop_all_std",
    "move_num", "age_30_move_num", "rent", "age_20_under_move_num",
    "f_move_num", "m_move_num"
  )
  
  df_final <- df_final %>%
    group_by(dong, big_ind) %>%
    arrange(ym_quarter) %>%
    mutate(
      
      # Lag features: 1 to 4 quarters
      across(all_of(top_variables), .fns = ~lag(.x, n = 1), .names = "{.col}_lag1"),
      across(all_of(top_variables), .fns = ~lag(.x, n = 2), .names = "{.col}_lag2"),
      across(all_of(top_variables), .fns = ~lag(.x, n = 3), .names = "{.col}_lag3"),
      across(all_of(top_variables), .fns = ~lag(.x, n = 4), .names = "{.col}_lag4"),
      
      # Rolling means: 2,3,4 quarters
      across(all_of(top_variables), 
             .fns = ~zoo::rollapply(.x, width = 2, FUN = mean, align = "right", fill = NA, na.rm = TRUE), 
             .names = "{.col}_ma2"),
      across(all_of(top_variables), 
             .fns = ~zoo::rollapply(.x, width = 3, FUN = mean, align = "right", fill = NA, na.rm = TRUE), 
             .names = "{.col}_ma3"),
      across(all_of(top_variables), 
             .fns = ~rollapply(.x, width = 4, FUN = mean, align = "right", fill = NA, na.rm = TRUE), 
             .names = "{.col}_ma4"),
      
      # Rolling standard deviations: 2,3,4 quarters
      across(all_of(top_variables), 
             .fns = ~rollapply(.x, width = 2, FUN = sd, align = "right", fill = NA, na.rm = TRUE), 
             .names = "{.col}_ma2_std"),
      across(all_of(top_variables), 
             .fns = ~rollapply(.x, width = 3, FUN = sd, align = "right", fill = NA, na.rm = TRUE), 
             .names = "{.col}_ma3_std"),
      across(all_of(top_variables), 
             .fns = ~rollapply(.x, width = 4, FUN = sd, align = "right", fill = NA, na.rm = TRUE), 
             .names = "{.col}_ma4_std"),
      
      
      # Differences: 1 to 4 quarters
      across(all_of(top_variables), .fns = ~(.x - lag(.x, n = 1)), .names = "{.col}_diff1"),
      across(all_of(top_variables), .fns = ~(.x - lag(.x, n = 2)), .names = "{.col}_diff2"),
      across(all_of(top_variables), .fns = ~(.x - lag(.x, n = 3)), .names = "{.col}_diff3"),
      across(all_of(top_variables), .fns = ~(.x - lag(.x, n = 4)), .names = "{.col}_diff4")
      
    ) %>%
    ungroup()
  
  df_final <- df_final %>%
    mutate(
      year = as.numeric(substr(as.character(ym_quarter), 1, 4)),
      quarter = as.numeric(substr(as.character(ym_quarter), 5, 5))
    ) %>%
    select(-ym_quarter)
  
  return(df_final)
}

