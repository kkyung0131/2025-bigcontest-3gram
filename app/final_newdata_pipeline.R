# final_model_pipeline_for_newdata

library(tidymodels)
library(tidyverse)
library(bonsai)   
library(dplyr)
library(zoo)
library(VIM)
library(lubridate)


### train_ref

train_ref <- read.csv("./train_for_cust_pattern_4class.csv")

### === common process pipeline === 

commom_process <- function(df) {
  
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
  
  # 외부 파일 로드 (사용환경에 맞춰 경로/파일 존재 확인)
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
  
  df_merged <- na.omit(df_merged)
  return(df_merged)
}



### === process pipeline for new data - sales model ===

process_pipeline_sales_newdata <- function(df, prefix = "sales_newdata") {
  df_final <- commom_process(df)
  #write.csv(df_final, paste0(prefix, "_processed.csv"), row.names = FALSE)
  return(df_final)
}

### === predict and scoring - sales model ===

predict_score_sales <- function(models, new_data) {
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



### === process pipeline for new data - cust model ===

process_pipeline_cust_newdata <- function(df, prefix = "cust_newdata") {
  
  #df_final <- commom_process(df)
  
  df_final <- df %>%
    na.omit() %>%
    mutate(big_ind = factor(big_ind, levels = unique(train_ref$big_ind)),
           has_dlv_cat = as.factor(has_dlv_cat), 
           is_brand = as.factor(is_brand))
  
  #write.csv(df_final, paste0(prefix, "_processed.csv"), row.names = FALSE)
  return(df_final)
}

### === predict and scoring - cust model ===

predict_score_cust <- function(models, new_data) {
  # 1. class 예측 (for ensemble majority vote)
  rowMode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  preds_class <- map(models, ~ predict(.x$model, new_data, type = "class")) %>%
    reduce(bind_cols)
  
  # 2. 확률 예측 (for risk score) -> 클래스별 확률 평균
  preds_prob <- map(models, ~ predict(.x$model, new_data, type = "prob")) %>%
    reduce(`+`) / length(models)
  
  # 3. 앙상블 클래스 (최빈값)
  ensemble_class <- apply(preds_class, 1, rowMode)
  
  # 4. risk score 계산: sum(k * P_k)
  prob_cols <- preds_prob %>% select(starts_with(".pred_"))
  
  class_levels <- gsub("^\\.pred_", "", names(prob_cols))  # "1_Growth_Extreme", ...
  class_index <- seq_along(class_levels)                   # 1:4
  
  risk_score <- as.numeric(as.matrix(prob_cols) %*% class_index)
  
  # 5. min-max scaling
  risk_score_scaled <- (risk_score - 1) / (4 - 1)
  
  # 결과
  results <- bind_cols(
    new_data %>% select(id, ym_quarter),
    prob_cols,
    tibble(
      ensemble_class = ensemble_class,
      risk_score = risk_score,
      risk_score_scaled = risk_score_scaled
    )
  )
  
  # score만 
  #cust_score = risk_score_scaled
  
  return(results)
}



### === process pipeline for new data - mkt model ===

process_pipeline_mkt_newdata <- function(df, prefix = "mkt_newdata") {
  
  #df_final <- commom_process(df)
  
  ### 시차 변수 추가 
  df_final  <- df  %>% 
    select(-c(close_rat, open_cnt, close_cnt, open_rat, id))  
  
  top_variables <- c(
    "sim_store_cnt", "store_cnt", "franchise_cnt", "pop_all_std",
    "move_num", "age_30_move_num", "rent", "age_20_under_move_num",
    "f_move_num", "m_move_num"
  )
  
  df_final <- df_final %>%
    group_by(dong, big_ind) %>%
    arrange(ym_quarter) %>%
    mutate(
      
      # ======== 1. 다양한 시차(Lag) 추가 ========
      # 1분기 전 Lag (단기 영향력)
      across(all_of(top_variables), .fns = ~lag(.x, n = 1), .names = "{.col}_lag1"),
      # 2분기 전 Lag (1년 전 동분기 값, 계절성 파악에 매우 중요!)
      across(all_of(top_variables), .fns = ~lag(.x, n = 2), .names = "{.col}_lag2"),
      # 3분기 전 Lag (1년 전 동분기 값, 계절성 파악에 매우 중요!)
      across(all_of(top_variables), .fns = ~lag(.x, n = 3), .names = "{.col}_lag3"),
      # 4분기 전 Lag (1년 전 동분기 값, 계절성 파악에 매우 중요!)
      across(all_of(top_variables), .fns = ~lag(.x, n = 4), .names = "{.col}_lag4"),
      
      # ======== 2. 이동 통계량(Rolling Statistics) 확장 ========
      # zoo::rollapply 함수 사용 (width: 기간, FUN: 함수, fill: 값 채우기, align: 정렬)
      
      across(all_of(top_variables), 
             .fns = ~zoo::rollapply(.x, width = 2, FUN = mean, align = "right", fill = NA, na.rm = TRUE), 
             .names = "{.col}_ma2"),
      across(all_of(top_variables), 
             .fns = ~zoo::rollapply(.x, width = 3, FUN = mean, align = "right", fill = NA, na.rm = TRUE), 
             .names = "{.col}_ma3"),
      # 4분기(1년) 이동 평균 (장기 추세 파악)
      across(all_of(top_variables), 
             .fns = ~rollapply(.x, width = 4, FUN = mean, align = "right", fill = NA, na.rm = TRUE), 
             .names = "{.col}_ma4"),
      
      # 4분기(1년) 이동 표준편차 (변동성, 불안정성 파악)
      across(all_of(top_variables), 
             .fns = ~rollapply(.x, width = 2, FUN = sd, align = "right", fill = NA, na.rm = TRUE), 
             .names = "{.col}_ma2_std"),
      across(all_of(top_variables), 
             .fns = ~rollapply(.x, width = 3, FUN = sd, align = "right", fill = NA, na.rm = TRUE), 
             .names = "{.col}_ma3_std"),
      across(all_of(top_variables), 
             .fns = ~rollapply(.x, width = 4, FUN = sd, align = "right", fill = NA, na.rm = TRUE), 
             .names = "{.col}_ma4_std"),
      
      
      # ======== 3. 변화량(Difference) 피처 심화 ========
      # 직전 분기 대비 변화량 (단기 변화)
      across(all_of(top_variables), .fns = ~(.x - lag(.x, n = 1)), .names = "{.col}_diff1"),
      
      # 반분기 대비 변화량 (계절성을 제거한 순수 성장률 파악에 매우 중요!)
      across(all_of(top_variables), .fns = ~(.x - lag(.x, n = 2)), .names = "{.col}_diff2"),
      across(all_of(top_variables), .fns = ~(.x - lag(.x, n = 3)), .names = "{.col}_diff3"),
      # 1년 전 동분기 대비 변화량 (Year-over-Year, 계절성을 제거한 순수 성장률 파악에 매우 중요!)
      across(all_of(top_variables), .fns = ~(.x - lag(.x, n = 4)), .names = "{.col}_diff4")
      
    ) %>%
    ungroup()
  
  
  df_final <- df_final %>%
    mutate(
      year = as.numeric(substr(as.character(ym_quarter), 1, 4)),
      quarter = as.numeric(substr(as.character(ym_quarter), 5, 5))
    ) %>%
    select(-ym_quarter)

  #write.csv(df_final, paste0(prefix, "_processed.csv"), row.names = FALSE)
  return(df_final)
}

### === predict and scoring - mkt model ===

predict_score_mkt <- function(model, new_data){
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


### === load models ===

load_models <- function(path = "./models_list.rds") {
  readRDS(path)
}
