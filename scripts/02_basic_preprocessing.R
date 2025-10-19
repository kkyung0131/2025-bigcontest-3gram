# ============================================================
# 02_basic_preprocessing.R
# ------------------------------------------------------------
# Description: Apply basic preprocessing to the competition
#              datasets 1, 2, and 3.
# ============================================================


#' Preprocess dataset1
#'
#' @param data1 Raw dataset1
#' @return Preprocessed dataset1
processed_data1 <- function(data1) {
  
  new_names <- c(
    "id", "address", "name", "is_brand", "sigungu", "ind", "mkt", "open_date", "close_date"
    )
  
  # Load coordinates
  coords_df <- read.csv("./data/coords_df.csv")
  
  data1 <- 
    data1 %>% 
    rename_with(~ new_names) %>%
    # Remove specific problematic IDs
    filter(!(id %in% c("43C7FF8A46", "15E95AF364"))) %>%
    # Encode brand flag
    mutate(is_brand = ifelse(is_brand == "", 1, 0)) %>%
    # Encode closed status
    mutate(is_closed = ifelse(is.na(close_date), 0, 1)) %>%
    # Map detailed industry to broader categories
    mutate(
      big_ind = case_when(
        ind %in% c("백반/가정식", "한식-찌개/전골", "한식-냉면", "한식-해물/생선", "한식-단품요리일반", "한식-국수/만두", "한식-육류/고기", "한식-국밥/설렁탕", "한식뷔페", "한정식", "한식-감자탕", "구내식당/푸드코트", "한식-죽", "한식-두부요리", "기사식당", "반찬", "도시락") ~ "한식음식점",
        ind %in% c("중식-딤섬/중식만두", "중식당", "중식-훠궈/마라탕") ~ "중식음식점",
        ind %in% c("일식당", "이자카야", "일식-우동/소바/라면", "일식-덮밥/돈가스", "일식-샤브샤브", "일식-초밥/롤", "일식-참치회") ~ "일식음식점",
        ind %in% c("양식", "동남아/인도음식", "기타세계요리", "스테이크") ~ "양식음식점",
        ind %in% c("베이커리", "떡/한과 제조", "마카롱", "도너츠", "떡/한과", "와플/크로플") ~ "제과점",
        ind %in% c("햄버거", "샌드위치/토스트", "피자") ~ "패스트푸드점",
        ind %in% c("치킨") ~ "치킨전문점",
        ind %in% c("분식", "탕후루") ~ "분식전문점",
        ind %in% c("요리주점", "룸살롱/단란주점", "일반 유흥주점", "포장마차", "호프/맥주", "주류", "꼬치구이", "와인바", "와인샵", "민속주점") ~ "호프-간이주점",
        ind %in% c("커피전문점", "카페", "아이스크림/빙수", "차", "주스", "테마카페", "테이크아웃커피") ~ "커피-음료",
        ind %in% c("청과물") ~ "청과상",
        ind %in% c("식료품", "농산물","유제품", "담배") ~ "슈퍼마켓",
        ind %in% c("미곡상") ~ "미곡판매",
        ind %in% c("축산물") ~ "육류판매",
        ind %in% c("수산물","건어물") ~ "수산물판매",
        ind %in% c("식품 제조","건강식품", "인삼제품","건강원") ~ "반찬가게",
        TRUE ~ "담배"
      )
    ) %>%
    # Add dong coordinates
    bind_cols(dong = coords_df$dong) %>%
    # Drop unnecessary columns
    select(-name, -address, -sigungu, -mkt, -open_date, -close_date, -ind)
  
  return(data1)
} 


#' Preprocess dataset2
#'
#' @param data2 Raw dataset2
#' @return Preprocessed dataset2
processed_data2 <- function(data2) {
  
  # Common levels for categorical variables
  common_levels <- c("1_10%이하", "2_10-25%","3_25-50%", "4_50-75%", "5_75-90%", "6_90%초과(하위 10% 이하)")
  
  data2 <-
    data2 %>%
    # Rename columns
    rename(
      id = ENCODED_MCT,
      ym = TA_YM,
      duration_cat = MCT_OPE_MS_CN,
      sales_amt_cat = RC_M1_SAA,
      sales_cnt_cat = RC_M1_TO_UE_CT,
      unique_cust_cat = RC_M1_UE_CUS_CN,
      aov_cat = RC_M1_AV_NP_AT,
      cancel_cat = APV_CE_RAT,
      dlv_cat = DLV_SAA_RAT,
      ind_sales_amt_rat = M1_SME_RY_SAA_RAT,
      ind_sales_cnt_rat = M1_SME_RY_CNT_RAT,
      ind_rank_rat = M12_SME_RY_SAA_PCE_RT,
      mkt_rank_rat = M12_SME_BZN_SAA_PCE_RT,
      ind_cancel_rat = M12_SME_RY_ME_MCT_RAT,
      mkt_cancel_rat = M12_SME_BZN_ME_MCT_RAT
    ) %>%
    filter(!(id %in% c("43C7FF8A46", "15E95AF364"))) %>%
    mutate(ym = ymd(paste0(ym, "01"))) %>%
    # Encode selected columns as factors
    mutate(across(
      .cols = c(duration_cat, sales_amt_cat, sales_cnt_cat, unique_cust_cat, aov_cat),
      .fns = ~ factor(as.integer(factor(., levels = common_levels)), levels = 1:6)
    ))
  
  # Identify stores with all missing delivery info
  na_dlv_stores <- data2 %>%
    group_by(id) %>%
    summarise(all_na = all(is.na(dlv_cat))) %>%
    filter(all_na) %>%
    pull(id)
  
  data2 <-
    data2 %>%
    # Clean delivery data
    mutate(dlv_cat = ifelse(dlv_cat < 0, NA, dlv_cat)) %>%
    mutate(dlv_cat = ifelse(dlv_cat > 100, 100, dlv_cat)) %>%
    # Add flag for stores with delivery info
    mutate(has_dlv_cat = if_else(id %in% na_dlv_stores, 0, 1)) %>%
    # Clean other negative ratios
    mutate(ind_sales_amt_rat = ifelse(ind_sales_amt_rat < 0, 0, ind_sales_amt_rat)) %>%
    # Drop unnecessary columns
    select(-cancel_cat, -dlv_cat, -mkt_rank_rat, -mkt_cancel_rat)
  
  return(data2)
}


#' Preprocess dataset3
#'
#' @param data3 Raw dataset3
#' @return Preprocessed dataset3
processed_data3 <- function(data3) {
  
  data3 <-
    data3 %>%
    # Rename columns
    rename(
      id = ENCODED_MCT,
      ym = TA_YM,
      m_20_under_rat = M12_MAL_1020_RAT,
      m_30_rat = M12_MAL_30_RAT,
      m_40_rat = M12_MAL_40_RAT,
      m_50_rat = M12_MAL_50_RAT,
      m_60_over_rat = M12_MAL_60_RAT,
      f_20_under_rat = M12_FME_1020_RAT,
      f_30_rat = M12_FME_30_RAT ,
      f_40_rat = M12_FME_40_RAT,
      f_50_rat = M12_FME_50_RAT,
      f_60_over_rat = M12_FME_60_RAT,
      re_cust_rat = MCT_UE_CLN_REU_RAT,
      new_cust_rat = MCT_UE_CLN_NEW_RAT,
      resid_cust_rat = RC_M1_SHC_RSD_UE_CLN_RAT,
      office_cust_rat = RC_M1_SHC_WP_UE_CLN_RAT,
      move_cust_rat = RC_M1_SHC_FLP_UE_CLN_RAT
    ) %>%
    filter(!(id %in% c("43C7FF8A46", "15E95AF364"))) %>%
    mutate(ym = ymd(paste0(ym, "01"))) %>%
    # Aggregate male/female age groups
    mutate(
      age_20_under_rat = m_20_under_rat + f_20_under_rat,
      age_30_rat = m_30_rat + f_30_rat,
      age_40_rat = m_40_rat + f_40_rat,
      age_50_rat = m_50_rat + f_50_rat,
      age_60_over_rat = m_60_over_rat + f_60_over_rat,
      m_rat = m_20_under_rat + m_30_rat + m_40_rat + m_50_rat + m_60_over_rat,
      f_rat = f_20_rat + f_30_rat + f_40_rat + f_50_rat + f_60_over_rat
    )
  
  return(data3)
}


#' Merge preprocessed datasets
#'
#' @param data1 Preprocessed dataset1
#' @param data2 Preprocessed dataset2
#' @param data3 Preprocessed dataset3
#' @return Merged dataset
merged_data <- function(data1, data2, data3) {
  
  merged_1 <- left_join(data2, data3, by = c("id", "ym"))
  merged_2 <- left_join(merged_1, data1, by = "id")
  
  return(merged_2)
}


#' Split merged dataset into closed and open subsets
#'
#' @description
#'   - Closed data (`is_closed == 1`): used for ensemble model weight adjustment
#'   - Open data (`is_closed == 0`): used for training and evaluation
#'     Open data is further split into train and test sets by `id`.
#'
#' @param data Merged and preprocessed dataset (datasets 1, 2, 3)
#' @param train_prop Proportion of open data to use as train set (default: 0.8)
#' @param seed Random seed for reproducible split (default: 2025)
#' @return A list containing:
#'   - `is_closed_df`: closed data (is_closed == 1)
#'   - `open_train`: training set of open data (is_closed == 0)
#'   - `open_test`: test set of open data (is_closed == 0)
split_data <- function(data, train_prop = 0.8, seed = 2025) {
  
  # Closed data
  closed_df <- data %>% filter(is_closed == 1) %>% select(-is_closed)
  
  # Open data
  open_df <- data %>% filter(is_closed == 0) %>% select(-is_closed)
  
  # Split open data into train and test sets by id
  set.seed(seed)
  data_split <- group_initial_split(open_df, group = id, prop = train_prop)
  train_data <- training(data_split)
  test_data  <- testing(data_split)
  
  # return list
  list(
    closed_df = closed_df,
    open_train = train_data,
    open_test = test_data
  )
}
