# ============================================================
# 06_final_model.R - "CORE-AI"
# ------------------------------------------------------------
# Description:
# - This is the main execution script for CORE-AI risk modeling.
# - Options control whether to run preprocessing pipeline, train models, or just load RDS models.
# - Final risk score is calculated as an ensemble of Sales, Customer, and Market models.
# ============================================================


# -----------------------------
# Options
# -----------------------------
run_pipeline <- FALSE   # TRUE: Run full preprocessing pipeline from raw data
train_models <- FALSE   # TRUE: Train models; FALSE: Load pre-trained RDS models
verbose <- TRUE         # TRUE: Print messages for each step


# -----------------------------
# Load Packages and Scripts
# -----------------------------
source("./scripts/01_load_packages.R")        # Load required R packages
source("./scripts/02_basic_preprocessing.R")  # Basic preprocessing functions
source("./scripts/03_data_pipeline_models.R") # Data processing pipelines for sales, customer, market
source("./scripts/04_train_models.R")         # Functions for training models
source("./scripts/05_predict.R")              # Prediction functions

if(verbose) message("Packages loaded.")


# -----------------------------
# Data Loading / Preprocessing
# -----------------------------
if(run_pipeline) {
  # Load raw datasets
  data1 <- read.csv("./data/raw/big_data_set1_f.csv", fileEncoding = "cp949")
  data2 <- read.csv("./data/raw/big_data_set2_f.csv", fileEncoding = "cp949")
  data3 <- read.csv("./data/raw/big_data_set3_f.csv", fileEncoding = "cp949")
  
  # Preprocess datasets individually
  data1 <- processed_data1(data1)
  data2 <- processed_data2(data2)
  data3 <- processed_data3(data3)
  
  # Merge processed datasets
  data <- merged_data(data1, data2, data3)
  
  # Split into closed, open, train, test
  res <- split_data(data)
  closed <- res$closed_df
  open   <- res$open_df
  train  <- res$open_train
  test   <- res$open_test
  
  # Save processed data
  write.csv(closed, "./data/processed/bigcon_closed.csv", row.names = FALSE)
  write.csv(open,   "./data/processed/bigcon_open.csv", row.names = FALSE)
  write.csv(train,  "./data/processed/bigcon_open_train.csv", row.names = FALSE)
  write.csv(test,   "./data/processed/bigcon_open_test.csv", row.names = FALSE)
  
} else {
  # Load already processed data
  closed <- read.csv("./data/processed/bigcon_closed.csv")
  open   <- read.csv("./data/processed/bigcon_open.csv")
  train  <- read.csv("./data/processed/bigcon_open_train.csv")
  test   <- read.csv("./data/processed/bigcon_open_test.csv")
}

if(verbose) message("Data ready. Pipeline executed: ", run_pipeline)


# -----------------------------
# Model Training / Loading
# -----------------------------
if(train_models) {
  
  # 3-1. Run pipeline on train data
  train_sales     <- process_pipeline_sales(train, mh = TRUE, median = FALSE, create_target = TRUE)
  train_sales_cat <- process_pipeline_sales(train, mh = TRUE, median = TRUE, create_target = FALSE)
  train_cust      <- process_pipeline_cust(train, mh = TRUE, create_target = TRUE)
  train_mkt       <- process_pipeline_mkt(train, mh = TRUE, create_target = TRUE)
  
  # 3-2. Train models
  sales_models     <- train_quarterly_sales_risk_models(train_sales)
  sales_cat_models <- train_quarterly_sales_models(train_sales_cat)
  cust_models      <- train_quarterly_cust_models(train_cust)
  mkt_model        <- train_quarterly_mkt_models(train_mkt)
  
  # 3-3. Save models
  save_models(sales_models,     path = "./models/my_sales_models_2.rds")
  save_models(sales_cat_models, path = "./models/sales_model_xgb_regression.rds")
  save_models(cust_models,      path = "./models/my_cust_models_4class.rds")
  save_models(mkt_model,        path = "./models/my_mkt_models_2.rds")
  
} else {
  
  # 3-4. Load pre-trained RDS models
  sales_models     <- readRDS("./app/models/my_sales_models_2.rds")
  sales_cat_models <- readRDS("./app/models/sales_model_xgb_regression.rds")
  cust_models      <- readRDS("./app/models/my_cust_models_4class.rds")
  mkt_model        <- readRDS("./app/models/my_mkt_models_2.rds")
}

if(verbose) message("Models ready. Training executed: ", train_models)



#' Final Risk Score Model (CORE-AI)
#'
#' @description
#' Calculates the final risk score as a weighted ensemble of Sales, Customer, and Market models.
#' - Latest available data row is used for prediction.
#'
#' @param new_data Dataframe containing new data for prediction
#' @param w1 Weight for Sales score (default 0.3333)
#' @param w2 Weight for Customer score (default 0.3333)
#' @param w3 Weight for Market score (default 0.3333)
#' @return Dataframe including:
#' - final_risk_score: Weighted sum (0-100)
#' - category scores: Component scores for Sales, Customer, Market
# ============================================================
final_risk_model <- function(new_data, w1 = 0.3333, w2 = 0.3333, w3 = 0.3333) {
  
  # 4-1. Process pipeline
  new_sales <- process_pipeline_sales(new_data, mh = FALSE, median = FALSE, create_target = FALSE)
  new_cust  <- process_pipeline_cust(new_data, mh = FALSE, create_target = FALSE)
  new_mkt   <- process_pipeline_mkt(new_data, mh = FALSE, create_target = FALSE) 
  
  new_sales <- new_sales %>% group_by(id) %>% slice_tail(n = 1) %>% ungroup()
  new_cust <- new_cust %>% group_by(id) %>% slice_tail(n = 1) %>% ungroup()
  new_mkt <- new_mkt %>% group_by(id) %>% slice_tail(n = 1) %>% ungroup()
  
  # 4-2. Predict component scores
  prediction_sales <- predict_sales_risk_score(sales_models, new_sales)
  prediction_cust  <- predict_cust_score(cust_models, new_cust)
  prediction_mkt   <- predict_mkt_score(mkt_model, new_mkt)
  
  # 4-3. Extract scores
  sales_score <- prediction_sales$sales_risk_pred
  cust_score  <- prediction_cust$risk_score_scaled
  mkt_score   <- prediction_mkt$risk_score
  
  # 4-4. Calculate final weighted risk score
  final_risk_score <- (w1 * sales_score + w2 * cust_score + w3 * mkt_score) * 100
  results <- data.frame(
    id = new_sales$id,
    sales_score = sales_score * 100, 
    cust_score = cust_score * 100, 
    mkt_score = mkt_score * 100,
    final_risk_score = final_risk_score)
  
  return(results)
}

# -----------------------------
# Apply CORE-AI to Closed Data
# -----------------------------
final_score_closed <- final_risk_model(closed)
head(final_score_closed)
write.csv(final_score_closed, "final_score.csv")

if(verbose) message("Final scores calculated.")
