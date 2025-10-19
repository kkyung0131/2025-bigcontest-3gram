# ============================================================
# 01_load_packages.R
# ------------------------------------------------------------
# Description: Load required packages and define basic utility 
#              functions for model saving/loading.
# ============================================================

# ---- Load Required Packages ----
library(tidymodels)
library(tidyverse)
library(bonsai)   
library(dplyr)
library(zoo)
library(VIM)
library(lubridate)
library(lightgbm)


# ---- Utility Functions ----
#' Save a list of models as RDS
#'
#' @param models A list of trained models.
#' @param path File path to save the RDS file (default: "./models_list.rds").
#' @return Invisibly returns the saved file path.
save_models <- function(models, path = "./models_list.rds") {
  saveRDS(models, path)
  invisible(path)
}

#' Load a list of saved models
#'
#' @param path File path to load the RDS file (default: "./models_list.rds").
#' @return A list of models read from the specified RDS file.
load_models <- function(path = "./models_list.rds") {
  readRDS(path)
}