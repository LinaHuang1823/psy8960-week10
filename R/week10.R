# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)
library(glmnet)
library(randomForest)

# Data Import and Cleaning
gss_tbl<-read_sav("../data/GSS2016.sav")%>% 
  #Mark any missing, don’t know, inapplicable, or otherwise not-clearly-answered items as missing values. 
  mutate(across(everything(), ~na_if(., -1))) %>% 
  mutate(across(everything(), ~na_if(., -2))) %>% 
  mutate(across(everything(), ~na_if(., -3)))%>% 
  mutate(across(everything(), ~na_if(., -4)))%>%
  filter(!is.na(HRS2))%>% #Remove anyone who has a missing value for HRS2
  select_if(~sum(!is.na(.))/length(.) >= 0.75) #retain only variables with less than 75% missingness

# Visualization
ggplot(gss_tbl, aes(x = as.numeric(HRS2))) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Workhours",
       x = "Workhours per Week",
       y = "Frequency")

# Analysis
#randomly order the dataset
row_tbl<-sample(nrow(gss_tbl))
gss_tbl<-gss_tbl[row_tbl,]
# Preprocess the data
gss_tbl_processed <- gss_tbl %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.),median(., na.rm = TRUE),.)))
# Create train-test split (75/25)
train_idx <- createDataPartition(gss_tbl_processed$HRS2, p = 0.75, list = FALSE)
gss_train_tbl <- gss_tbl_processed[train_idx, ] #training set
gss_test_tbl <- gss_tbl_processed[-train_idx, ] #test set
# Set up cross-validation
train_control <- trainControl(method = "cv", number = 10)
# Set hyperparameter grids for elastic net
elastic_net_grid <- expand.grid(alpha = seq(0, 1, length.out = 5),
                                lambda = seq(0.001, 0.1, length.out = 5))
# Set hyperparameter grids for random forest
random_f_grid <- expand.grid(mtry = seq(2, floor(sqrt(ncol(gss_train_tbl))),
                                             length.out = 5))
# Set hyperparameter grids for eXtreme Gradient Boosting
xgboost_grid <- expand.grid(nrounds = c(100),
                            max_depth = seq(3, 10, by = 3),
                            eta = c(0.01, 0.1, 0.3),
                            gamma = c(0, 1, 5),
                            colsample_bytree = c(0.5, 1),
                            min_child_weight = c(1, 5),
                            subsample = c(0.5, 1))
# Train OLS, elastic net, random forest, and xgboost models with grid search
formula <- HRS2 ~ .
models <- list(
  OLS = train(formula, data = gss_train_tbl, method = "lm", 
              trControl = train_control),
  ElasticNet = train(formula, data = gss_train_tbl, method = "glmnet", 
                     trControl = train_control, tuneGrid = elastic_net_grid),
  RandomForest = train(formula, data = gss_train_tbl, method = "rf", 
                       trControl = train_control, tuneGrid = random_f_grid),
  XGBoost = train(formula, data = gss_train_tbl, method = "xgbTree", 
                  trControl = train_control, tuneGrid = xgboost_grid)
)
# Print summaries of the models
lapply(models, summary)

# Publication

