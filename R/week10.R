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
# Evaluate models on test set
test_results <- lapply(models, function(model) {
  pred <- predict(model, gss_test_tbl)
  rmse <- RMSE(pred, gss_test_tbl$HRS2)
  r_squared <- R2(pred, gss_test_tbl$HRS2)
  return(data.frame(Model = model$method, RMSE = rmse, R_Squared = r_squared))
})
# Combine test results into a data frame
test_results_df <- do.call(rbind, test_results)
# Display test results
print(test_results_df)

# Publication
# Initialize vectors to store R-squared values
cv_rsq <- numeric(length(models))
ho_rsq <- numeric(length(models))
# Calculate R-squared for 10-fold CV and holdout CV using a for loop
for (i in seq_along(models)) {
  model <- models[[i]]
  if (inherits(model, "train")) {
    cv_rsq[i] <- round(model$results$Rsquared[which(row.names(model$results) == row.names(model$bestTune))], 2)
  } else {
    cv_rsq[i] <- round(summary(model)$r.squared, 2)
  }
  preds <- predict(model, newdata = gss_test_tbl)
  ho_rsq[i] <- round(1 - sum((preds - gss_test_tbl$HRS2)^2) / sum((mean(gss_test_tbl$HRS2) - gss_test_tbl$HRS2)^2), 2)
}
# Construct the table
table1_tbl <- tibble(
  algo = c("OLS", "Elastic Net", "Random Forest", "XGBoost"),
  cv_rsq = sprintf("%.2f", cv_rsq),
  ho_rsq = sprintf("%.2f", ho_rsq)
)
# Replace leading zeros with spaces
table1_tbl$cv_rsq <- sub("^0", " ", table1_tbl$cv_rsq)
table1_tbl$ho_rsq <- sub("^0", " ", table1_tbl$ho_rsq)
# Display the table
print(table1_tbl, row.names = FALSE, width = Inf)

#How did your results change between models? Why do you think this happened, specifically?
#The result changed between models due to the differences in their underlying 
#algorithms and assumptions. OLS and Elastic Net are both linear models, 
#but Elastic Net incorporates regularization, which helps to prevent overfitting 
#and may lead to better performance. Thus, it is not surprised that Elastic Net 
#has greater 10-fold CV R square than OLS. Random Forest and XGBoost are both 
#tree-based ensemble methods, which can capture complex non-linear relationships 
#between variables. Random Forest had the highest 10-fold CV R-squared, likely 
#due to its ability to capture non-linear relationships and reduce overfitting 
#through averaging multiple trees. However, the 10-fold CV R square of XGBoost 
#is not greater than better than OLS, possibly because it was not able to fully 
#leverage its boosting capabilities on this particular dataset.

#How did you results change between k-fold CV and holdout CV? Why do you think this happened, specifically?
#The result between 10-fold CV and holdout CV changed due to differences in the 
#validation process. K-fold CV uses multiple folds and averages the performance 
#across them, which helps to provide a more reliable estimate of the model's 
#performance. Holdout CV evaluates the model on a single, separate dataset that 
#the model has not seen before. It can sometimes give a less accurate performance 
#estimate due to the randomness of the data split. In this case, we see a substantial 
#decrease in the holdout CV R-squared values for OLS and Random Forest, suggesting 
#that these models might be overfitting the training data, and their performance 
#does not generalize well to new data.

#Among the four models, which would you choose for a real-life prediction problem, and why? Are there tradeoffs?
#Decision is really depending on the situation and specific problem. Based on 
#the results, I would choose the Elastic Net model for a real-life prediction 
#problem if my data is simple and only have linear relationship. The Elastic 
#Net model has the highest holdout CV R-squared value, which indicates good 
#generalization performance, and it also has a reasonable 10-fold CV R-squared 
#value. The Elastic Net model benefits from the regularization, which helps to 
#prevent overfitting, and it is a linear model, which is simpler and easier to 
#interpret than tree-based ensemble models like Random Forest and XGBoost. 
#However, if my data is complex and have non-linear relationships, then I may 
#consider XGBoost since it also shows a relatively good performance in both 
#10-fold CV and holdout CV. In addition, the Elastic Net model might not 
#capture complex non-linear relationships as well as tree-based models, and its 
#performance might be more sensitive to the choice of hyperparameters. 