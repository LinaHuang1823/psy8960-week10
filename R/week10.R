#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)
library(glmnet)
library(randomForest)

# Data Import and Cleaning
gss_tbl<-read_sav("../data/GSS2016.sav")

gss_tbl <- gss_tbl%>% #Mark any missing, don’t know, inapplicable, or otherwise not-clearly-answered items as missing values. 
  mutate(across(everything(), ~na_if(., -1))) %>% 
  mutate(across(everything(), ~na_if(., -2))) %>% 
  mutate(across(everything(), ~na_if(., -3)))%>% 
  mutate(across(everything(), ~na_if(., -4)))%>%
  filter(!is.na(HRS2))%>% #Remove anyone who has a missing value for HRS2
  select_if(~sum(!is.na(.))/length(.) >= 0.75) #retain only variables with less than 75% missingness

#Visualization
ggplot(gss_tbl, aes(x = as.numeric(HRS2))) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Workhours",
       x = "Workhours per Week",
       y = "Frequency")
 

