#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)

# Data Import
gss_tbl<-read_sav("../data/GSS2016.sav")
gss_tbl<-gss_tbl %>% 
  filter(!(HRS1 %in% c(98, 99)))%>%
  select_if(function(x) mean(x %in% c(98, 99)) < 0.75)

#Visualization
ggplot(gss_tbl, aes(x = HRS1)) +
  geom_histogram(binwidth = 1, color = "black", fill = "dodgerblue", alpha = 0.8) +
  labs(title = "Distribution of Workhours",
       x = "Workhours per Week",
       y = "Frequency")+
  theme_minimal() 
