#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)

# Data Import
gss_tbl<-read_sav("../data/GSS2016.sav")
gss_tbl<-gss_tbl %>% 
  filter(!(HRS1 %in% c(98, 99)))%>%
  select_if(function(x) mean(x %in% c(98, 99)) < 0.75)

