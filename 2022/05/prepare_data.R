library(tidyverse)

library(readr)

source("~/project/2022/05/functions/clean_data.R")

train = read_csv("~/project/2022/05/data/train.csv",show_col_types = F) 

train_cleaned = clean_data(x = train) %>% 
  relocate(target, .after = everything()) %>% 
  mutate(target = case_when(target == 1 ~ "one",
                            target == 0 ~ "zero")) %>% 
  mutate(target = factor(target))

set.seed(1)

train_fold = train_cleaned %>% 
  slice_sample(prop = 0.9)
  
validation_fold = train_cleaned %>% 
  filter(!(id %in% train_fold$id))

saveRDS(object = train_fold, file = "~/project/2022/05/outputs/train_fold.rds")

saveRDS(object = validation_fold, file = "~/project/2022/05/outputs/validation_fold.rds")
