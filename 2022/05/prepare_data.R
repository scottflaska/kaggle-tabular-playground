library(tidyverse)

library(readr)

train = read_csv("~/project/2022/05/data/train.csv",show_col_types = F)

model_data = train %>% 
  select(id, 
         f_00,
         f_01,
         f_02,
         f_03,
         f_04,
         f_05,
         f_06,
         target)

model_data %>% 
  head()

train_fold = model_data %>% 
  filter(id < 100000)

validation_fold = model_data %>% 
  filter(!(id %in% train_fold$id))

saveRDS(object = train_fold, file = "~/project/2022/05/outputs/train_fold.rds")

saveRDS(object = validation_fold, file = "~/project/2022/05/outputs/validation_fold.rds")