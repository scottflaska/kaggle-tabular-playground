suppressPackageStartupMessages(library(tidyverse))

library(readr)

train = read_csv("~/project/2022/05/data/train.csv",show_col_types = F) %>% 
  mutate(target = case_when(target == 1 ~ "One",
                                   target == 0 ~ "Zero")) %>% 
  mutate(target = factor(target))

set.seed(1)

train_fold = train %>% 
  slice_sample(prop = 0.75)
  
validation_fold = train %>% 
  filter(!(id %in% train_fold$id))

saveRDS(object = train_fold, file = "~/project/2022/05/outputs/train_fold.rds")

saveRDS(object = validation_fold, file = "~/project/2022/05/outputs/validation_fold.rds")