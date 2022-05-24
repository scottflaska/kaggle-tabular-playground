library(tidyverse)

x = read_csv("~/project/2022/05/data/train.csv")

x %>% 
  slice_sample(n = 50000) %>% 
  mutate(n_a = str_count(string = f_27,pattern = "A")) %>% 
  select(f_27,
         target,
         n_a) %>% 
  group_by(n_a) %>% 
  summarize(target_pct = mean(target),
            rows = n())

train_fold = readRDS("~/project/2022/05/outputs/train_fold.rds")