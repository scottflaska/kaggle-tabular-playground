library(tidyverse)

library(readr)

source("~/project/2022/05/functions/clean_data.R")

test = read_csv("~/project/2022/05/data/test.csv",
                show_col_types = F)

test_cleaned = clean_data(test)

machine_state_model = readRDS("~/project/2022/05/outputs/machine_state_model.rds")

test_cleaned$target = predict(machine_state_model, 
                              newdata = test_cleaned, 
                              type = "prob") %>% 
  pull(one)

submission_data = test_cleaned %>% 
  select(id, target)

write_csv(x = submission_data,
          file = "~/project/2022/05/outputs/submission_data.csv",
          append = F)
