library(tidyverse)

library(readr)

source("~/project/2022/05/functions/clean_data.R")

test = read_csv("~/project/2022/05/data/test.csv",
                show_col_types = F)

test_cleaned = clean_data(test)

machine_state_model = readRDS("~/project/2022/05/outputs/machine_state_model.rds")

machine_state_rf_model = readRDS("~/project/2022/05/outputs/machine_state_rf_model.rds")

test_cleaned$xgb = predict(machine_state_model, 
                              newdata = test_cleaned, 
                              type = "prob") %>% 
  pull(one)

test_cleaned$rf = predict(machine_state_rf_model, 
                          newdata = test_cleaned, 
                          type = "prob") %>% 
  pull(one)

# test_cleaned %>% 
#   slice_sample(n = 10000) %>% 
#   ggplot(mapping = aes(x = rf,
#                        y = xgb)) +
#   geom_point()

compare_data = test_cleaned %>% 
  mutate(target = (xgb + rf)/2) %>% 
  select(id, xgb, rf, target)

# compare_data %>% 
#   slice_sample(n= 10000) %>% 
#   ggplot(mapping = aes(x = xgb,
#                        y = target)) + 
#   geom_point()

submission_data = compare_data %>% 
  select(id, target)

write_csv(x = submission_data,
          file = "~/project/2022/05/outputs/submission_data.csv",
          append = F)