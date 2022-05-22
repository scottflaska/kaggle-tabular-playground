library(tidyverse)

library(readr)

test = read_csv("~/project/2022/05/data/test.csv",
                show_col_types = F)

machine_state_model = readRDS("~/project/2022/05/outputs/machine_state_model.rds")

test$target = predict(machine_state_model, 
                      newdata = test, 
                      type = "prob") %>% 
  pull(One)

submission_data = test %>% 
  select(id, target)

write_csv(x = submission_data,
          file = "~/project/2022/05/outputs/submission_data.csv",
          append = F)
