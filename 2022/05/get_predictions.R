library(tidyverse)

library(readr)

source("~/project/2022/05/functions/clean_data.R")

test = read_csv("~/project/2022/05/data/test.csv",
                show_col_types = F)

test_cleaned = clean_data(test)

machine_state_model = readRDS("~/project/2022/05/outputs/machine_state_model.rds")

cal_model = glm(obs ~ one, 
                family = binomial,
                data = machine_state_model$pred)

machine_state_model_preds = predict(machine_state_model, 
                                    newdata = test_cleaned, 
                                    type = "prob")

machine_state_model_preds$cal_pred = (1 - predict(cal_model, 
                                             newdata = machine_state_model_preds,
                                             type = "response"))

# machine_state_model_preds %>% 
#   slice_sample(n = 10000) %>% 
#   ggplot(mapping = aes(x = one,
#                        y = cal_pred)) + 
#   geom_point()

submission_data = test %>% 
  select(id)

submission_data$target = machine_state_model_preds$cal_pred

write_csv(x = submission_data,
          file = "~/project/2022/05/outputs/submission_data.csv",
          append = F)
