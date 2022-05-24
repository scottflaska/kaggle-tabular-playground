library(tidyverse)

library(caret)

# install.packages("ggbeeswarm")

library(ggbeeswarm)

machine_state_model = readRDS("~/project/2022/05/outputs/machine_state_model.rds")

machine_state_model %>% 
  varImp()

machine_state_model$bestTune

train = machine_state_model$trainingData

train %>% 
  slice_sample(n = 25000) %>% 
  ggplot(mapping = aes(x = f_25,
                       y = f_19,
                       color = target)) +
  geom_point()

pred = machine_state_model$pred

pred$one %>% hist()

pred %>% 
  select(one,
         pred,
         obs) %>% 
  mutate(one_round = round(one, digits = 2)) %>%
  mutate(one_bin = ifelse(obs == "one",1,0)) %>% 
  group_by(one_round) %>% 
  summarize(rows = n(),
            actual_pct = mean(one_bin)) %>% 
  as.data.frame() %>% 
  ggplot(mapping = aes(x = one_round,
                       y = actual_pct,
                       size = rows)) +
  geom_abline(slope = 1) +
  geom_point()



cal_model = glm(obs ~ one, 
                family = binomial,
                data = pred)

pred_cal = pred

pred_cal$new_pred = predict(cal_model,
                            newdata = pred,
                            type = "response")

new_pred_final = pred_cal %>% 
  mutate(new_pred = 1 - new_pred)

new_pred_final %>% 
  mutate(obs_bin = ifelse(obs == "one",1,0)) %>% 
  select(new_pred,
         old_pred = one,
         obs_bin) %>% 
  pivot_longer(cols = c(new_pred,old_pred)) %>% 
  mutate(pred_round = round(value, digits = 2)) %>% 
  group_by(name, pred_round) %>% 
  summarize(rows = n(),
            actual_pct = mean(obs_bin)) %>% 
  ggplot(mapping = aes(x = pred_round,
                       y = actual_pct,
                       size = rows,
                       color = name)) +
  geom_abline(slope = 1) +
  geom_point()
  
  
  
  # mutate(one_round = round(one, digits = 2)) %>%
  # mutate(one_bin = ifelse(obs == "one",1,0)) %>% 
  # group_by(one_round) %>% 
  # summarize(rows = n(),
  #           actual_pct = mean(one_bin)) %>% 
  # as.data.frame() %>% 
  # ggplot(mapping = aes(x = one_round,
  #                      y = actual_pct,
  #                      size = rows)) +
  # geom_abline(slope = 1) +
  # geom_point()

