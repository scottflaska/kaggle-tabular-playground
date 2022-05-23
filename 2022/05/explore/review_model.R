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
