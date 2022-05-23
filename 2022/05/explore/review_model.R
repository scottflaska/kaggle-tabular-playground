library(tidyverse)

library(caret)

# install.packages("ggbeeswarm")

library(ggbeeswarm)

machine_state_model = readRDS("~/project/2022/05/outputs/machine_state_model.rds")

machine_state_model %>% 
  varImp()

train = machine_state_model$trainingData

train %>% 
  slice_sample(n = 50000) %>% 
  ggplot(mapping = aes(x = 1,
                       y = f_26,
                       color = target)) +
  geom_quasirandom()
