library(tidyverse)

library(rpart)

library(rpart.plot)

train_fold = readRDS("~/project/2022/05/outputs/train_fold.rds")

train_sample = train_fold %>% 
  slice_sample(n = 10000)

rpart_model = rpart(formula = target ~ . - id,
                    data = train_sample,
                    method = "class")

rpart.plot(rpart_model)

