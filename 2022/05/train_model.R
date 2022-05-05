library(tidyverse)

library(rpart)

train_fold = readRDS("~/project/2022/05/train_fold.rds")

validation_fold = readRDS("~/project/2022/05/validation_fold.rds")

rpart_model = rpart(formula = target ~ . - id,
                    data = train_fold,
                    method = "class",
                    cp = 0.001)

saveRDS(rpart_model,file = "~/project/2022/05/rpart_model.rds")

validation_preds = validation_fold

validation_preds$pred = predict(object = rpart_model,
                                newdata = validation_fold,
                                type = "class")

validation_performance = validation_preds %>% 
  mutate(correct = ifelse(pred == target,1,0))

accuracy = validation_performance %>% 
  summarize(accuracy = mean(correct)) %>% 
  pull(accuracy) %>% 
  round(digits = 4)

cat(accuracy,file = "~/project/2022/05/accuracy.txt")

