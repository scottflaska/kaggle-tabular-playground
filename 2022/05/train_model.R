library(plyr)

library(dplyr)

library(caret)

library(recipes)

library(xgboost)

library(tidyverse)

library(doParallel)

train_fold = readRDS("~/project/2022/05/outputs/train_fold.rds")

validation_fold = readRDS("~/project/2022/05/outputs/validation_fold.rds")

model_recipe = recipe(target ~ ., data = train_fold) %>% 
  update_role(id, new_role = "info")
  # step_dummy(contains("f_27_"))

set.seed(1)

folds = createFolds(y = train_fold$target,
                    k = 5,
                    returnTrain = T)

ctrl = trainControl(method = "cv",
                    number = 5,
                    index = folds,
                    savePredictions = "final",
                    allowParallel = T,
                    classProbs = T,
                    summaryFunction = twoClassSummary)

# workers = round(detectCores()*0.5)
# 
# cl = makePSOCKcluster(workers)
# 
# registerDoParallel(cl)

start_time = Sys.time()

machine_state_model = caret::train(model_recipe,
                                   data = train_fold,
                                   method = "xgbTree",
                                   trControl = ctrl, 
                                   metric = "ROC",
                                   tuneLength = 1,
                                   nthread = 1)

end_time = Sys.time()

print(end_time - start_time)

# stopCluster(cl)
# 
# foreach::registerDoSEQ()

saveRDS(machine_state_model, file = "~/project/2022/05/outputs/machine_state_model.rds")

roc = max(machine_state_model$results$ROC)

cat(roc, file = "~/project/2022/05/metrics/roc.txt")

# rpart_model = rpart(formula = target ~ . - id,
#                     data = train_fold,
#                     method = "class",
#                     cp = 0.001)
# 
# saveRDS(rpart_model,file = "~/project/2022/05/outputs/model.rds")
# 
# validation_preds = validation_fold
# 
# validation_preds$pred = predict(object = rpart_model,
#                                 newdata = validation_fold,
#                                 type = "class")
# 
# validation_performance = validation_preds %>% 
#   mutate(correct = ifelse(pred == target,1,0))
# 
# accuracy = validation_performance %>% 
#   summarize(accuracy = mean(correct)) %>% 
#   pull(accuracy) %>% 
#   round(digits = 4)
# 
# cat(accuracy,file = "~/project/2022/05/accuracy.txt")

