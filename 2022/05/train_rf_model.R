library(plyr)

library(dplyr)

library(caret)

library(recipes)

library(tidyverse)

library(doParallel)

library(randomForest)

set.seed(1)

train_fold = readRDS("~/project/2022/05/outputs/train_fold.rds") %>% 
  slice_sample(n = 50000)
  

validation_fold = readRDS("~/project/2022/05/outputs/validation_fold.rds")

model_recipe = recipe(target ~ ., data = train_fold) %>% 
  update_role(id, new_role = "info")

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

machine_state_rf_model = caret::train(model_recipe,
                                   data = train_fold,
                                   method = "rf",
                                   trControl = ctrl,
                                   tuneLength = 1,
                                   metric = "ROC")

end_time = Sys.time()

print(end_time - start_time)

# stopCluster(cl)
# 
# foreach::registerDoSEQ()

saveRDS(machine_state_rf_model, file = "~/project/2022/05/outputs/machine_state_rf_model.rds")

roc = max(machine_state_rf_model$results$ROC)

cat(roc, file = "~/project/2022/05/metrics/rf_roc.txt")

