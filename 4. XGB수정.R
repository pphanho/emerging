#####################################################
#                                                   #
#                                                   #
#     Author : 최영재                               #
#     Date : 2019 - 09 - 30                         #
#     Title : XGBOOST                               #
#                                                   #
#                                                   #
#####################################################

## 0. 사전작업 -----------------------------------------------------------------

# 패키지 불러오기
library(caret)
library(e1071)
library(corrplot)
library(tidyverse)
library(readxl)
library(writexl)
library(progress)
library(naniar)
library(MLmetrics)
library(spacyr)
library(tm)
library(ROSE) # ROSE 샘플링
library(pROC) # AUC값
library(ranger) # RandomForest
library(doParallel) # 병렬처리
library(xgboost) # XGBOOST
library(kernlab) # SVM

# 변수제거 및 작업공간 설정
rm(list = ls(all = TRUE))
setwd("C:/Users/Choi Young Jae/Desktop/2019 Semi-Supervised Learning")


## 3. 모형 적합 ----------------------------------------------------------------
load(file = './temp_data(기본 전처리 완료).Rdata')
str(temp_data)
colnames(temp_data)

# factor형 변수 numeric type으로 변환
temp_data[, 10] <- as.numeric(as.character(temp_data[, 10]))
temp_data[, 18] <- as.numeric(as.character(temp_data[, 18]))
temp_data[, 19] <- as.numeric(as.character(temp_data[, 19]))
temp_data[, 20] <- as.numeric(as.character(temp_data[, 20]))
temp_data[, 21] <- as.numeric(as.character(temp_data[, 21]))
temp_data[, 22] <- as.numeric(as.character(temp_data[, 22]))
temp_data[, 23] <- as.numeric(as.character(temp_data[, 23]))
temp_data[, 24] <- as.numeric(as.character(temp_data[, 24]))
temp_data[, 25] <- as.numeric(as.character(temp_data[, 25]))
temp_data[, 26] <- as.numeric(as.character(temp_data[, 26]))
temp_data[, 27] <- as.numeric(as.character(temp_data[, 27]))

# 시드 설정
set.seed(20191002)

# 기본 값 설정
trainsplitting_prop <- 0.90
labelling_prop <- 0.30
rpt_var <- 10
cv_var <- 4
unit <- 200
count <- 0
results <- c()
semi_table <- c()

# 새로 추가한 변수 제거
temp_data <- temp_data[,-c(34, 35, 36, 37, 38)]

## 0. Change independent variable's level
levels(temp_data$핵심특허) <- c("NET", "ET")

## 1. Train vs Test splitting
tr.index <- createDataPartition(temp_data$핵심특허, p = trainsplitting_prop, list = FALSE)
TrainDataSet <- temp_data[tr.index, ] # 2429 Records
TestDataSet <- temp_data[-tr.index, ] # 269 Records

x_test <- as.matrix(TestDataSet %>% select(-핵심특허)) # 최종 test에 사용할 독립변수
y_test <- TestDataSet$핵심특허 # 최종 test에 사용할 종속변수

## 2. Labelled vs Unlabelled splitting
lb.index <- createDataPartition(TrainDataSet$핵심특허, p = labelling_prop, list = FALSE)
labeledSet <- TrainDataSet[lb.index, ] # 486 Records
unlabeledSet <- TrainDataSet[-lb.index, ] # 1943 Records

## 3. Model Fitting
set.seed(20191002)

# 불균형 데이터셋 처리
trainSet_up <- labeledSet
# trainSet_up <- ROSE(핵심특허 ~ ., data = labeledSet)$data # ROSE 샘플링
# trainSet_up <- SMOTE(핵심특허 ~ ., data = labeledSet) # SMOTE 샘플링


# XGBOOOST를 위한 데이터 형태 변환(Matrix form)
x_labeled <- as.matrix(trainSet_up %>% select(-핵심특허))
y_labeled <- trainSet_up$핵심특허
x_unlabeled <- as.matrix(unlabeledSet %>% select(-핵심특허))
y_unlabeled <- unlabeledSet$핵심특허
# unlabeledSet[, length(unlabeledSet)] <- NA

# Make parallel cluster and register
cl = makeCluster(detectCores()-1) # n-1 core cluster 병렬처리를 위해 코어들을 할당하는 클러스터
registerDoParallel(cl) # 해당 클러스터를 작업하기 위해 할당해주는 것

# 교차검증 수행
fitControl = trainControl(method = "repeatedcv", number = cv_var, repeats = rpt_var,
                          classProbs = TRUE, summaryFunction = twoClassSummary, search = "grid")

# 튜닝을 위한 그리드 범위 설정
fitGrid = expand.grid(nrounds = c(100, 200),
                      eta = 0.3,
                      gamma = 0,
                      max_depth = c(1, 2),
                      colsample_bytree = 0.6,
                      min_child_weight = 1,
                      subsample = c(0.833, 1))


# names(getModelInfo()) # 사용할 수 있는 method 확인
set.seed(20191002)
# 모델 적합
xgb_fit <- train(x_labeled, y_labeled, method = "xgbTree",
                 metric = "ROC",
                 trControl = fitControl,
                 # tuneGrid = fitGrid,
                 tuneLength = 7,
                 verbose = FALSE)
xgb_fit
# plot(xgb_fit)

# The final values used for the model were nrounds = 150, max_depth = 2, eta = 0.3, gamma = 0, colsample_bytree = 0.6, min_child_weight = 1 and subsample = 1.

stopCluster(cl) # End parallel cluster

## 4. Predict and save results(지도학습만을 이용해 TestSet을 예측한 결과)
pred.class <- predict(xgb_fit, x_test, type = 'raw')
pred.prob <- predict(xgb_fit, x_test, type = 'prob')

temp_result <- confusionMatrix(pred.class, y_test, positive = "ET")
results <- rbind(results, c(temp_result$overall[1] %>% round(digits = 3),
                            temp_result$byClass[5] %>% round(digits = 3),
                            temp_result$byClass[6] %>% round(digits = 3),
                            temp_result$byClass[7] %>% round(digits = 3),
                            roc(y_test, pred.prob[, "ET"], levels = rev(levels(y_test)))[[9]][1] %>% round(digits = 3)))

semi_table <- rbind(semi_table, c(temp_result$table[1],
                                  temp_result$table[2],
                                  temp_result$table[3],
                                  temp_result$table[4]))

## 5. Semi-supervised learning (SSL)

while(TRUE){
  
  count <- count +1
  
  # UnlabeledSet 예측
  temp_for_insert <- predict(xgb_fit, x_unlabeled[1:unit,], type = 'prob') %>% as.data.frame()
  
  lb_insert_size <- nrow(temp_for_insert) * 0.3 # 숫자 변경 가능
  
  lb_insert_vector <- (lb_insert_size * prop.table(labeledSet$핵심특허 %>% table())) %>% round(digits = 0) %>% as.vector()
  
  lb_cutoff_0 <- temp_for_insert %>% arrange(desc(`NET`)) %>% head(lb_insert_vector[1])[1, length(lb_insert_vector)]
  lb_cutoff_1 <- temp_for_insert %>% arrange(desc(`ET`)) %>% head(lb_insert_vector[2])[2, length(lb_insert_vector)]
  
  temp_for_insert[which(temp_for_insert[, 1] >= lb_cutoff_0), 3] <- "NET" 
  temp_for_insert[which(temp_for_insert[, 2] >= lb_cutoff_1), 3] <- "ET" 
  names(temp_for_insert)[3] <- "pred"
  temp_for_insert[ ,3] <- factor(temp_for_insert[ ,3], levels = c("NET", "ET"))
  
  # temp_for_insert에 실제값 추가
  temp_for_insert <- temp_for_insert %>% mutate(actual = y_unlabeled[1:unit])
  
  ## 6. Active Learning (AL)
  
  # 분류확률이 0.40~0.60로 애매하여 ET 혹은 NET로 분류하지 못한 애들에게 실제 값을 부여함
  # 0.40 ~ 0.60사이의 확률로 ET라고 예측했던 데이터들에게 실제값을 부여함
  
  temp_for_insert[which(0.40 <= temp_for_insert[, 2] & temp_for_insert[, 2] <= 0.60), 3] <-
    temp_for_insert[which(0.40 <= temp_for_insert[, 2] & temp_for_insert[, 2] <= 0.60), 4]
  
  
  unlabeledSet[1:unit, length(unlabeledSet)] <- temp_for_insert[, 3] %>% as.character() # UnlabeledSet에 정답을 부여
  
  labeledSet <- rbind(labeledSet, unlabeledSet[1:unit, ]) %>% na.omit() # LabeledSet에 UnlabeledSet을 예측한 결과 추가
  
  unlabeledSet <- unlabeledSet[-c(1:unit), ] # UnlabeledSet에서 예측한 결과값들은 지우기
  
  # 새로운 LabeledSet으로 다시 모형 적합
  set.seed(20191002)
  
  trainSet_up <- labeledSet
  # trainSet_up <- ROSE(핵심특허 ~ ., data = labeledSet)$data # ROSE 샘플링
  # trainSet_up <- SMOTE(핵심특허 ~ ., data = labeledSet) # SMOTE 샘플링
  
  # XGBOOOST를 위한 데이터 형태 변환(Matrix form)
  x_labeled <- as.matrix(trainSet_up %>% select(-핵심특허))
  y_labeled <- trainSet_up$핵심특허
  x_unlabeled <- as.matrix(unlabeledSet %>% select(-핵심특허))
  y_unlabeled <- unlabeledSet$핵심특허
  
  # Make parallel cluster and register
  cl = makeCluster(detectCores()-1) # n-1 core cluster 병렬처리를 위해 코어들을 할당하는 클러스터
  registerDoParallel(cl) # 해당 클러스터를 작업하기 위해 할당해주는 것
  
  # 교차검증 수행
  fitControl = trainControl(method = "repeatedcv", number = cv_var, repeats = rpt_var,
                            classProbs = TRUE, summaryFunction = twoClassSummary, search = "grid")
  
  # 튜닝을 위한 그리드 범위 설정
  fitGrid = expand.grid(nrounds = c(100, 150, 200),
                        eta = 0.3,
                        gamma = 0,
                        max_depth = c(1, 2),
                        colsample_bytree = 0.6,
                        min_child_weight = 1,
                        subsample = c(0.833, 1))
  
  # names(getModelInfo()) # 사용할 수 있는 method 확인
  set.seed(20191002)
  # 모델 적합
  xgb_fit <- train(x_labeled, y_labeled, method = "xgbTree",
                   metric = "ROC",
                   trControl = fitControl,
                   tuneGrid = fitGrid,
                   tuneLength = 5,
                   verbose = FALSE)
  # xgb_fit
  # plot(xgb_fit)
  
  stopCluster(cl) # End parallel cluster
  
  pred.class <- predict(xgb_fit, x_test, type = 'raw')
  pred.prob <- predict(xgb_fit, x_test, type = 'prob')
  
  temp_result <- confusionMatrix(pred.class, y_test, positive = "NET")
  results <- rbind(results, c(temp_result$overall[1] %>% round(digits = 3),
                              temp_result$byClass[5] %>% round(digits = 3),
                              temp_result$byClass[6] %>% round(digits = 3),
                              temp_result$byClass[7] %>% round(digits = 3),
                              roc(y_test, pred.prob[, "ET"], levels = rev(levels(y_test)))[[9]][1] %>% round(digits = 3)))
  
  
  # SSL과 AL을 진행함에 따른 혼동행렬 결과
  semi_table <- rbind(semi_table, c(temp_result$table[1],
                                    temp_result$table[2],
                                    temp_result$table[3],
                                    temp_result$table[4]))
  
  if (unit > nrow(unlabeledSet)) {
    break
  }
  
}
semi_table <- semi_table %>% as.data.frame()
colnames(semi_table) <- c("True_Negative", "False_Positive", "False_Negative", "True_Positive")
results <- results %>% as.data.frame()
colnames(results) <- c("Accuracy", "Precision", "Recall", "F1-Score", "AUROC")

print(semi_table)
print(results)

write_xlsx(semi_table,"./XGB_혼동행렬최종1.xlsx")
write_xlsx(results,"./XGB_결과최종1.xlsx")

# rm(temp_data)
# save.image(file = "./예측을위한XGB모형.RData")






## 추가로 변수중요도 계산
imp = varImp(object = xgb_fit)
imp <- xgb.importance(feature_names = xgb_fit$finalModel$feature_names,
                          model = xgb_fit$finalModel)
imp2 <- head(imp, 6)

data.frame(variable = rep(imp2$Feature,1),
           value = c(imp2$Gain),
           Type = c(rep('Gain',nrow(imp2)))
) %>% ggplot(aes(variable,value,fill = variable))+
  geom_bar(stat = 'identity')+
  facet_grid(~Type)+
  theme_bw()+
  ggtitle('XGB : Variable Importance Plot')


# 변수 중요도
# xgb.importance(feature_names = colnames(temp_data[,-length(temp_data)]), model = xgb.fit)
# xgb.plot.importance(xgb.importance(feature_names = colnames(temp_data[,-length(temp_data)]), model = xgb.fit))


