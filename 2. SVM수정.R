#####################################################
#                                                   #
#                                                   #
#     Author : 최영재                               #
#     Date : 2019 - 09 - 30                         #
#     Title : SVM                                   #
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
library(xgboost)

# 변수제거 및 작업공간 설정
rm(list = ls(all = TRUE))
setwd("C:/Users/Choi Young Jae/Desktop/2019 Semi-Supervised Learning")


## 3. 모형 적합 ----------------------------------------------------------------
load(file = './temp_data(기본 전처리 완료).Rdata')
str(temp_data)

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

## 0. Change independent variable's level
levels(temp_data$핵심특허) <- c("NET", "ET")

## 1. Train vs Test splitting
tr.index <- createDataPartition(temp_data$핵심특허, p = trainsplitting_prop, list = FALSE)
TrainDataSet <- temp_data[tr.index, ] # 2429 Records
TestDataSet <- temp_data[-tr.index, ] # 269 Records

## 2. Labelled vs Unlabelled splitting
lb.index <- createDataPartition(TrainDataSet$핵심특허, p = labelling_prop, list = FALSE)
labeledSet <- TrainDataSet[lb.index, ]
unlabeledSet <- TrainDataSet[-lb.index, ]
핵심특허실제값 <- unlabeledSet$핵심특허
unlabeledSet$핵심특허 <- NA

## 3. Model Fitting
set.seed(20191002)

# 불균형 데이터셋 처리
trainSet_up <- ROSE(핵심특허 ~ ., data = labeledSet)$data # ROSE 샘플링
# trainSet_up <- SMOTE(핵심특허 ~ ., data = labeledSet) # SMOTE 샘플링

# Make parallel cluster and register
cl = makeCluster(detectCores()-1) # n-1 core cluster 병렬처리를 위해 코어들을 할당하는 클러스터
registerDoParallel(cl) # 해당 클러스터를 작업하기 위해 할당해주는 것

# 교차검증 수행
fitControl = trainControl(method = "repeatedcv", number = cv_var, repeats = rpt_var,
                          classProbs = TRUE, summaryFunction = twoClassSummary)

# 튜닝을 위한 그리드 범위 설정
# fitGrid = expand.grid(sigma = 2^(-6:0), C = 10^(-2:3))

# names(getModelInfo()) # 사용할 수 있는 method 확인

# 모델 적합
svm_fit <- train(핵심특허 ~ ., data = trainSet_up, method = "svmRadial",
                     metric = "ROC", trControl = fitControl,
                     tunelength = 10,
                     # tuneGrid = fitGrid,
                     verbose = FALSE)
# svm_fit
# plot(svm_fit)

stopCluster(cl) # End parallel cluster

## 4. Predict and save results(지도학습만을 이용해 TestSet을 예측한 결과)
true.class <- TestDataSet$핵심특허
pred.class <- predict(svm_fit, TestDataSet, type = 'raw')
pred.prob <- predict(svm_fit, TestDataSet, type = 'prob')

temp_result <- confusionMatrix(pred.class, true.class, positive = "ET")
results <- rbind(results, c(temp_result$overall[1] %>% round(digits = 3),
                            temp_result$byClass[5] %>% round(digits = 3),
                            temp_result$byClass[6] %>% round(digits = 3),
                            temp_result$byClass[7] %>% round(digits = 3),
                            roc(true.class, pred.prob[, "ET"], levels = rev(levels(true.class)))[[9]][1] %>% round(digits = 3)))

semi_table <- rbind(semi_table, c(temp_result$table[1],
                                  temp_result$table[2],
                                  temp_result$table[3],
                                  temp_result$table[4]))

## 5. Semi-supervised learning (SSL)

while(TRUE){
  
  count <- count +1
  
  # UnlabeledSet 예측
  temp_for_insert <- predict(svm_fit, unlabeledSet[1:unit,], type = 'prob') %>% as.data.frame()
  
  lb_insert_size <- nrow(temp_for_insert) * 0.1 # 숫자 변경 가능
  
  lb_insert_vector <- (lb_insert_size * prop.table(labeledSet$핵심특허 %>% table())) %>% round(digits = 0) %>% as.vector()
  
  lb_cutoff_0 <- temp_for_insert %>% arrange(desc(`NET`)) %>% head(lb_insert_vector[1])[1, length(lb_insert_vector)]
  lb_cutoff_1 <- temp_for_insert %>% arrange(desc(`ET`)) %>% head(lb_insert_vector[2])[2, length(lb_insert_vector)]
  
  temp_for_insert[which(temp_for_insert[, 1] >= lb_cutoff_0), 3] <- "NET" 
  temp_for_insert[which(temp_for_insert[, 2] >= lb_cutoff_1), 3] <- "ET" 
  names(temp_for_insert)[3] <- "pred"
  temp_for_insert[ ,3] <- factor(temp_for_insert[ ,3], levels = c("NET", "ET"))
  
  # temp_for_insert에 실제값 추가
  temp_for_insert <- temp_for_insert %>% mutate(actual = 핵심특허실제값[1:unit])
  
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
  trainSet_up <- ROSE(핵심특허 ~ ., data = labeledSet)$data # ROSE 샘플링
  # trainSet_up <- SMOTE(핵심특허 ~ ., data = labeledSet) # SMOTE 샘플링
  
  # Make parallel cluster and register
  cl = makeCluster(detectCores()-1) # n-1 core cluster 병렬처리를 위해 코어들을 할당하는 클러스터
  registerDoParallel(cl) # 해당 클러스터를 작업하기 위해 할당해주는 것
  
  fitControl = trainControl(method = "repeatedcv", number = cv_var, repeats = rpt_var,
                            classProbs = TRUE, summaryFunction = twoClassSummary)
  
  # fitGrid = expand.grid(sigma = 2^(-6:0), C = 10^(-2:3))
  
  svm_fit <- train(핵심특허 ~ ., data = trainSet_up, method = "svmRadial",
                       metric = "ROC", trControl = fitControl,
                       tunelength = 10,
                       # tuneGrid = fitGrid,
                       verbose = FALSE)
  
  stopCluster(cl) # End parallel cluster
  
  true.class <- TestDataSet$핵심특허
  pred.class <- predict(svm_fit, TestDataSet, type = 'raw')
  pred.prob <- predict(svm_fit, TestDataSet, type = 'prob')
  
  temp_result <- confusionMatrix(pred.class, true.class, positive = "ET")
  results <- rbind(results, c(temp_result$overall[1] %>% round(digits = 3),
                              temp_result$byClass[5] %>% round(digits = 3),
                              temp_result$byClass[6] %>% round(digits = 3),
                              temp_result$byClass[7] %>% round(digits = 3),
                              roc(true.class, pred.prob[, "ET"], levels = rev(levels(true.class)))[[9]][1] %>% round(digits = 3)))
  
  
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

write_xlsx(semi_table,"./SVM_혼동행렬최종4.xlsx")
write_xlsx(results,"./SVM_결과최종4.xlsx")


