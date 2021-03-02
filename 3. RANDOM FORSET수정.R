#####################################################
#                                                   #
#                                                   #
#     Author : 최영재                               #
#     Date : 2019 - 09 - 30                         #
#     Title : RANDOM FOREST                         #
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


temp_data <- read.csv('./temp.csv', header = TRUE, stringsAsFactors = TRUE)
expert_data <- read.csv('./전문가평가.csv', header = TRUE, stringsAsFactors = TRUE)
colnames(temp_data)
temp_data <- temp_data[,-c(34:39)]


## 3. 모형 적합 ----------------------------------------------------------------
load(file = './temp_data(기본 전처리 완료).Rdata')
str(temp_data)
colnames(temp_data)

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

temp_data[, 10] <- as.factor(temp_data[, 10])
temp_data[, 18] <- as.factor(temp_data[, 18])
temp_data[, 19] <- as.factor(temp_data[, 19])
temp_data[, 20] <- as.factor(temp_data[, 20])
temp_data[, 21] <- as.factor(temp_data[, 21])
temp_data[, 22] <- as.factor(temp_data[, 22])
temp_data[, 23] <- as.factor(temp_data[, 23])
temp_data[, 24] <- as.factor(temp_data[, 24])
temp_data[, 25] <- as.factor(temp_data[, 25])
temp_data[, 26] <- as.factor(temp_data[, 26])
temp_data[, 27] <- as.factor(temp_data[, 27])
temp_data[, 33] <- as.factor(temp_data[, 33])

expert_data[, 10] <- as.factor(expert_data[, 10])
expert_data[, 18] <- as.factor(expert_data[, 18])
expert_data[, 19] <- as.factor(expert_data[, 19])
expert_data[, 20] <- as.factor(expert_data[, 20])
expert_data[, 21] <- as.factor(expert_data[, 21])
expert_data[, 22] <- as.factor(expert_data[, 22])
expert_data[, 23] <- as.factor(expert_data[, 23])
expert_data[, 24] <- as.factor(expert_data[, 24])
expert_data[, 25] <- as.factor(expert_data[, 25])
expert_data[, 26] <- as.factor(expert_data[, 26])
expert_data[, 27] <- as.factor(expert_data[, 27])
expert_data[, 33] <- as.factor(expert_data[, 33])
expert_data[, 34] <- as.factor(expert_data[, 34])


levels(temp_data$핵심특허) <- c("NET", "ET")
levels(expert_data$핵심특허) <- c("NET", "ET")
levels(expert_data$전문가평가) <- c("NET", "ET")

## 1. Train vs Test splitting
tr.index <- createDataPartition(temp_data$핵심특허, p = trainsplitting_prop, list = FALSE)
TrainDataSet <- temp_data[tr.index, ] # 2429 Records
TestDataSet <- temp_data[-tr.index, ] # 269 Records

## 2. Labelled vs Unlabelled splitting
lb.index <- createDataPartition(TrainDataSet$핵심특허, p = labelling_prop, list = FALSE)
labeledSet <- TrainDataSet[lb.index, ] # 486 Records
labeledSet <- rbind(labeledSet, expert_data[,-c(34:39)])
unlabeledSet <- TrainDataSet[-lb.index, ] # 1943 Records
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
fitGrid = expand.grid(mtry = c(10, 15, 20), splitrule = "extratrees", min.node.size = c(1, 2, 3))

# names(getModelInfo()) # 사용할 수 있는 method 확인

# 모델 적합
rf_fit <- train(핵심특허 ~ ., data = trainSet_up, method = "ranger",
                    metric = "ROC", trControl = fitControl, 
                    tuneGrid = fitGrid,
                    verbose = FALSE)
# rf_fit
# plot(rf_fit)

stopCluster(cl) # End parallel cluster

## 4. Predict and save results(지도학습만을 이용해 TestSet을 예측한 결과)
true.class <- TestDataSet$핵심특허
pred.class <- predict(rf_fit, TestDataSet, type = 'raw')
pred.prob <- predict(rf_fit, TestDataSet, type = 'prob')

length(true.class)
length(pred.class)

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

print(results)
print(semi_table)


## 5. Semi-supervised learning (SSL)

while(TRUE){
  
  count <- count +1 # 준지도학습 진행 횟수

  temp_for_insert <- predict(rf_fit, unlabeledSet[1:unit,], type = 'prob') %>% as.data.frame() # UnlabeledSet 예측
  
  lb_insert_size <- nrow(temp_for_insert) * 0.3 #! 0.1 # labeled data set에 추가할 unlabeled data의 개수 설정 (비율 변경 가능)
  
  lb_insert_vector <- (lb_insert_size * prop.table(labeledSet$핵심특허 %>% table())) %>% round(digits = 0) %>% as.vector() # labeling 컷오프
  
  lb_cutoff_0 <- temp_for_insert %>% arrange(desc(`NET`)) %>% head(lb_insert_vector[1])[1, length(lb_insert_vector)] # NET 컷오프 계산
  lb_cutoff_1 <- temp_for_insert %>% arrange(desc(`ET`)) %>% head(lb_insert_vector[2])[2, length(lb_insert_vector)] # ET 컷오프 계산
  
  temp_for_insert[which(temp_for_insert[, 1] >= lb_cutoff_0), 3] <- "NET" # NET 컷오프 이상이면 NET로 라벨링
  temp_for_insert[which(temp_for_insert[, 2] >= lb_cutoff_1), 3] <- "ET" # ET 컷오프 이상이면 ET로 라벨링
  names(temp_for_insert)[3] <- "pred" # 3번째 열 이름 pred로 설정
  temp_for_insert[ ,3] <- factor(temp_for_insert[ ,3], levels = c("NET", "ET")) # pred열 factor형으로 변환
  
  temp_for_insert <- temp_for_insert %>% mutate(actual = 핵심특허실제값[1:unit]) # temp_for_insert에 실제값(정답 label) 추가
  
  ## 6. Active Learning (AL)
  
  # 분류확률이 0.40~0.60로 애매하여 ET 혹은 NET로 분류하지 못한 애들에게 실제값을 부여함
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
  
  fitGrid = expand.grid(mtry = c(10, 15, 20), splitrule = "extratrees", min.node.size = c(1, 2, 3))
  
  rf_fit <- train(핵심특허 ~ ., data = trainSet_up, method = "ranger",
                      metric = "ROC", trControl = fitControl, 
                      tuneGrid = fitGrid,
                      verbose = FALSE)
  
  stopCluster(cl) # End parallel cluster
  
  true.class <- TestDataSet$핵심특허
  pred.class <- predict(rf_fit, TestDataSet, type = 'raw')
  pred.prob <- predict(rf_fit, TestDataSet, type = 'prob')
  
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

write_xlsx(semi_table,"./RF_혼동행렬최종8.xlsx")
write_xlsx(results,"./RF_결과최종8.xlsx")


