#####################################################
#                                                   #
#                                                   #
#     Author : 최영재                               #
#     Date : 2020 - 03 - 17                         #
#     Title : Using experts Data                    #
#                                                   #
#                                                   #
#####################################################

## 0. 사전작업 -----------------------------------------------------------------

# 패키지 불러오기
library(caret) # machine learning
library(hrbrthemes) # visualization
library(e1071) # svm
library(corrplot) # correlation analysis
library(tidyverse) # data preprocessing
library(readxl) # excel
library(writexl) # excel
library(progress) # progress bar
library(naniar) # missing value
library(MLmetrics) # machine learning evaluation metrics
library(spacyr) # NLP
library(tm) # text mining
library(xgboost) # XGBoost
library(foreach) # parallel processing
library(doParallel) # parallel processing
library(pastecs) # descriptive statistics
library(ROSE) # ROSE 샘플링
library(pROC) # AUC값
library(ranger) # RandomForest
library(doParallel) # 병렬처리
library(xgboost) # XGBOOST
library(kernlab) # SVM



# 변수제거 및 작업공간 설정
rm(list = ls(all = TRUE))
setwd("C:/Users/Choi Young Jae/Desktop/2019 Semi-Supervised Learning")
source(file = './ROC_AUROC_fun.R')
set.seed(20200319)

## 1. 데이터 불러오기 및 전처리 -------------------------------------------------------
load(file = './temp_data(전문가DB활용).Rdata')
temp_data$핵심특허 <- NULL

# 전문가 DB 추가
temp_data$특허번호 <- temp_data$번호
temp_data <- temp_data[,-1]
temp_data$특허번호 <- temp_data$특허번호 %>% as.character()
unist_data <- read_excel("./data/전기차배터리200건_유니스트.xlsx")
hyundai_data <- read_excel("./data/전기차배터리200건_현대차.xlsx")
temp_data <- left_join(temp_data, unist_data[,1:3])
temp_data <- left_join(temp_data, hyundai_data[,1:3])

# 종속변수 생성
temp_data$핵심특허 <- ifelse(temp_data$유니스트기술성 == 'Y', 1,
                         ifelse(temp_data$현대기술성 == 'Y', 1, 0))
temp_data$핵심특허 <- temp_data$핵심특허 %>% as.factor()

temp_data$특허번호 <- NULL
temp_data$유니스트기술성 <- NULL
temp_data$유니스트시장성 <- NULL
temp_data$현대기술성 <- NULL
temp_data$현대시장성 <- NULL

str(temp_data$핵심특허)
summary(temp_data$핵심특허)


# 초기 설정
unit <- 200
count <- 0
semi_table <- c()
results_list <- list()
loocv_var <- 86

# save.image(file = './expert.Rdata')
# load(file = './expert.Rdata')

## Train vs Test splitting
temp_data$indx <- 1:nrow(temp_data)
labeledSet <- subset(temp_data, 핵심특허 == "0" | 핵심특허 == "1")
unlabeledSet <- temp_data[-which(temp_data$indx %in% labeledSet$indx),]
temp_data <- temp_data[,-length(temp_data)]
labeledSet <- labeledSet[,-length(labeledSet)]
unlabeledSet <- unlabeledSet[,-length(unlabeledSet)]

index <- sample(x = 1:2,
                size = nrow(x = labeledSet),
                prob = c(0.65, 0.35),
                replace =  T)

testdata <- labeledSet[index ==2,]
labeledSet <- labeledSet[index ==1,]
temp_data <- rbind(labeledSet, unlabeledSet)

i = 1
j = 1

for(j in 1:loocv_var){
  
  # Labelled vs Unlabelled splitting
  temp_data$indx <- 1:nrow(temp_data)
  labeledSet <- subset(temp_data, 핵심특허 == "0" | 핵심특허 == "1")
  unlabeledSet <- temp_data[-which(temp_data$indx %in% labeledSet$indx),]
  temp_data <- temp_data[,-length(temp_data)]
  labeledSet <- labeledSet[,-length(labeledSet)]
  unlabeledSet <- unlabeledSet[,-length(unlabeledSet)]
  unlabeledSet$핵심특허 <- 3
  results <- c()
  semi_table <- c()
  flds <- createFolds(labeledSet$핵심특허, k = loocv_var, list = TRUE, returnTrain = FALSE) # labeledSet$핵심특허를 data 개수만큼 fold로 나눔
  names(flds)[j] <- "test" # j번 째 fold를 test 데이터로 사용
  test_idx <- flds[names(flds) == "test"] %>% unlist() # test 데이터의 index값을 test_idx에 저장
  trdata <- labeledSet[-test_idx, ] # test_idx와 일치하는 행을 제외한 labeledSet을 train 데이터로 사용
  tedata <- labeledSet[test_idx, ] # test_idx와 일치하는 행을 labeledSet에서 뽑아 test 데이터로 사용
  md <- svm(formula = 핵심특허 ~ ., data = trdata, probability = TRUE)
  
  # 지도학습만을 이용한 TrainSet 성능 평가
  true <- testdata$핵심특허
  pred <- predict(md, testdata, type = 'class')
  temp_result <- confusionMatrix(true, pred, positive = "0")
  
  # trProb <- predict(object = md, newdata = trdata, probability = TRUE) %>% attr(which = 'probabilities')
  # trPred <- md$fitted
  # trReal <- trdata$핵심특허
  # temp_result <- confusionMatrix(data = trPred, reference = trReal, positive = '1')
  
  results <- rbind(results, c(temp_result$overall[1] %>% round(digits = 3),
                              temp_result$byClass[5] %>% round(digits = 3),
                              temp_result$byClass[6] %>% round(digits = 3),
                              temp_result$byClass[7] %>% round(digits = 3),
                              AUC(y_pred = pred, y_true = true)))
  
  # semi_table <- rbind(semi_table, c(temp_result$table[1],
  #                                   temp_result$table[2],
  #                                   temp_result$table[3],
  #                                   temp_result$table[4]))
  
  # Semi-supervised learning
  while(TRUE){
    
    count <- count + 1
    
    # UnlabeledSet 예측
    temp_for_insert <- predict(md, unlabeledSet[1:unit,], probability = TRUE) %>% attr(which = 'probabilities') %>% as.data.frame()
    
    # lb_insert_size <- nrow(temp_for_insert)*0.1
    # lb_insert_vector <- (lb_insert_size * prop.table(trdata$핵심특허 %>% table())) %>% round(digits = 0) %>% as.vector()
    # lb_cutoff_0 <- temp_for_insert %>% arrange(desc(`0`)) %>% head(lb_insert_vector[1])[1, length(lb_insert_vector)]
    # lb_cutoff_1 <- temp_for_insert %>% arrange(desc(`1`)) %>% head(lb_insert_vector[2])[2, length(lb_insert_vector)]
    
    
    
    temp_for_insert[which(temp_for_insert[, 1] >= 0.7), 3] <- "0"
    temp_for_insert[which(temp_for_insert[, 2] >= 0.7), 3] <- "1"
    names(temp_for_insert)[3] <- "pred"
    
    # temp_for_insert[ ,3] <- factor(temp_for_insert[ ,3], levels = c("0", "1"))
    
    unlabeledSet[1:unit, c('핵심특허')] <- temp_for_insert[, 3] # UnlabeledSet에 정답을 부여
    
    trdata <- rbind(trdata, unlabeledSet[1:unit, ]) %>% na.omit() # LabeledSet에 UnlabeledSet을 예측한 결과 추가
    
    unlabeledSet <- unlabeledSet[-c(1:unit), ] # UnlabeledSet에서 예측한 결과값들은 지우기
    
    # 새로운 LabeledSet으로 다시 모형 적합
    
    md <- svm(formula = 핵심특허 ~ ., data = trdata, probability = TRUE)
    
    true <- testdata$핵심특허
    pred <- predict(md, testdata, type = 'class')
    temp_result <- confusionMatrix(true, pred, positive = "0")
    
    # trProb <- predict(object = md, newdata = trdata, probability = TRUE) %>% attr(which = 'probabilities')
    # trPred <- md$fitted
    # trReal <- trdata$핵심특허
    # temp_result <- confusionMatrix(data = trPred, reference = trReal, positive = '1')
    results <- rbind(results, c(temp_result$overall[1] %>% round(digits = 3),
                                temp_result$byClass[5] %>% round(digits = 3),
                                temp_result$byClass[6] %>% round(digits = 3),
                                temp_result$byClass[7] %>% round(digits = 3),
                                AUC(y_pred = pred, y_true = true)))
    
    # semi_table <- rbind(semi_table, c(temp_result$table[1],
    #                                   temp_result$table[2],
    #                                   temp_result$table[3],
    #                                   temp_result$table[4]))
    
    if (unit > nrow(unlabeledSet)) {
      break
    }
  }
  cnt <- (i-1)*loocv_var + j
  
  results_list[[cnt]] <- results
}


loocv_result <- c()

j = 1
i = 1

for(j in 1: nrow(results_list[[1]])){
  
  temp_result <- c()
  
  for(i in 1:cnt){
    
    temp_result <- rbind(temp_result, c(results_list[[i]][j,1] %>% as.numeric(),
                                        results_list[[i]][j,2] %>% as.numeric(),
                                        results_list[[i]][j,3] %>% as.numeric(),
                                        results_list[[i]][j,4] %>% as.numeric(),
                                        results_list[[i]][j,5] %>% as.numeric()))
    
  }
  
  loocv_result <- rbind(loocv_result, c(paste0("iter",j-1),
                                        mean(temp_result[,1], na.rm = T) %>% round(digits = 3),
                                        mean(temp_result[,2], na.rm = T) %>% round(digits = 3),
                                        mean(temp_result[,3], na.rm = T) %>% round(digits = 3),
                                        mean(temp_result[,4], na.rm = T) %>% round(digits = 3),
                                        mean(temp_result[,5], na.rm = T) %>% round(digits = 3)
  ))
  
  
  colnames(loocv_result) <- c("Iteration", "Accuracy", "Precision", "Recall", "F1-Score", "AUROC")
  
}

loocv_result



# save.image(file = './expert_svm_ET.Rdata')
# load(file = './expert_svm_ET.Rdata')

# save.image(file = './expert_svm_NET.Rdata')
# load(file = './expert_svm_NET.Rdata')
