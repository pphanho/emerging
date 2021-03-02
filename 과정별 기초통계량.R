library(pastecs) # descriptive statistics

temp_data$독립.청구항수 <- exp(log(temp_data$독립.청구항수+1))-1
temp_data$인용횟수 <- exp(log(temp_data$인용횟수+1))-1
temp_data$비특허인용횟수 <- exp(log(temp_data$비특허인용횟수+1))-1
temp_data$도면수 <- exp(log(temp_data$도면수+1))-1
temp_data$INPADOC패밀리국가수 <- exp(log(temp_data$INPADOC패밀리국가수 +1))-1
temp_data$기업평균인용수 <- exp(log(temp_data$기업평균인용수 + 1))-1
temp_data$기업평균피인용수 <- exp(log(temp_data$기업평균피인용수 + 1))-1
temp_data$기업평균패밀리국가수 <- exp(log(temp_data$기업평균패밀리국가수 + 1))-1
temp_data$CPC평균인용수 <- exp(log(temp_data$CPC평균인용수 + 1))-1
temp_data$CPC평균피인용수 <- exp(log(temp_data$CPC평균피인용수 + 1))-1
temp_data$CPC평균패밀리국가수 <- exp(log(temp_data$CPC평균패밀리국가수 + 1))-1
temp_data$CPC최신지수 <- exp(log(temp_data$CPC최신지수 + 1))-1
temp_data$RN값 <- exp(log(temp_data$RN값 + 1))-1
temp_data$출원인수 <- exp(log(temp_data$출원인수 +1))-1
temp_data$국제특허분류수 <- exp(log(temp_data$국제특허분류수 + 1))-1
temp_data$공통특허분류수 <- exp(log(temp_data$공통특허분류수 +1))-1

quanti_data <- temp_data %>% purrr::discard(is.factor) # 양적 변수만 선별



## SL / SVM
quanti_data <- labeledSet %>% purrr::discard(is.factor)

quanti_data$독립.청구항수 <- exp(log(quanti_data$독립.청구항수+1))-1
quanti_data$인용횟수 <- exp(log(quanti_data$인용횟수+1))-1
quanti_data$비특허인용횟수 <- exp(log(quanti_data$비특허인용횟수+1))-1
quanti_data$도면수 <- exp(log(quanti_data$도면수+1))-1
quanti_data$INPADOC패밀리국가수 <- exp(log(quanti_data$INPADOC패밀리국가수 +1))-1
quanti_data$기업평균인용수 <- exp(log(quanti_data$기업평균인용수 + 1))-1
quanti_data$기업평균피인용수 <- exp(log(quanti_data$기업평균피인용수 + 1))-1
quanti_data$기업평균패밀리국가수 <- exp(log(quanti_data$기업평균패밀리국가수 + 1))-1
quanti_data$CPC평균인용수 <- exp(log(quanti_data$CPC평균인용수 + 1))-1
quanti_data$CPC평균피인용수 <- exp(log(quanti_data$CPC평균피인용수 + 1))-1
quanti_data$CPC평균패밀리국가수 <- exp(log(quanti_data$CPC평균패밀리국가수 + 1))-1
quanti_data$CPC최신지수 <- exp(log(quanti_data$CPC최신지수 + 1))-1
quanti_data$RN값 <- exp(log(quanti_data$RN값 + 1))-1
quanti_data$출원인수 <- exp(log(quanti_data$출원인수 +1))-1
quanti_data$국제특허분류수 <- exp(log(quanti_data$국제특허분류수 + 1))-1
quanti_data$공통특허분류수 <- exp(log(quanti_data$공통특허분류수 +1))-1

write.csv(stat.desc(quanti_data), './SL_SVM.csv')

## AL / SVM
quanti_data <- labeledSet %>% purrr::discard(is.factor)

quanti_data$독립.청구항수 <- exp(log(quanti_data$독립.청구항수+1))-1
quanti_data$인용횟수 <- exp(log(quanti_data$인용횟수+1))-1
quanti_data$비특허인용횟수 <- exp(log(quanti_data$비특허인용횟수+1))-1
quanti_data$도면수 <- exp(log(quanti_data$도면수+1))-1
quanti_data$INPADOC패밀리국가수 <- exp(log(quanti_data$INPADOC패밀리국가수 +1))-1
quanti_data$기업평균인용수 <- exp(log(quanti_data$기업평균인용수 + 1))-1
quanti_data$기업평균피인용수 <- exp(log(quanti_data$기업평균피인용수 + 1))-1
quanti_data$기업평균패밀리국가수 <- exp(log(quanti_data$기업평균패밀리국가수 + 1))-1
quanti_data$CPC평균인용수 <- exp(log(quanti_data$CPC평균인용수 + 1))-1
quanti_data$CPC평균피인용수 <- exp(log(quanti_data$CPC평균피인용수 + 1))-1
quanti_data$CPC평균패밀리국가수 <- exp(log(quanti_data$CPC평균패밀리국가수 + 1))-1
quanti_data$CPC최신지수 <- exp(log(quanti_data$CPC최신지수 + 1))-1
quanti_data$RN값 <- exp(log(quanti_data$RN값 + 1))-1
quanti_data$출원인수 <- exp(log(quanti_data$출원인수 +1))-1
quanti_data$국제특허분류수 <- exp(log(quanti_data$국제특허분류수 + 1))-1
quanti_data$공통특허분류수 <- exp(log(quanti_data$공통특허분류수 +1))-1

write.csv(stat.desc(quanti_data), './AL_SVM.csv')

## SSL / SVM
quanti_data <- labeledSet %>% purrr::discard(is.factor)

quanti_data$독립.청구항수 <- exp(log(quanti_data$독립.청구항수+1))-1
quanti_data$인용횟수 <- exp(log(quanti_data$인용횟수+1))-1
quanti_data$비특허인용횟수 <- exp(log(quanti_data$비특허인용횟수+1))-1
quanti_data$도면수 <- exp(log(quanti_data$도면수+1))-1
quanti_data$INPADOC패밀리국가수 <- exp(log(quanti_data$INPADOC패밀리국가수 +1))-1
quanti_data$기업평균인용수 <- exp(log(quanti_data$기업평균인용수 + 1))-1
quanti_data$기업평균피인용수 <- exp(log(quanti_data$기업평균피인용수 + 1))-1
quanti_data$기업평균패밀리국가수 <- exp(log(quanti_data$기업평균패밀리국가수 + 1))-1
quanti_data$CPC평균인용수 <- exp(log(quanti_data$CPC평균인용수 + 1))-1
quanti_data$CPC평균피인용수 <- exp(log(quanti_data$CPC평균피인용수 + 1))-1
quanti_data$CPC평균패밀리국가수 <- exp(log(quanti_data$CPC평균패밀리국가수 + 1))-1
quanti_data$CPC최신지수 <- exp(log(quanti_data$CPC최신지수 + 1))-1
quanti_data$RN값 <- exp(log(quanti_data$RN값 + 1))-1
quanti_data$출원인수 <- exp(log(quanti_data$출원인수 +1))-1
quanti_data$국제특허분류수 <- exp(log(quanti_data$국제특허분류수 + 1))-1
quanti_data$공통특허분류수 <- exp(log(quanti_data$공통특허분류수 +1))-1

write.csv(stat.desc(quanti_data), './SSL_SVM.csv')


## SL / RF
quanti_data <- labeledSet %>% purrr::discard(is.factor)

quanti_data$독립.청구항수 <- exp(log(quanti_data$독립.청구항수+1))-1
quanti_data$인용횟수 <- exp(log(quanti_data$인용횟수+1))-1
quanti_data$비특허인용횟수 <- exp(log(quanti_data$비특허인용횟수+1))-1
quanti_data$도면수 <- exp(log(quanti_data$도면수+1))-1
quanti_data$INPADOC패밀리국가수 <- exp(log(quanti_data$INPADOC패밀리국가수 +1))-1
quanti_data$기업평균인용수 <- exp(log(quanti_data$기업평균인용수 + 1))-1
quanti_data$기업평균피인용수 <- exp(log(quanti_data$기업평균피인용수 + 1))-1
quanti_data$기업평균패밀리국가수 <- exp(log(quanti_data$기업평균패밀리국가수 + 1))-1
quanti_data$CPC평균인용수 <- exp(log(quanti_data$CPC평균인용수 + 1))-1
quanti_data$CPC평균피인용수 <- exp(log(quanti_data$CPC평균피인용수 + 1))-1
quanti_data$CPC평균패밀리국가수 <- exp(log(quanti_data$CPC평균패밀리국가수 + 1))-1
quanti_data$CPC최신지수 <- exp(log(quanti_data$CPC최신지수 + 1))-1
quanti_data$RN값 <- exp(log(quanti_data$RN값 + 1))-1
quanti_data$출원인수 <- exp(log(quanti_data$출원인수 +1))-1
quanti_data$국제특허분류수 <- exp(log(quanti_data$국제특허분류수 + 1))-1
quanti_data$공통특허분류수 <- exp(log(quanti_data$공통특허분류수 +1))-1

write.csv(stat.desc(quanti_data), './SL_RF.csv')

## AL / RF
quanti_data <- labeledSet %>% purrr::discard(is.factor)

quanti_data$독립.청구항수 <- exp(log(quanti_data$독립.청구항수+1))-1
quanti_data$인용횟수 <- exp(log(quanti_data$인용횟수+1))-1
quanti_data$비특허인용횟수 <- exp(log(quanti_data$비특허인용횟수+1))-1
quanti_data$도면수 <- exp(log(quanti_data$도면수+1))-1
quanti_data$INPADOC패밀리국가수 <- exp(log(quanti_data$INPADOC패밀리국가수 +1))-1
quanti_data$기업평균인용수 <- exp(log(quanti_data$기업평균인용수 + 1))-1
quanti_data$기업평균피인용수 <- exp(log(quanti_data$기업평균피인용수 + 1))-1
quanti_data$기업평균패밀리국가수 <- exp(log(quanti_data$기업평균패밀리국가수 + 1))-1
quanti_data$CPC평균인용수 <- exp(log(quanti_data$CPC평균인용수 + 1))-1
quanti_data$CPC평균피인용수 <- exp(log(quanti_data$CPC평균피인용수 + 1))-1
quanti_data$CPC평균패밀리국가수 <- exp(log(quanti_data$CPC평균패밀리국가수 + 1))-1
quanti_data$CPC최신지수 <- exp(log(quanti_data$CPC최신지수 + 1))-1
quanti_data$RN값 <- exp(log(quanti_data$RN값 + 1))-1
quanti_data$출원인수 <- exp(log(quanti_data$출원인수 +1))-1
quanti_data$국제특허분류수 <- exp(log(quanti_data$국제특허분류수 + 1))-1
quanti_data$공통특허분류수 <- exp(log(quanti_data$공통특허분류수 +1))-1

write.csv(stat.desc(quanti_data), './AL_RF.csv')

## SSL / RF
quanti_data <- labeledSet %>% purrr::discard(is.factor)

quanti_data$독립.청구항수 <- exp(log(quanti_data$독립.청구항수+1))-1
quanti_data$인용횟수 <- exp(log(quanti_data$인용횟수+1))-1
quanti_data$비특허인용횟수 <- exp(log(quanti_data$비특허인용횟수+1))-1
quanti_data$도면수 <- exp(log(quanti_data$도면수+1))-1
quanti_data$INPADOC패밀리국가수 <- exp(log(quanti_data$INPADOC패밀리국가수 +1))-1
quanti_data$기업평균인용수 <- exp(log(quanti_data$기업평균인용수 + 1))-1
quanti_data$기업평균피인용수 <- exp(log(quanti_data$기업평균피인용수 + 1))-1
quanti_data$기업평균패밀리국가수 <- exp(log(quanti_data$기업평균패밀리국가수 + 1))-1
quanti_data$CPC평균인용수 <- exp(log(quanti_data$CPC평균인용수 + 1))-1
quanti_data$CPC평균피인용수 <- exp(log(quanti_data$CPC평균피인용수 + 1))-1
quanti_data$CPC평균패밀리국가수 <- exp(log(quanti_data$CPC평균패밀리국가수 + 1))-1
quanti_data$CPC최신지수 <- exp(log(quanti_data$CPC최신지수 + 1))-1
quanti_data$RN값 <- exp(log(quanti_data$RN값 + 1))-1
quanti_data$출원인수 <- exp(log(quanti_data$출원인수 +1))-1
quanti_data$국제특허분류수 <- exp(log(quanti_data$국제특허분류수 + 1))-1
quanti_data$공통특허분류수 <- exp(log(quanti_data$공통특허분류수 +1))-1

write.csv(stat.desc(quanti_data), './SSL_RF.csv')

## SL / XGB
quanti_data <- labeledSet[,-c(10, 18:27, 33)]

quanti_data$독립.청구항수 <- exp(log(quanti_data$독립.청구항수+1))-1
quanti_data$인용횟수 <- exp(log(quanti_data$인용횟수+1))-1
quanti_data$비특허인용횟수 <- exp(log(quanti_data$비특허인용횟수+1))-1
quanti_data$도면수 <- exp(log(quanti_data$도면수+1))-1
quanti_data$INPADOC패밀리국가수 <- exp(log(quanti_data$INPADOC패밀리국가수 +1))-1
quanti_data$기업평균인용수 <- exp(log(quanti_data$기업평균인용수 + 1))-1
quanti_data$기업평균피인용수 <- exp(log(quanti_data$기업평균피인용수 + 1))-1
quanti_data$기업평균패밀리국가수 <- exp(log(quanti_data$기업평균패밀리국가수 + 1))-1
quanti_data$CPC평균인용수 <- exp(log(quanti_data$CPC평균인용수 + 1))-1
quanti_data$CPC평균피인용수 <- exp(log(quanti_data$CPC평균피인용수 + 1))-1
quanti_data$CPC평균패밀리국가수 <- exp(log(quanti_data$CPC평균패밀리국가수 + 1))-1
quanti_data$CPC최신지수 <- exp(log(quanti_data$CPC최신지수 + 1))-1
quanti_data$RN값 <- exp(log(quanti_data$RN값 + 1))-1
quanti_data$출원인수 <- exp(log(quanti_data$출원인수 +1))-1
quanti_data$국제특허분류수 <- exp(log(quanti_data$국제특허분류수 + 1))-1
quanti_data$공통특허분류수 <- exp(log(quanti_data$공통특허분류수 +1))-1

write.csv(stat.desc(quanti_data), './SL_XGB.csv')

## AL / XGB
quanti_data <- labeledSet[,-c(10, 18:27, 33)]

quanti_data$독립.청구항수 <- exp(log(quanti_data$독립.청구항수+1))-1
quanti_data$인용횟수 <- exp(log(quanti_data$인용횟수+1))-1
quanti_data$비특허인용횟수 <- exp(log(quanti_data$비특허인용횟수+1))-1
quanti_data$도면수 <- exp(log(quanti_data$도면수+1))-1
quanti_data$INPADOC패밀리국가수 <- exp(log(quanti_data$INPADOC패밀리국가수 +1))-1
quanti_data$기업평균인용수 <- exp(log(quanti_data$기업평균인용수 + 1))-1
quanti_data$기업평균피인용수 <- exp(log(quanti_data$기업평균피인용수 + 1))-1
quanti_data$기업평균패밀리국가수 <- exp(log(quanti_data$기업평균패밀리국가수 + 1))-1
quanti_data$CPC평균인용수 <- exp(log(quanti_data$CPC평균인용수 + 1))-1
quanti_data$CPC평균피인용수 <- exp(log(quanti_data$CPC평균피인용수 + 1))-1
quanti_data$CPC평균패밀리국가수 <- exp(log(quanti_data$CPC평균패밀리국가수 + 1))-1
quanti_data$CPC최신지수 <- exp(log(quanti_data$CPC최신지수 + 1))-1
quanti_data$RN값 <- exp(log(quanti_data$RN값 + 1))-1
quanti_data$출원인수 <- exp(log(quanti_data$출원인수 +1))-1
quanti_data$국제특허분류수 <- exp(log(quanti_data$국제특허분류수 + 1))-1
quanti_data$공통특허분류수 <- exp(log(quanti_data$공통특허분류수 +1))-1

write.csv(stat.desc(quanti_data), './AL_XGB.csv')

## SSL / XGB
quanti_data <- labeledSet[,-c(10, 18:27, 33)]

quanti_data$독립.청구항수 <- exp(log(quanti_data$독립.청구항수+1))-1
quanti_data$인용횟수 <- exp(log(quanti_data$인용횟수+1))-1
quanti_data$비특허인용횟수 <- exp(log(quanti_data$비특허인용횟수+1))-1
quanti_data$도면수 <- exp(log(quanti_data$도면수+1))-1
quanti_data$INPADOC패밀리국가수 <- exp(log(quanti_data$INPADOC패밀리국가수 +1))-1
quanti_data$기업평균인용수 <- exp(log(quanti_data$기업평균인용수 + 1))-1
quanti_data$기업평균피인용수 <- exp(log(quanti_data$기업평균피인용수 + 1))-1
quanti_data$기업평균패밀리국가수 <- exp(log(quanti_data$기업평균패밀리국가수 + 1))-1
quanti_data$CPC평균인용수 <- exp(log(quanti_data$CPC평균인용수 + 1))-1
quanti_data$CPC평균피인용수 <- exp(log(quanti_data$CPC평균피인용수 + 1))-1
quanti_data$CPC평균패밀리국가수 <- exp(log(quanti_data$CPC평균패밀리국가수 + 1))-1
quanti_data$CPC최신지수 <- exp(log(quanti_data$CPC최신지수 + 1))-1
quanti_data$RN값 <- exp(log(quanti_data$RN값 + 1))-1
quanti_data$출원인수 <- exp(log(quanti_data$출원인수 +1))-1
quanti_data$국제특허분류수 <- exp(log(quanti_data$국제특허분류수 + 1))-1
quanti_data$공통특허분류수 <- exp(log(quanti_data$공통특허분류수 +1))-1

write.csv(stat.desc(quanti_data), './SSL_XGB.csv')





