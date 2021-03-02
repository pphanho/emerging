#####################################################
#                                                   #
#                                                   #
#     Author : 최영재                               #
#     Date : 2019 - 09 - 28                         #
#     Title : Ecosystem                             #
#                                                   #
#                                                   #
#####################################################

## 0. 사전작업 -----------------------------------------------------------------

# 패키지 불러오기
library(caret)
library(hrbrthemes)
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
library(xgboost)
library(foreach)
library(doParallel)

# 변수제거 및 작업공간 설정
rm(list = ls(all = TRUE))
setwd("D:/잼/연구/2019 Semi-Supervised Learning")

## 1. 데이터 불러오기 -------------------------------------------------------

# 원본데이터 불러오기
data.raw <- read.csv(file = "./data/FinalDB.csv")

## 2. 데이터 전처리 -------------------------------------------------------

# 분석을 위해 data.raw변수를 data.processed 변수에 저장
data.processed <- data.raw

# data.processed 구조 확인
str(data.processed)

# 피인용횟수 "error"값 NA로 변경 후 숫자형 변수로 변환
which(data.processed$피인용횟수 == "error")
data.processed$피인용횟수 <- gsub("error", NA, data.processed$피인용횟수)
data.processed$피인용횟수 <- data.processed$피인용횟수 %>% as.numeric()

# 도면수의 'error'값 NA로 변경 후 숫자형 변수로 변환
which(data.processed$도면수 == "error")
data.processed$도면수 <- gsub("error", NA, data.processed$도면수)
data.processed$도면수 <- data.processed$도면수 %>% as.numeric()

# 출원년도 칼럼 생성
year <- data.processed$출원일 %>% as.character() %>% str_sub(start = 1, end = 4) %>% as.numeric()
data.processed$출원년도 <- year %>% as.numeric()

# 분석에 필요한 컬럼을 위해 출원년도가 2009 ~ 2018년인 특허들만 선별 (최신의 특허들)
data.processed <- data.processed %>% filter(data.processed$출원년도 >= 2009) # 1244 obs. of 69 variables
table(data.processed$출원년도)

# 2-(1). CPC 역량 변수 -------------------------------------------------------

data.processed$CPC평균인용수 <- NA
data.processed$CPC평균피인용수 <- NA
data.processed$CPC평균패밀리국가수 <- NA
data.processed$CPC최신지수 <- 0

data.processed$공통특허분류 <- data.processed$공통특허분류 %>% as.character()

cpc_dic <- data.processed$공통특허분류 %>% str_split(", ") %>% unlist()
cpc_dic <- table(cpc_dic) %>% sort(decreasing = T) %>% head(1000) # CPC 빈도순으로 1000개 추출

cpc_df <- data.frame(cpc_dic)
cpc_df$평균인용수 <- NA
cpc_df$평균피인용수 <- NA
cpc_df$평균패밀리국가수 <- NA

# CPC 사전
len <- cpc_dic %>% length()
for(i in 1: len){
  
  temp_CPC <- cpc_dic %>% names() %>% `[`(i)
  
  temp_df <- data.processed[str_detect(data.processed$공통특허분류, pattern = temp_CPC) %>% which(),]
  
  cpc_df[i,3] <- temp_df %>% select(인용횟수) %>% unlist() %>% mean(na.rm = T) 
  cpc_df[i,4] <- temp_df %>% select(피인용횟수) %>% unlist() %>% mean(na.rm = T)
  cpc_df[i,5] <- temp_df %>% select(INPADOC패밀리국가수) %>% unlist() %>% mean(na.rm = T)
  
}

# CPC 배치
for(i in 1: nrow(data.processed)){
  
  temp_CPC <- data.processed$공통특허분류[i] %>% str_split(pattern = ", ") %>% unlist()
  temp_score <- c()
  temp_score2 <- c()
  temp_score3 <- c()
  
  for(j in 1: length(temp_CPC)){
    
    temp_score <- c(temp_score, cpc_df[cpc_df$cpc_dic == temp_CPC[j],3])
    temp_score2 <- c(temp_score2, cpc_df[cpc_df$cpc_dic == temp_CPC[j],4])
    temp_score3 <- c(temp_score3, cpc_df[cpc_df$cpc_dic == temp_CPC[j],5])
    
  }
  
  data.processed$CPC평균인용수[i] <- mean(temp_score, na.rm = T)
  data.processed$CPC평균피인용수[i] <- mean(temp_score2, na.rm = T)
  data.processed$CPC평균패밀리국가수[i] <- mean(temp_score3, na.rm = T)
}

# CPC 사전2(최신기준)
cpc_dic <- data.processed$공통특허분류 %>% str_split(", ") %>% unlist() 
cpc_dic <- table(cpc_dic) %>% sort(decreasing = T) %>% head(1000) # CPC 빈도순으로 1000개 추출

table(data.processed$출원년도)

cpc_near_dic <- data.processed %>% filter(data.processed$출원년도 >= 2016 & data.processed$출원년도 <= 2018) #최근 3년 기준 
cpc_near_dic <- cpc_near_dic$공통특허분류 %>% str_split(", ") %>% unlist() 
cpc_near_dic <- table(cpc_near_dic) %>% sort(decreasing = T) %>% as.data.frame()
colnames(cpc_near_dic) <- c("cpc", "freq")

cpc_before_dic <- data.processed %>% filter(data.processed$출원년도 >= 2013 &data.processed$출원년도 <= 2015) #지난 3년 기준 
cpc_before_dic <- cpc_before_dic$공통특허분류 %>% str_split(", ") %>% unlist() 
cpc_before_dic <- table(cpc_before_dic) %>% sort(decreasing = T) %>% as.data.frame()
colnames(cpc_before_dic) <- c("cpc", "freq")

cpc_dic2 <- merge(cpc_before_dic, cpc_near_dic, by = 'cpc')
colnames(cpc_dic2) <- c("cpc","before","near")
cpc_dic2$increase_rates <- (cpc_dic2$near - cpc_dic2$before)/cpc_dic2$before
cpc_dic2 <- arrange(cpc_dic2, desc(cpc_dic2$increase_rates))
cpc_dic2 <- cpc_dic2[cpc_dic2$cpc %in% names(cpc_dic),] %>% head((0.1 * nrow(cpc_dic2)) %>% as.integer())

len <- cpc_dic2 %>% nrow()

for(i in 1: len){
  
  temp_CPC <- cpc_dic2$cpc[i] %>% as.character()
  
  temp <- str_detect(data.processed$공통특허분류,pattern = temp_CPC) %>% which()
  
  data.processed[temp,"CPC최신지수"] <- data.processed[temp,"CPC최신지수"] + 1
  
}

summary(data.processed$CPC최신지수)


# 2-(2). 기업 역량 변수 -------------------------------------------------------

data.processed$기업평균인용수 <- NA
data.processed$기업평균피인용수 <- NA
data.processed$기업평균패밀리국가수 <- NA

len <- levels(data.processed$출원인대표명) %>% length()

for(i in 2:len){
  
  temp_corp <- table(data.processed$출원인대표명) %>% sort(decreasing = T)  %>% `[`(i) %>% names()
  
  temp <- data.processed %>% filter(data.processed$출원인대표명 == temp_corp) %>% 
    select(인용횟수) %>% unlist() %>% mean(na.rm=T)
  
  temp2 <- data.processed %>% filter(data.processed$출원인대표명 == temp_corp) %>% 
    select(피인용횟수) %>% unlist() %>% mean(na.rm=T)
  
  temp3 <- data.processed %>% filter(data.processed$출원인대표명 == temp_corp) %>% 
    select(INPADOC패밀리국가수) %>% unlist() %>% mean(na.rm=T)
  
  data.processed[which(data.processed$출원인대표명 == temp_corp), "기업평균인용수"] <- temp
  data.processed[which(data.processed$출원인대표명 == temp_corp), "기업평균피인용수"] <- temp2
  data.processed[which(data.processed$출원인대표명 == temp_corp), "기업평균패밀리국가수"] <- temp3
  
}

# 과거 5년과 최신 5년을 비교

temp <- data.processed %>% filter(data.processed$출원년도 >= 2009 & data.processed$출원년도 <= 2013)
temp <- table(temp$출원인대표명) %>% sort(decreasing = T) %>% as.data.frame()
colnames(temp) <- c("corp","freq")

temp2 <- data.processed %>% filter(data.processed$출원년도 >= 2014 & data.processed$출원년도 <= 2018)
temp2 <- table(temp2$출원인대표명) %>% sort(decreasing = T) %>% as.data.frame()
colnames(temp2) <- c("corp","freq")

corp_dic <- merge(temp,temp2, by = "corp")
colnames(corp_dic) <- c("corp","before","near")
corp_dic <- corp_dic[-1,]

corp_dic$before <- corp_dic$before +1
corp_dic$near <- corp_dic$near +1

corp_dic$increase_rates <- (corp_dic$near - corp_dic$before) / corp_dic$before
corp_dic <- corp_dic %>% arrange(desc(increase_rates))

corp_dic <- corp_dic %>% filter((corp_dic$increase_rates >= mean(corp_dic$increase_rates)) &
                                  corp_dic$near >= mean(corp_dic$near))

data.processed$기업최신성 <- "0"
data.processed$기업최신성[which(data.processed$출원인대표명 %in% corp_dic$corp)] <- "1"
data.processed$기업최신성 <- data.processed$기업최신성 %>% as.factor()



# 2-(3). 키워드 변수 -------------------------------------------------------

# spacyr 사용을 위해 https://github.com/quanteda/spacyr 참고
spacy_initialize()

# 명칭 관련
pb <- progress_bar$new(total = nrow(data.processed))

data.processed$명칭_명사 <- NA

# 제목 명사 추출
for(i in 1: nrow(data.processed)){
  
  pb$tick()
  
  temp_txt <- data.processed$명칭[i] %>% as.character()
  
  temp_parsed <- temp_txt %>% spacy_parse()
  
  temp_parsed <- temp_parsed %>% filter(temp_parsed$pos == "NOUN"  | temp_parsed$pos =="PROPN")
  
  data.processed$명칭_명사[i] <- paste(temp_parsed$lemma , collapse = " ")
  
}

corp1 <- VCorpus(VectorSource(data.processed$명칭_명사))

tdm1 <- TermDocumentMatrix(corp1 , control = list(weighting = weightTfIdf,
                                                  stopwords = stopwords(kind = "smart"),
                                                  removePunctuation = T,
                                                  removeNumbers = T,
                                                  tolower = T))

tdm2 <- tm::removeSparseTerms(tdm1, sparse = 0.99) # sparsity 0.999 제거 

freq <- rowSums(as.matrix(tdm2))
plot(sort(freq,decreasing = T),col = "blue", main= "Word TF-IDF frequencies" ,xlab = "TF-IDF-based rank", ylab ="TF-IDF")

# 명칭 체크
temp <- data.frame(sort(rowSums(as.matrix(tdm2)), decreasing=TRUE))
colnames(temp) <- c("freq")
rownames(temp)

str_view_all(tdm2$dimnames$Terms,"\\b(electr)+\\w*" ,match = T) 
str_view_all(tdm2$dimnames$Terms,"\\b(automo)+\\w*" ,match = T) 
str_view_all(tdm2$dimnames$Terms,"\\b(vehicle)+\\w*" ,match = T) 
str_view_all(tdm2$dimnames$Terms,"\\b(car)+\\w*" ,match = T) 
str_view_all(tdm2$dimnames$Terms,"\\b(hybrid)+\\w*" ,match = T) 
str_view_all(tdm2$dimnames$Terms,"\\b(battery)+\\w*" ,match = T) 

search_stopwords <- c("\\b(electr)+\\w*","\\b(automo)+\\w*","\\b(vehicle)+\\w*",
                      "\\b(car)+\\w*","\\b(hybrid)+\\w*","\\b(battery)+\\w*")

title_stopwords <- c("system","method","apparatus","device","module")

# 동의어 사전
str_view_all(tdm2$dimnames$Terms,"\\b(charg)+\\w*" ,match = T)

union_dic <- data.frame(original_word = character(0) , words = character(0) , stringsAsFactors = F)

union_dic <- rbind.data.frame(union_dic, c("charge" , "\\b(charg)+\\w*") ,stringsAsFactors = F)
colnames(union_dic) <- c("original", "pattern")

# 함수
changeTerms <- function(corpus, dictionary){
  
  temp_text <- sapply(corpus , `[[` , 'content')
  
  # n <- length(x = corpus)
  for(i in 1: nrow(dictionary)){
    print(paste("dictionary에 따라 정제 중(",i,"/",nrow(dictionary),")",sep=""))
    
    before <- dictionary$pattern[i]
    after <- dictionary$original[i]
    
    # 병렬처리
    sp =1
    
    row_limit =300
    rep_leng = length(corpus) / row_limit
    if(!is.integer(rep_leng)) rep_leng=as.integer(rep_leng)+1
    
    for(j in 1:rep_leng){
      
      print(paste("corpus별 정제 중(",j,"/",rep_leng,")",sep=""))
      
      # 정의
      ep = j*row_limit
      
      if(j == rep_leng) ep = length(corpus)
      
      # 본문
      temp_text[sp:ep] <- temp_text[sp:ep] %>% 
        sapply(FUN = function(x) gsub(pattern = before , replacement = after, x)) 
      
      # 정의
      sp = sp +row_limit
      
    }
    
  }
  
  corpus <- VCorpus(VectorSource(temp_text))
  
  return(corpus)
}

corp2 <- tm_map(corp1, content_transformer(tolower)) 
corp2 <- tm_map(corp2, content_transformer(removeWords), stopwords(kind = "smart")) 
corp2 <- tm_map(corp2, content_transformer(removePunctuation))
corp2 <- tm_map(corp2, content_transformer(removeNumbers))
corp2 <- tm_map(corp2, content_transformer(removeWords) , search_stopwords) # 스탑워즈 제거
corp2 <- tm_map(corp2, content_transformer(removeWords) , title_stopwords) # 스탑워즈 제거
corp2 <- tm_map(corp2, content_transformer(stripWhitespace)) # space delete

corp3 <- changeTerms(corpus = corp2, union_dic)
# corp3 <- changeTerms(corpus = corp3, union_dic)

tdm2 <- TermDocumentMatrix(corp3)
tdm2 <- tm::removeSparseTerms(tdm2 ,sparse = 0.99)

temp <- data.frame(sort(rowSums(as.matrix(tdm2)), decreasing=TRUE))

colnames(temp) <- c("freq")

title_dic <- temp %>% head(10)

temp <- sapply(corp3 , `[[` , 'content')

data.processed$명칭_명사 <- temp
data.processed$명칭_명사 <- str_trim(data.processed$명칭_명사)

data.processed$명칭_명사_수 <- ifelse(data.processed$명칭_명사 %>% str_length() == 0,
                                 yes = 0 ,
                                 str_count(data.processed$명칭_명사 , pattern = " ") +1)

data.processed$명칭_명사_수 %>% table()

rownames(title_dic)

data.processed <- data.processed %>% mutate(title_control = str_detect(명칭_명사,rownames(title_dic)[1]),
                                            title_power = str_detect(명칭_명사,rownames(title_dic)[2]),
                                            title_charge = str_detect(명칭_명사,rownames(title_dic)[3]),
                                            title_energy = str_detect(명칭_명사,rownames(title_dic)[4]),
                                            title_storage = str_detect(명칭_명사,rownames(title_dic)[5]),
                                            title_motor = str_detect(명칭_명사,rownames(title_dic)[6]),
                                            title_pack = str_detect(명칭_명사,rownames(title_dic)[7]),
                                            title_lithium = str_detect(명칭_명사,rownames(title_dic)[8]),
                                            title_management = str_detect(명칭_명사,rownames(title_dic)[9]),
                                            title_drive = str_detect(명칭_명사,rownames(title_dic)[10]))

temp <- data.processed %>% select(contains("title_")) %>% sapply(FUN = function(x)as.integer(x))
var_names <- data.processed %>% select(contains("title_")) %>% colnames()
data.processed[,var_names] <- temp
data.processed[,var_names] <- lapply(data.processed[,var_names], factor)

# save.image(file = './data_processed까지 진행.Rdata') # data.processed까지 완료된 데이터 저장
# load(file = './data_processed까지 진행.Rdata')

# 2-(4). 칼럼 선택 및 변형 -------------------------------------------------------

# 칼럼 선택

colnames(data.processed)

var_names <- c("피인용횟수", "번호", "요약", "출원인", "출원인국가", "발명자국가", "국제특허분류", "공통특허분류", "출원일", "독립.청구항수",
               "전체.청구항", "자국인용특허", "외국인용특허", "인용횟수", "발명자수", "비특허인용횟수", "도면수", "INPADOC패밀리국가수",
               "기업평균인용수","기업평균피인용수","기업평균패밀리국가수", "기업최신성","CPC평균인용수","CPC평균피인용수",
               "CPC평균패밀리국가수","CPC최신지수","출원년도", "명칭_명사_수", "RN값", "CF값")

var_names <- c(var_names, data.processed %>% select(contains("title_")) %>% colnames())

data.processed.selected <- data.processed[, var_names]

# 칼럼 변형 및 추가

# 출원인수 - strsplit로 |로 분리후 unique로 고유값 뽑은후 length 개수세기
data.processed.selected$출원인수 <- sapply(data.processed.selected[, '출원인'],
                                       function(y){length(unique((strsplit(as.character(y), "\\|")[[1]])))})

# 출원인국가수 - strsplit로 |로 분리후 unique로 고유값 뽑은후 length 개수세기
data.processed.selected$출원인국가수 <- sapply(data.processed.selected[, '출원인국가'],
                                         function(y){length(unique((strsplit(as.character(y), "\\|")[[1]])))})

# 발명자국가수 - strsplit로 |로 분리후 unique로 고유값 뽑은후 length 개수세기
data.processed.selected$발명자국가수 <- sapply(data.processed.selected[, '발명자국가'],
                                         function(y){length(unique((strsplit(as.character(y), "\\|")[[1]])))})

# 국제특허분류수 - strsplit로 ,로 분리후 length 개수세기
data.processed.selected$국제특허분류수 <- sapply(data.processed.selected[, '국제특허분류'],
                                          function(y){length(strsplit(as.character(y), ", ")[[1]])})

# 공통특허분류수 - strsplit로 ,로 분리후 length 개수세기
data.processed.selected$공통특허분류수 <- sapply(data.processed.selected[, '공통특허분류'],
                                          function(y){length(strsplit(as.character(y), ", ")[[1]])})

# 국제특허분류다양성 - strsplit로 ,로 분리후 substring로 1,4까지 4글짜 따옴. 그 후 unique로 고유값 뽑은후 length 개수세기
data.processed.selected$국제특허분류다양성 <- sapply(data.processed.selected[, '국제특허분류'],
                                            function(y){length(unique(substring(strsplit(as.character(y), ", ")[[1]], 1, 4)))})

# 공통특허분류다양성 - strsplit로 ,로 분리후 substring로 1,4까지 4글짜 따옴. 그 후 unique로 고유값 뽑은후 length 개수세기
data.processed.selected$공통특허분류다양성 <- sapply(data.processed.selected[, '공통특허분류'],
                                            function(y){length(unique(substring(strsplit(as.character(y), ", ")[[1]], 1, 4)))})

# 분석에 사용할 칼럼 선별
colnames(data.processed.selected)
data.processed.selected <- data.processed.selected[, c(1, 2, 10, 14:length(data.processed.selected))]

# 종속변수 NA값 제거 (NA의 이유 = 특허 자체가 사라진 특허임)
data.processed.selected <- data.processed.selected %>% filter(!is.na(data.processed.selected$피인용횟수)) # 2827 Records of 36 variables

# 전체 변수 NA값 여부 확인
naniar::miss_var_summary(data.processed.selected)

# 종속변수 설정 전까지의데이터 저장
# save(data.processed.selected, file = './data_processed_selected까지 진행(종속변수 설정 전).Rdata')
# load(file = './data_processed_selected까지 진행(종속변수 설정 전).Rdata')

# 종속변수 설정
data.processed.selected$출원년도 <- data.processed.selected$출원년도 %>% as.factor()

temp_data <- data.processed.selected # 종속변수 labeling을 위해 temp_data에 저장

temp_data <- temp_data %>% na.omit() # 2698 Records of 36 variables

temp_data$핵심특허 <- NA # Make output variable column

after_data <- data.frame()

# 핵심특허 피인용수 상위 10%
for(i in 1 : length(levels(temp_data$출원년도))){
  
  temp_year <- levels(temp_data$출원년도)[i]
  year_data <- temp_data[temp_data$출원년도 == temp_year, ] # temp_data의 출원년도가 temp_year이랑 같은 Row만 선별하여 year_data에 저장
  
  top_10_pct <- (nrow(year_data) * 0.10) %>% round(digits = 0) # 상위 10%에 해당하는 수를 변수 top_5_pct에 저장
  
  if(top_10_pct == 0) {year_data$핵심특허 <- "0" # 만약 상위 10%에 해당하는 특허의 개수가 한 개도 없다면 해당 특허의 핵심특허 여부는 "0"
  next()} # 다시 반복문 수행
  temp <- head(sort(year_data$피인용횟수 ,decreasing = T), top_10_pct) # 상위 10%에 해당하는 특허의 피인용횟수를 temp에 저장
  temp <- temp[length(temp)]
  
  year_data$핵심특허 <- ifelse(year_data$피인용횟수 >= temp, "1","0")
  
  after_data <- rbind(after_data, year_data)
  
}

# 핵심특허 Mu + Sigma
# for (i in 1:length(levels(temp_data$출원년도))){
# 
#   temp_year <- levels(temp_data$출원년도)[i]
#   year_data <- temp_data[temp_data$출원년도 == temp_year, ]
# 
#   mu <- mean(year_data$피인용횟수)
#   sd <- sd(year_data$피인용횟수)
#   mu_plus_sd <- mu + sd
# 
#   year_data$핵심특허 <- ifelse(year_data$피인용횟수 >= mu_plus_sd,
#                            "1",
#                            "0")
#   year_data$핵심특허
# 
#   after_data <- rbind(after_data, year_data)
# 
# }


# 종속변수 설정이 완료된 after_data를 temp_data에 다시 저장
temp_data <- after_data

temp_data$피인용횟수 <- NULL
temp_data$출원일 <- NULL
temp_data$외국인용특허 <- NULL
temp_data$출원년도 <- NULL

temp_data$핵심특허 <- temp_data$핵심특허 %>% as.factor()

# save(temp_data, file = './citedby10%.Rdata') # 종속변수 피인용수 상위 10% 데이터 저장
# load(file = './citedby10%.Rdata')
# save(temp_data, file = './Mu_Plus_Sigma.Rdata') # 종속변수 Mu + Sigma 데이터 저장
# load(file = './Mu_Plus_Sigma.Rdata')



# 2-(5). 분석을 위한 추가적인 Feature Selection & Feature Scaling -------------------------------------------------------

# A. Skewness 
# B. Near-Zero Variance 
# C. Multicollinearity
# D. Variable Importance

# A. Skewness 해결을 위해 변수 선별 및 log transformation 적용
colnames(temp_data)
skew_temp_data <- temp_data[, -c(1, 11, 19:28, 34)] # 양적 변수만 선별
skewValues <- apply(skew_temp_data, 2, skewness) %>% as.data.frame()
skewValues

# temp_data에서 왜도 값이 2이상인 값들은 로그변환
# temp_data에서 왜도 값이 -2이하인 값들은 제곱

temp_data$독립.청구항수 <- log(temp_data$독립.청구항수+1)
temp_data$인용횟수 <- log(temp_data$인용횟수+1)
temp_data$비특허인용횟수 <- log(temp_data$비특허인용횟수+1)
temp_data$도면수 <- log(temp_data$도면수+1)
temp_data$INPADOC패밀리국가수 <- log(temp_data$INPADOC패밀리국가수 +1)
temp_data$기업평균인용수 <- log(temp_data$기업평균인용수 + 1)
temp_data$기업평균피인용수 <- log(temp_data$기업평균피인용수 + 1)
temp_data$기업평균패밀리국가수 <- log(temp_data$기업평균패밀리국가수 + 1)
temp_data$CPC평균인용수 <- log(temp_data$CPC평균인용수 + 1)
temp_data$CPC평균피인용수 <- log(temp_data$CPC평균피인용수 + 1)
temp_data$CPC평균패밀리국가수 <- log(temp_data$CPC평균패밀리국가수 + 1)
temp_data$CPC최신지수 <- log(temp_data$CPC최신지수 + 1)
temp_data$명칭_명사_수 <- log(temp_data$명칭_명사_수 + 1)
temp_data$RN값 <- log(temp_data$RN값 + 1)
temp_data$출원인수 <- log(temp_data$출원인수 +1)
temp_data$출원인국가수 <- temp_data$출원인국가수^2
temp_data$발명자국가수 <- log(temp_data$발명자국가수 + 1)
temp_data$국제특허분류수 <- log(temp_data$국제특허분류수 + 1)
temp_data$공통특허분류수 <- log(temp_data$공통특허분류수 +1)


# B. Near-Zero Variance인 변수들 선별 및 제거
colnames(temp_data)
zero_temp_data <- temp_data[, -c(1, 11, 19:28, 36)] # 양적 변수만 선별
NearZeroValues <- nearZeroVar(zero_temp_data, saveMetrics = TRUE)

# 분산이 0인 변수 제거
temp_data <- temp_data[, -c(30, 31)]


# C. Multicollinearity를 고려하여 변수간 상관계수 확인 및 상관계수가 너무 큰 변수 제거
colnames(temp_data)
cor_temp_data <- temp_data[, -c(10, 18:27, 33)] # 양적 변수만 선별
cor_matrix <- round(cor(cor_temp_data), digits = 3)
corrplot.mixed(cor_matrix, order = 'hclust', lower = 'ellipse', upper = 'number') # 시각화
highCor = findCorrelation(cor_matrix, cutoff = 0.8, names = T) # 상관계수 0.8이상인 변수 없음


# D. 변수 중요도를 확인하여 중요도가 0인 변수 제거


# save(temp_data, file = './Ecosystem.Rdata') # 전처리 완료된 최종 데이터 저장
# load(file = './Ecosystem.Rdata')

