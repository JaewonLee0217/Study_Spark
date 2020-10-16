install.packages("knitr")
library(MASS)
knitr::opts_chunk$set(echo = TRUE)
#함수 정의
perf_eval <- function(cm){
  # true positive rate
  TPR = Recall = cm[2,2]/sum(cm[2,])
  # precision
  Precision = cm[2,2]/sum(cm[,2])
  # true negative rate
  TNR = cm[1,1]/sum(cm[1,])
  # accuracy
  ACC = sum(diag(cm)) / sum(cm)
  # balance corrected accuracy (geometric mean)
  BCR = sqrt(TPR*TNR)
  # f1 measure
  F1 = 2 * Recall * Precision / (Recall + Precision)
  
  re <- data.frame(TPR = TPR,
                   Precision = Precision,
                   TNR = TNR,
                   ACC = ACC,
                   BCR = BCR,
                   F1 = F1)
  return(re)
}
# 로지스틱 회귀에 대한 데이터 로드
dat <- read.csv("archieve/diabetes2.csv")
head(dat) 

#데이터 디멘션 보면
dim(dat)

head(dat, 10)

#데이터 전처리
#colSums(is.na(dat))
#dat = na.omit(dat)
# Remove gender,customerID
#dat <- dat[, !colnames(dat) %in% c("prevalentStroke")]

#값을 세보면
table(dat$Churn)

library(dummies)

#데이터 학습/테스트 구분
set.seed(2020)
test_id <- sample(1:nrow(dat), round(nrow(dat)*0.93))
dat_train <- dat[-test_id, ]
dat_test <- dat[test_id, ]
print("Training: ", str(nrow(dat_train)))
print("Test: ", str(nrow(dat_test)))

#로지스틱 회귀 모형 구축
# train model
model <- glm(Outcome~., dat_train, family = binomial())
summary(model)
#예측수행
pred_prob <- predict(model, dat_test, type="response")
pred_class <- rep(0, nrow(dat_test))
pred_class[pred_prob > 0.5] <- 1 
cm <- table(pred=pred_class, actual=dat_test$Outcome)
perf_eval(cm)

#forward selection
model_fwd <- step(glm(Outcome ~ 1, dat_train, 
                      family = binomial()), 
                  direction = "forward", trace = 0,
                  scope = formula(model))

pred_prob <- predict(model_fwd, dat_test, type="response")
pred_class <- rep(0, nrow(dat_test))
pred_class[pred_prob > 0.5] <- 1
cm <- table(pred=pred_class, actual=dat_test$Outcome)
perf_eval(cm)

#Backward elimination
model_bwd <- step(glm(Outcome ~ ., dat_train, 
                      family = binomial()), 
                  direction = "backward", trace = 0,
                  scope = list(lower=Outcome ~ 1, upper = formula(model)))

pred_prob <- predict(model_bwd, dat_test, type="response")
pred_class <- rep(0, nrow(dat_test))
pred_class[pred_prob > 0.5] <- 1
cm <- table(pred=pred_class, actual=dat_test$Outcome)
perf_eval(cm)

#Stepwise selection
model_step <- step(glm(Outcome ~ ., dat_train,
                       family = binomial()), direction = "both", trace = 0,
                   scope = list(lower=Outcome ~ 1, upper = formula(model)))

pred_prob <- predict(model_step, dat_test, type="response")
pred_class <- rep(0, nrow(dat_test))
pred_class[pred_prob > 0.5] <- 1
cm <- table(pred=pred_class, actual=dat_test$Outcome)
perf_eval(cm)