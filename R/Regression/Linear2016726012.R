install.packages("knitr")
library(knitr)
knitr::opts_chunk$set(echo = TRUE)
install.packages("knitr")
install.packages("lattice")
install.packages("UsingR")
install.packages("psych")

#데이터 읽어오기
dat<-read.csv(file ="archieve/50Startups.csv",header=T)
head(dat)
view(dat)
attach(dat)
names(dat)
subset <- cbind(dat$R.D.Spend, dat$Administration, dat$Marketing.Spend, dat$State, dat$Profit)
pairs.panels(subset)
##변수들에 대해서 모델 구축
model2<-lm(Profit ~ ., data=dat)
summary(model2)

#- p-value 값에 의해서 설명력 없는  State 제거
multi_model <- lm(Profit ~ R.D.Spend + Administration + Marketing.Spend, data = dat)
summary(multi_model)

#- p-value 값에 의해서 설명력 없는  Administration 제거
multi_model <- lm(Profit ~ R.D.Spend  + Marketing.Spend, data = dat)
summary(multi_model)

#- p-value 값에 의해서 설명력 부족한  Marketing.Spend 제거
#multi_model <- lm(Profit ~ R.D.Spend, data = dat)
#summary(multi_model)
# 모델에 대한 정확도가 떨어지므로 제거 기각


