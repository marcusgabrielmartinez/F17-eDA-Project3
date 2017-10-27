library(tidyverse)
library(modelr)
require(dplyr)
require(data.world)
require(MASS)
require(ISLR)
require(ggplot2)
library(leaps)

data.world::set_config(save_config(auth_token = "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50Om1hcmN1c2dhYmUtdXQiLCJpc3MiOiJhZ2VudDptYXJjdXNnYWJlLXV0OjowYjE2NDQzOC1mYzRlLTRhNDktYWY1MC1iMWU1YjViYmIzYzMiLCJpYXQiOjE0ODQ4NjgyNjMsInJvbGUiOlsidXNlcl9hcGlfcmVhZCIsInVzZXJfYXBpX3dyaXRlIl0sImdlbmVyYWwtcHVycG9zZSI6dHJ1ZX0.Eb9i31mYAv6zQGjlze-PbiBJ_5_JNBDIZn51wcPnnNPny_ih2SSN9Ur_LVyRltEbrReNXM5b371XWrmMiexEKw"))
#vignette("quickstart", package = "data.world")

project <- "https://data.world/tarrantrl/parkinsons-telemonitoring"
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM parkinsons_telemonitoring"),
  dataset = project
)

summary(df)

#Add binary column for gender
df <- df %>% dplyr::mutate(sex2 = ifelse(sex == "true", 1, 0)) 

#Add column for age <=65 or not
df <- df %>% dplyr::mutate(age2 = ifelse(age <= 65, 1, 0)) #so 1 means age<=65 and 0 means age>65
summary(df)

attach(df)


### Insight 3 - age2 ###

##Best Subset Section##

# Builds a new dataframe that excludes a column that should not be included as a predictor.
df_subset <- df %>% dplyr::select(., -subject, -age, -sex2)

regfit.full=regsubsets(age2~.,data=df_subset, nvmax=22)
reg.summary=summary(regfit.full)
names(reg.summary)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp")
which.min(reg.summary$cp)
#points(10,reg.summary$cp[10],pch=20,col="red")

plot(reg.summary$adjr2,xlab="Number of Variables",ylab="adjr2")
which.min(reg.summary$adjr2)

summary(regfit.full)

plot(regfit.full,scale="Cp")
plot(regfit.full,scale="adjr2")
coef(regfit.full,10)

##Forward Selection Section##

regfit.fwd=regsubsets(age2~.,data=df_subset,nvmax=22,method="forward")
summary(regfit.fwd)

plot(regfit.fwd,scale="Cp")
plot(regfit.fwd,scale="adjr2")

regfwd.summary=summary(regfit.fwd)

which.min(regfwd.summary$cp)
which.min(regfwd.summary$adjr2)

plot(regfwd.summary$cp,xlab="Number of Variables",ylab="Cp")
plot(regfwd.summary$adjr2,xlab="Number of Variables",ylab="adjr2")

#KNN 
predictorsKNN2=cbind(age, hnr, dfa)
knn2.pred=class::knn(predictorsKNN2[train, ],predictorsKNN2[test_knn,],total_updrs[train],k=1)
table(knn2.pred,total_updrs[test_knn])
mean(knn2.pred==total_updrs[test_knn])




### KNN ### (marcus)
predictorsKNN=cbind(age, motor_updrs, total_updrs, jitter, jitter_abs, jitter_ppq5, rpde, dfa, ppe)
knn7.pred=class::knn(predictorsKNN7[train, ],predictorsKNN7[test_knn,],sex2[train],k=1)
table(knn7.pred,sex2[test_knn])
mean(knn7.pred==sex2[test_knn])



### Logistic Regression ###
#1 age+rpde+ppe+total_updrs predicting sex2
set.seed(1)
train = sample(nrow(df), 2937)
test = df[-train,]

glm1.fit=glm(sex2 ~ age + rpde + ppe + total_updrs,
             data=df, family=binomial,
             subset=train)
summary(glm1.fit)

glm1.probs=predict(glm1.fit,newdata=test,type="response")
glm1.pred=ifelse(glm1.probs>0.5,"1","0")
sex2.test = test$sex2
table(glm1.pred,sex2.test) #confusion matrix
mean(glm1.pred==sex2.test) #bad mean


### LDA ###
#1 age+rpde+ppe+total_updrs predicting sex2
lda1.fit=lda(sex2 ~ age + rpde + ppe + total_updrs,
             data=df, subset=train)
lda1.fit
lda1.pred=predict(lda1.fit, test)
lda1_df = data.frame(lda1.pred)
table(lda1.pred$class,test$sex2) #confusion matrix
mean(lda1.pred$class==test$sex2) #bad mean


### QDA ###
#1 age+rpde+ppe+total_updrs predicting sex2
qda1.fit = qda(sex2 ~ age + rpde + ppe + total_updrs,
               data=df, subset=train)
qda1.fit
qda1.pred = predict(qda1.fit, test)
table(qda1.pred$class,test$sex2)
mean(qda1.pred$class==test$sex2)


### KNN ###
predictorsKNN=cbind(age, motor_updrs, total_updrs, jitter, jitter_abs, jitter_ppq5, rpde, dfa, ppe)
knn7.pred=class::knn(predictorsKNN7[train, ],predictorsKNN7[test_knn,],sex2[train],k=1)
table(knn7.pred,sex2[test_knn])
mean(knn7.pred==sex2[test_knn])


### Insight 2: hnr vs Shimmer ###
plot(hnr~shimmer, data = df)
#ggplot(df, aes(x=shimmer, y=hnr)) + geom_hex(bins = 50)
glm.fit=glm(hnr~shimmer, data=df)
#cv.glm(df,glm.fit)$delta 

#Leave One Out Cross Validation - LOOCV
loocv=function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}
loocv(glm.fit)

cv.error=rep(0,6)
degree=1:6
for(d in degree){
  glm.fit=glm(nhr~poly(shimmer,d), data=df)
  cv.error[d]=loocv(glm.fit)
}
plot(degree,cv.error,type="b")

#K fold cross validation
cv.error10=rep(0,6)
for(d in degree){
  glm.fit=glm(nhr~poly(shimmer,d), data=df)
  cv.error10[d]=cv.glm(df,glm.fit,K=10)$delta[1]
}
lines(degree,cv.error10,type="b",col="red")
