library(tidyverse)
library(modelr)
require(dplyr)
require(data.world)
require(MASS)
require(ISLR)
require(ggplot2)
library(leaps)
library(glmnet)
library(boot)


data.world::set_config(save_config(auth_token = "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50Om1hcmN1c2dhYmUtdXQiLCJpc3MiOiJhZ2VudDptYXJjdXNnYWJlLXV0OjowYjE2NDQzOC1mYzRlLTRhNDktYWY1MC1iMWU1YjViYmIzYzMiLCJpYXQiOjE0ODQ4NjgyNjMsInJvbGUiOlsidXNlcl9hcGlfcmVhZCIsInVzZXJfYXBpX3dyaXRlIl0sImdlbmVyYWwtcHVycG9zZSI6dHJ1ZX0.Eb9i31mYAv6zQGjlze-PbiBJ_5_JNBDIZn51wcPnnNPny_ih2SSN9Ur_LVyRltEbrReNXM5b371XWrmMiexEKw"))
#vignette("quickstart", package = "data.world")


project <- "https://data.world/tarrantrl/parkinsons-telemonitoring"
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM parkinsons_telemonitoring"),
  dataset = project
)

summary(df)
names(df)
df <- df %>% dplyr::mutate(sex2 = ifelse(sex == "true", 1, 0))

attach(df)
pairs(pairs_df)

#subset selection
regfit.full=regsubsets(sex2~.,data=df, nvmax = 19)
summary(regfit.full)
reg.summary=summary(regfit.full)
names(reg.summary)
plot(reg.summary$cp, xlab="Number of Variables", ylab='cp')
plot(regfit.full, scale="Cp")

#KNN
set.seed(1)
train = sample(nrow(df), nrow(df)/2)
test = df[-train,]

predictors1=cbind(age, test_time, total_updrs, jitter, jitter_rap, jitter_ppq5, shimmer_db, shimmer_apq3, hnr, rpde, dfa, ppe)
knn.pred=class::knn(predictors1[train, ],predictors1[test_knn,],sex2[train],k=1)
table(knn.pred,sex2[test_knn])
mean(knn.pred==sex2[test_knn])

predictors2=cbind(age, test_time, total_updrs, jitter, jitter_rap, jitter_ppq5, shimmer_db, shimmer_apq3, nhr, hnr, rpde, dfa, ppe)
knn.pred=class::knn(predictors2[train, ],predictors2[test_knn,],sex2[train],k=1)
table(knn.pred,sex2[test_knn])
mean(knn.pred==sex2[test_knn])

predictors3=cbind(age, test_time, motor_updrs, total_updrs, jitter, jitter_rap, jitter_ppq5, shimmer_db, shimmer_apq3, hnr, rpde, dfa, ppe)
knn.pred=class::knn(predictors3[train, ],predictors3[test_knn,],sex2[train],k=1)
table(knn.pred,sex2[test_knn])
mean(knn.pred==sex2[test_knn])


#Ridge Regression
# x=model.matrix(sex2~.-1, data=df)
# y=df$sex2
# fit.ridge=glmnet(x,y,alpha=0)
# plot(fit.ridge, xvar="lambda", label = TRUE)
# cv.ridge=cv.glmnet(x,y,alpha=0)
# plot(cv.ridge)

## LOOCV

cv.error=rep(0,6)
degree=1:6
for(d in degree){
  glm.fit=glm(nhr~poly(hnr,d), data=df)
  cv.error[d]=loocv(glm.fit)
}
plot(degree,cv.error,type="b")

## 10-fold CV

cv.error10=rep(0,6)
for(d in degree){
  glm.fit=glm(nhr~poly(hnr,d), data=df)
  cv.error10[d]=cv.glm(df,glm.fit,K=10)$delta[1]
}
lines(degree,cv.error10,type="b",col="red")

## LOOCV

cv.error=rep(0,6)
degree=1:6
for(d in degree){
  glm.fit=glm(nhr~poly(shimmer,d), data=df)
  cv.error[d]=loocv(glm.fit)
}
plot(degree,cv.error,type="b")

## 10-fold CV

cv.error10=rep(0,6)
for(d in degree){
  glm.fit=glm(nhr~poly(shimmer,d), data=df)
  cv.error10[d]=cv.glm(df,glm.fit,K=10)$delta[1]
}
lines(degree,cv.error10,type="b",col="red")

## LOOCV

cv.error=rep(0,6)
degree=1:6
for(d in degree){
  glm.fit=glm(nhr~poly(jitter,d), data=df)
  cv.error[d]=loocv(glm.fit)
}
plot(degree,cv.error,type="b")

## 10-fold CV

cv.error10=rep(0,6)
for(d in degree){
  glm.fit=glm(nhr~poly(jitter,d), data=df)
  cv.error10[d]=cv.glm(df,glm.fit,K=10)$delta[1]
}
lines(degree,cv.error10,type="b",col="red")
