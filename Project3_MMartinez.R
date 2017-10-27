library(tidyverse)
library(modelr)
require(dplyr)
require(data.world)
require(MASS)
require(ISLR)
require(ggplot2)
library(leaps)
library(glmnet)

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

attach(df)

#Pairs Chart#
pairs_df <- df %>% dplyr::select(., age, motor_updrs, total_updrs, jitter, shimmer, nhr, hnr, dfa)

pairs(pairs_df)

##KNN Section##

#KNN Test Data#
set.seed(1)
train = sample(nrow(df), 2937)
test = df[-train,]

test_knn = sample(nrow(test), 2937)

#KNN Analysis Build#
predictorsKNN=cbind(jitter, nhr)
knn.pred=class::knn(predictorsKNN[train, ],predictorsKNN[test_knn,],motor_updrs[train],k=1)
table(knn.pred,motor_updrs[test_knn])
mean(knn.pred==motor_updrs[test_knn])

predictorsKNN2=cbind(jitter, hnr)
knn2.pred=class::knn(predictorsKNN2[train, ],predictorsKNN2[test_knn,],motor_updrs[train],k=1)
table(knn2.pred,motor_updrs[test_knn])
mean(knn2.pred==motor_updrs[test_knn])

predictorsKNN3=cbind(age, hnr, dfa)
knn3.pred=class::knn(predictorsKNN3[train, ],predictorsKNN3[test_knn,],motor_updrs[train],k=1)
table(knn3.pred,motor_updrs[test_knn])
mean(knn3.pred==motor_updrs[test_knn])

predictorsKNN4=cbind(age, hnr, dfa)
knn4.pred=class::knn(predictorsKNN4[train, ],predictorsKNN4[test_knn,],sex2[train],k=1)
table(knn4.pred,sex2[test_knn])
mean(knn4.pred==sex2[test_knn])

predictorsKNN5=cbind(age, rpde, ppe, total_updrs)
knn5.pred=class::knn(predictorsKNN5[train, ],predictorsKNN5[test_knn,],sex2[train],k=1)
table(knn5.pred,sex2[test_knn])
mean(knn5.pred==sex2[test_knn])

predictorsKNN6=cbind(jitter_abs, jitter_rap)
knn6.pred=class::knn(predictorsKNN6[train, ],predictorsKNN6[test_knn,],sex2[train],k=1)
table(knn6.pred,sex2[test_knn])
mean(knn6.pred==sex2[test_knn])

predictorsKNN7=cbind(age, motor_updrs, total_updrs, jitter, jitter_abs, jitter_ppq5, rpde, dfa, ppe)
knn7.pred=class::knn(predictorsKNN7[train, ],predictorsKNN7[test_knn,],sex2[train],k=1)
table(knn7.pred,sex2[test_knn])
mean(knn7.pred==sex2[test_knn])

predictorsKNN8=cbind(motor_updrs, total_updrs, jitter_abs, jitter_rap, jitter_ppq5, shimmer_apq11, nhr, hnr, rpde, ppe)
knn8.pred=class::knn(predictorsKNN8[train, ],predictorsKNN8[test_knn,],sex2[train],k=1)
table(knn8.pred,sex2[test_knn])
mean(knn8.pred==sex2[test_knn])

predictorsKNN9=cbind(motor_updrs, total_updrs, jitter_abs, jitter_rap, jitter_ppq5, shimmer_apq11, nhr, hnr, rpde, ppe)
knn9.pred=class::knn(predictorsKNN9[train, ],predictorsKNN9[test_knn,],sex2[train],k=10)
table(knn9.pred,sex2[test_knn])
mean(knn9.pred==sex2[test_knn])


# ##Lasso Section##- Not currently working
# 
# # Builds a new dataframe that excludes a column that should not be included as a predictor.
# df_subset <- df %>% dplyr::select(., -subject)
#  
# x=model.matrix(sex2~.-1,data=df_subset) 
# y=df_subset$sex2
#  
# fit.lasso=glmnet(x,y)
# plot(fit.lasso,xvar="lambda",label=TRUE)
# cv.lasso=cv.glmnet(x,y)
# plot(cv.lasso)
# coef(cv.lasso)
#  
# lasso.tr=glmnet(x[train,],y[train])
# lasso.tr
# pred=predict(lasso.tr,x[-train,])
# dim(pred)
# rmse= sqrt(apply((y[-train]-pred)^2,2,mean))
# plot(log(lasso.tr$lambda),rmse,type="b",xlab="Log(lambda)")
# lam.best=lasso.tr$lambda[order(rmse)[1]]
# lam.best
# coef(lasso.tr,s=lam.best)

##Best Subset Section##

# Builds a new dataframe that excludes a column that should not be included as a predictor.
df_subset <- df %>% dplyr::select(., -subject)

regfit.full=regsubsets(sex2~.,data=df_subset, nvmax=22)
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

#Same Best Subset Code with FIXED Dataframe#
df_subset_fixed <- df %>% dplyr::select(., -subject, -sex)

regfit.full=regsubsets(sex2~.,data=df_subset_fixed, nvmax=22)
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

# Builds a new dataframe that excludes a column that should not be included as a predictor.
df_subset <- df %>% dplyr::select(., -subject)

regfit.fwd=regsubsets(sex2~.,data=df_subset,nvmax=22,method="forward")
summary(regfit.fwd)

plot(regfit.fwd,scale="Cp")
plot(regfit.fwd,scale="adjr2")

regfwd.summary=summary(regfit.fwd)

which.min(regfwd.summary$cp)
which.min(regfwd.summary$adjr2)

plot(regfwd.summary$cp,xlab="Number of Variables",ylab="Cp")
plot(regfwd.summary$adjr2,xlab="Number of Variables",ylab="adjr2")

#Same Forward Code with FIXED Dataframe#
df_subset_fixed <- df %>% dplyr::select(., -subject, -sex)

regfit.fwd=regsubsets(sex2~.,data=df_subset_fixed,nvmax=22,method="forward")
summary(regfit.fwd)

plot(regfit.fwd,scale="Cp")
plot(regfit.fwd,scale="adjr2")

regfwd.summary=summary(regfit.fwd)

which.min(regfwd.summary$cp)
which.min(regfwd.summary$adjr2)

plot(regfwd.summary$cp,xlab="Number of Variables",ylab="Cp")
plot(regfwd.summary$adjr2,xlab="Number of Variables",ylab="adjr2")

# ##Validation Set Section## - Not working!
# 
# # Builds a new dataframe that excludes a column that should not be included as a predictor.
# df_subset <- df %>% dplyr::select(., -subject)
# 
# dim(df_subset)
# set.seed(1)
# train=sample(seq(263),180,replace=FALSE)
# train
# regfit.fwd=regsubsets(sex2~.,data=df_subset[train,],nvmax=20,method="forward")
# 
# 
# val.errors=rep(NA,20)
# x.test=model.matrix(sex2~.,data=df_subset[-train,])# notice the -index!
# for(i in 1:20){
#   coefi=coef(regfit.fwd,id=i)
#   pred=x.test[,names(coefi)]%*%coefi
#   val.errors[i]=mean((df_subset$sex2[-train]-pred)^2)
# }
# plot(sqrt(val.errors),ylab="Root MSE",ylim=c(300,400),pch=19,type="b")
# points(sqrt(regfit.fwd$rss[-1]/180),col="blue",pch=19,type="b")
# legend("topright",legend=c("Training","Validation"),col=c("blue","black"),pch=19)

##Cross Validation Section##

# Builds a new dataframe that excludes a column that should not be included as a predictor.
df_subset <- df %>% dplyr::select(., -subject)

predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  mat[,names(coefi)]%*%coefi
}

set.seed(11)
folds=sample(rep(1:10,length=nrow(df_subset)))
folds
table(folds)
cv.errors=matrix(NA,10,20)
for(k in 1:10){
  best.fit=regsubsets(sex2~.,data=df_subset[folds!=k,],nvmax=20,method="forward")
  for(i in 1:20){
    pred=predict(best.fit,df_subset[folds==k,],id=i)
    cv.errors[k,i]=mean( (df_subset$sex2[folds==k]-pred)^2)
  }
}
rmse.cv=sqrt(apply(cv.errors,2,mean))
plot(rmse.cv,pch=19,type="b")





