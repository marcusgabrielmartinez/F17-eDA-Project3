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

# ##Lasso Section##- Not currently working
# 
# library(glmnet)
# x=model.matrix(sex2~.-1,data=df) 
# y=df$sex2
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
points(10,reg.summary$cp[10],pch=20,col="red")
summary(regfit.full)

plot(regfit.full,scale="Cp")
plot(regfit.full,scale="adjr2")
coef(regfit.full,10)

