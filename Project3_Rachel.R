library(tidyverse)
library(modelr)
require(dplyr)
require(data.world)
require(MASS)
require(ISLR)
require(ggplot2)
library(leaps)

data.world::set_config(save_config(auth_token = "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50OnRhcnJhbnRybCIsImlzcyI6ImFnZW50OnRhcnJhbnRybDo6MDE1OTQxYzQtNTUyZC00YjI3LWIxNGEtYzllN2ExMjYxN2FiIiwiaWF0IjoxNTA1MzEzMjAyLCJyb2xlIjpbInVzZXJfYXBpX3dyaXRlIiwidXNlcl9hcGlfcmVhZCJdLCJnZW5lcmFsLXB1cnBvc2UiOnRydWV9.vWrAbNkyU0mhgsdXXL-bxESWzppmpm8wguw9uI7pJ64ZsDtovi8kbWbPYS5pPcX8DDnVMuYxJJWHhqdxv--R_w"))
#vignette("quickstart", package = "data.world")

project <- "https://data.world/tarrantrl/parkinsons-telemonitoring"
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM parkinsons_telemonitoring"),
  dataset = project
)

project <- "https://data.world/marcusgabe-ut/parkinsons-data"
df_b <- data.world::query(
  data.world::qry_sql("SELECT * FROM parkinsons"),
  dataset = project
)

summary(df)
attach(df)
head(df)

dftotal = dplyr::select(df,-subject, -motor_updrs)
head(dftotal)
dfmotor = dplyr::select(df, -subject, -total_updrs)

## Forward selection
regfit.fwd = regsubsets(total_updrs~., data=dftotal, nvmax=19, method="forward")
regfit.summary = summary(regfit.fwd)
regfit.summary
plot(regfit.fwd,scale="Cp")
plot(regfit.summary$cp,xlab="Number of Variables",ylab="Cp", type="b")
which.min(regfit.summary$cp)
plot(regfit.fwd,scale="adjr2")
plot(regfit.summary$adjr2,xlab="Number of Variables",ylab="Adjr2", type="b")
which.max(regfit.summary$adjr2)

regfit.time = regsubsets(test_time~., data=df, nvmax=21, method="forward")
time.sum = summary(regfit.time)
time.sum
plot(regfit.time,scale="Cp")
plot(time.sum$cp,xlab="Number of Variables", ylab="Cp", type="b")
which.min(time.sum$cp)
plot(regfit.time,scale="adjr2")
plot(time.sum$adjr2,xlab="Number of Variables", ylab="Adjr2", type="b")
which.max(time.sum$adjr2)

## Lasso
library(glmnet)
# for this you have to pass in a matrix of x. You have to construct the x's
x=model.matrix(sex~.-1,data=df) 
# and construct the y's
y=df$sex
?glmnet
fit.lasso=glmnet(x,y,family="binomial")
# plot how lambda is changing against the model fit
plot(fit.lasso,xvar="lambda",label=TRUE) # the one that disappears last is our best predictor
# do cross validation for each lambda (default is kfold of 10 across 100 lambdas)
cv.lasso=cv.glmnet(x,y,family="binomial")
# plot the cross validation mean squared errors of all 100 models
plot(cv.lasso)
# get the coefficients for what it thinks is the best model
coef(cv.lasso)

## predicting the previous dataset
head(df_b)
# predicting status
df_b1 = dplyr::select(df_b,-name)
df_b1 = df_b1 %>% dplyr::mutate(status2 = ifelse(status == "true", 1, 0))
x=model.matrix(status~.-1,data=df_b1)
y=df_b1$status
fit.lasso=glmnet(x,y,family="binomial")
plot(fit.lasso,xvar="lambda",label=TRUE)
cv.lasso=cv.glmnet(x,y,family="binomial")
plot(cv.lasso)
coef(cv.lasso)

# with binary status
df_b1 = dplyr::select(df_b1,-status)
x=model.matrix(status2~.-1,data=df_b1)
y=df_b1$status2
fit.lasso=glmnet(x,y,family="binomial")
plot(fit.lasso,xvar="lambda",label=TRUE)
cv.lasso=cv.glmnet(x,y,family="binomial")
plot(cv.lasso)
coef(cv.lasso)
