library(ggplot2)
require(dplyr)
require(ISLR)
require(boot)
library(tidyverse)
library(modelr)
require(MASS)
library(leaps)
library(glmnet)
require(data.world)


project <- "https://data.world/tarrantrl/parkinsons-telemonitoring"
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM parkinsons_telemonitoring"),
  dataset = project
)

summary(df)

#Add binary column for gender
df <- df %>% dplyr::mutate(sex2 = ifelse(sex == "true", 1, 0))
#remove outliers
df <- df %>% dplyr::filter(jitter < 0.007, shimmer < 0.035, nhr < 0.032)
#Pairs Chart#
pairs_df <- df %>% dplyr::select(., age, motor_updrs, total_updrs, jitter, shimmer, nhr, hnr, dfa)
pairs(pairs_df)
#sub = sample(nrow(df), 500)
#df <- df[sub,]
plot(nhr~jitter, data = df)
ggplot(df, aes(x=jitter, y=nhr)) + geom_hex(bins = 50)
glm.fit=glm(nhr~jitter, data=df)
#cv.glm(df,glm.fit)$delta 
#pretty slow (doesnt use formula (5.2) on page 180)

##Lets write a simple function to use formula (5.2)
loocv=function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}

## Now we try it out
loocv(glm.fit)

cv.error=rep(0,6)
degree=1:6
for(d in degree){
  glm.fit=glm(nhr~poly(jitter,d), data=df)
  cv.error[d]=loocv(glm.fit)
}
plot(degree,cv.error,type="b")

#K fold
cv.error10=rep(0,6)
for(d in degree){
  glm.fit=glm(nhr~poly(jitter,d), data=df)
  cv.error10[d]=cv.glm(df,glm.fit,K=10)$delta[1]
}
lines(degree,cv.error10,type="b",col="red")


# bootstrap
?boot
alpha=function(x,y){
  vx=var(x)
  vy=var(y)
  cxy=cov(x,y)
  (vy-cxy)/(vx+vy-2*cxy)
}

## What is the standard error of alpha?

alpha.fn=function(data, index){
  with(data[index,],alpha(jitter,nhr))
}

set.seed(1)
alpha.fn (df,sample(1:300,300,replace=TRUE))
boot.out=boot(df,alpha.fn,R=2000)
boot.out
plot(boot.out)

lin.statistic <- function(data, index) {
  lm.fit <- lm(nhr ~ jitter, data = data, subset = index)
  coef(lm.fit)
}
set.seed(1)
boot(df, lin.statistic, 4000)
summary(lm(nhr ~ jitter, data = df))
#b = boot(df, quad.statistic, 4000)
#boot.ci(b)
#plot(b)
quad.statistic <- function(data, index) {
  lm.fit <- lm(nhr ~ poly(jitter, 2), data = data, subset = index)
  coef(lm.fit)
}
boot(df, quad.statistic, 4000)
summary(lm(nhr ~ poly(jitter, 2), data = df))
#b2 = boot(df, quad.statistic, 4000)
#boot.ci(b2)
plot(b2)



df <- data.world::query(
  data.world::qry_sql("SELECT * FROM parkinsons_telemonitoring"),
  dataset = project
)
#Add binary column for gender
df <- df %>% dplyr::mutate(sex2 = ifelse(sex == "true", 1, 0))

#forward selection
##################
#set.seed(1)

df_subset_fixed <- df %>% dplyr::select(., -subject, -sex)

regfit.fwd=regsubsets(sex2~.,data=df_subset_fixed,nvmax=22,method="forward")
summary(regfit.fwd)

plot(regfit.fwd,scale="Cp")
plot(regfit.fwd,scale="adjr2")

regfwd.summary=summary(regfit.fwd)

which.min(regfwd.summary$cp)
which.max(regfwd.summary$adjr2)

plot(regfwd.summary$cp,xlab="Number of Variables",ylab="Cp")
plot(regfwd.summary$adjr2,xlab="Number of Variables",ylab="adjr2")

#forward selection without shimmer predictors except shimmer
##################
#set.seed(1)

df_subset_fixed2 <- df %>% dplyr::select(., -subject, -sex, -shimmer_db, -shimmer_apq3, -shimmer_apq5, -shimmer_apq11, -shimmer_dda)

regfit.fwd=regsubsets(sex2~.,data=df_subset_fixed2,nvmax=22,method="forward")
summary(regfit.fwd)

plot(regfit.fwd,scale="Cp")
plot(regfit.fwd,scale="adjr2")

regfwd.summary=summary(regfit.fwd)

which.min(regfwd.summary$cp)
which.max(regfwd.summary$adjr2)

plot(regfwd.summary$cp,xlab="Number of Variables",ylab="Cp")
plot(regfwd.summary$adjr2,xlab="Number of Variables",ylab="adjr2")
#backward selection
##################
df_subset_fixed <- df %>% dplyr::select(., -subject, -sex)

regfit.bw=regsubsets(sex2~.,data=df_subset_fixed,nvmax=22,method="backward")
regfit.bw
summary(regfit.bw)

plot(regfit.bw,scale="Cp")
plot(regfit.bw,scale="adjr2")

regbw.summary=summary(regfit.bw)

which.min(regbw.summary$cp)
which.max(regbw.summary$adjr2)

plot(regbw.summary$cp,xlab="Number of Variables",ylab="Cp")
plot(regbw.summary$adjr2,xlab="Number of Variables",ylab="adjr2")




