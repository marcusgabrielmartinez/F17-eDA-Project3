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


### stef
library(leaps)
regfit.full=regsubsets(Salary~.,data=Hitters)
summary(regfit.full)

regfit.full=regsubsets(Salary~.,data=Hitters, nvmax=19)
reg.summary=summary(regfit.full)
names(reg.summary)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp") #uses cp from the summary to compare the models and see which is best
#plot(reg.summary$bic,xlab="Number of Variables",ylab="Bic")
#plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjr2")
which.min(reg.summary$cp) #cp is the statistic you want to minimize
points(10,reg.summary$cp[10],pch=20,col="red")

plot(regfit.full,scale="Cp")
coef(regfit.full,10)
