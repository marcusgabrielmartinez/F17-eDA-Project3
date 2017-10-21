library(tidyverse)
library(modelr)
require(dplyr)
require(data.world)
require(MASS)
require(ISLR)
require(ggplot2)

data.world::set_config(save_config(auth_token = "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50Om1hcmN1c2dhYmUtdXQiLCJpc3MiOiJhZ2VudDptYXJjdXNnYWJlLXV0OjowYjE2NDQzOC1mYzRlLTRhNDktYWY1MC1iMWU1YjViYmIzYzMiLCJpYXQiOjE0ODQ4NjgyNjMsInJvbGUiOlsidXNlcl9hcGlfcmVhZCIsInVzZXJfYXBpX3dyaXRlIl0sImdlbmVyYWwtcHVycG9zZSI6dHJ1ZX0.Eb9i31mYAv6zQGjlze-PbiBJ_5_JNBDIZn51wcPnnNPny_ih2SSN9Ur_LVyRltEbrReNXM5b371XWrmMiexEKw"))
#vignette("quickstart", package = "data.world")

project <- "https://data.world/tarrantrl/parkinsons-telemonitoring"
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM parkinsons_telemonitoring"),
  dataset = project
)

summary(df)
attach(df)

#Pairs Chart#
pairs_df <- df %>% dplyr::select(., motor_updrs, total_updrs, jitter, shimmer, nhr, hnr)

pairs(pairs_df)

##KNN Section##

#KNN Test Data#
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
