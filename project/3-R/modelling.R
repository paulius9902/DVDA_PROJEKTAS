library(h2o)
library(tidyverse)
h2o.init()

df <- h2o.importFile("../1-data/train_data.csv")
df
class(df)
summary(df)

y <- "y"
x <- setdiff(names(df), c(y, "id"))
df$y <- as.factor(df$y)
summary(df)

aml <- h2o.automl(x = x,
                  y = y,
                  training_frame = df,
                  max_runtime_secs = 60)

aml@leaderboard

predictions <- h2o.predict(aml@leader, df)

predictions %>%
  as_tibble() %>%
  write_csv("../5-predictions/predictions_test.csv")

### ID, Y

h2o.saveModel(aml@leader, "../4-model/aml_leader")

# model <- h2o.loadModel("4-model/aml_leader/GLM_1_AutoML_2_20211023_171408")

h2o.shutdown()