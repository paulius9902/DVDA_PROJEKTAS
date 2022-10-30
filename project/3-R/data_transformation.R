install.packages("tidyverse")

library(tidyverse)

data <- read_csv("../1-data/1-sample_data.csv")

select(data, y)

data_additional <- read_csv("../1-data/3-additional_features.csv")

joined_data <- inner_join(data, data_additional, by = "id")


data_full <- data %>%
  inner_join(data_additional, by = "id")


write_csv(data_full, "../1-data/train_data.csv") 