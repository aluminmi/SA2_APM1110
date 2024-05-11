library(dplyr)
library(readr)

eth_data <- read_csv("C:/Users/Dindette/Downloads/ETH-USD.csv", col_types = cols())

eth_data <- eth_data %>%
  mutate(Return = (Close - lag(Close)) / lag(Close))

shapiro_test_result <- shapiro.test(eth_data$Return)

print(shapiro_test_result)

# While the W value is not far from 1, the p-value indicates that the data is unlikely to be normally distributed according to the Shapiro-Wilk test