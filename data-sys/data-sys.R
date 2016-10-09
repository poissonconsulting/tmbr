library(devtools)

rm(list = ls())

set.seed(123)
data_set_example1 <- data.frame(x = stats::runif(1000, 1, 10))
data_set_example1$y = stats::rnorm(20, mean = 1.8 + 2.4 * data_set_example1$x, sd = exp(0.3))

data_set_example2 <- data_set_example1
data_set_example2$Year <- factor(c("first", "second", "third", "4th", "5th", "6th", "7", "eight", "nine", "10"))
data_set_example2$y <- data_set_example2$y + as.integer(data_set_example2$Year) * 4
use_data(data_set_example1, data_set_example2, internal = TRUE, overwrite = TRUE)
