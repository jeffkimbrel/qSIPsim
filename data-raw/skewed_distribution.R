# Makes a consistent skewed (left heavy) distribution used in various functions

set.seed(123)
skewed_distribution <- sample(1:10, size = 100, replace = TRUE, prob = 10:1) * 0.01

usethis::use_data(skewed_distribution, overwrite = TRUE)
