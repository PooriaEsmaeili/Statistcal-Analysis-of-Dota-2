setwd("/Users/peiman/Desktop/Project phase 1/DOTA2")
mydata <- read.csv("DOTA2.csv") 
sample_size <- 200
sampled_data <- mydata[sample(nrow(mydata), sample_size), ]

#1
alpha <- 1 - 0.98
alpha
z <- qnorm(alpha / 2)
z
x_bar <- mean(sampled_data$xp_per_min)
x_bar
standard_devitation <- sd(sampled_data$xp_per_min)
standard_devitation
standard_error <- standard_devitation / sqrt(sample_size)
standard_error
lower_boundry_of_confidence_interval <- x_bar + z * standard_error
upper_boundry_of_confidence_interval <- x_bar - z * standard_error
CI <- c(lower_boundry_of_confidence_interval, upper_boundry_of_confidence_interval)
CI

#3
null_value <- 452
z_score <- (x_bar - null_value) / standard_devitation
if(z_score > 0){
  p_value <- 2 * pnorm(z_score, lower.tail = FALSE)
}else{
  p_value <- 2 * pnorm(z_score)
}
if(p_value < 0.05){
  print("reject null hypothesis")
}else{
  print("fail to reject null hypothesis")
}

#4
mu <- mean(mydata$xp_per_min)
mu
alpha <- 0.05
alpha
z <- qnorm(alpha / 2)
z
upper_bound <- x_bar - z * standard_error
lower_bound <- x_bar + z * standard_error
CI <- c(lower_bound, upper_bound)
CI
p <- pnorm(CI, mean = mu, sd = standard_error)
betta <- p[2] - p[1]
betta

#5
z_score <- (CI[1] - x_bar) / standard_error
power <- pnorm(z_score)
power
betta <- (1 - power)
betta
