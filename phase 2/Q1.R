setwd("/Users/peiman/Desktop/Project phase 2/DOTA2")
mydata <- read.csv("DOTA2.csv")
sample_size <- 60
sample <- mydata[sample(nrow(mydata), sample_size)  , ]
PF_sample <- sample$WL
p_hat <- length(which(PF_sample == 'Win' ))/sample_size
p_hat
p <- 0.5
SE = sqrt (p*(1-p)/sample_size)
Z_score <- (p_hat-p)/SE
p_value <- 2 * pnorm(Z_score, lower.tail = FALSE)
p_value
effect_size <- p_hat - p
effect_size