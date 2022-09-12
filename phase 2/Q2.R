setwd("/Users/peiman/Desktop/Project phase 2/DOTA2")
mydata <- read.csv("DOTA2.csv")
sample_size <- 500
sample <- mydata[sample(nrow(mydata), sample_size)  , ]
tb <- table(sample[,c("WL", "Class")])
tb
n_loss <- sum(tb["Loss",])
p_hat_loss <- tb["Loss", "AGI"]/n_loss
n_win <- sum(tb["Win",])
p_hat_win <- tb["Win", "AGI"]/n_win
CL = 0.95
SE <- sqrt (p_hat_loss * (1 - p_hat_loss)/n_loss + p_hat_win * (1 - p_hat_win)/n_win)
z_value <- abs(qnorm((1 - CL)/2))
CI <- c((p_hat_loss - p_hat_win) - z_value * SE , (p_hat_loss - p_hat_win) + z_value * SE)
CI
chisq.test(tb)
