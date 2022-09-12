setwd("/Users/peiman/Desktop/Project phase 2/DOTA2")
mydata <- read.csv("DOTA2.csv")
sample_size <- 150
sample <- mydata[sample(nrow(mydata), sample_size)  , ]
gold_per_mean_Win <- sample[which(sample$WL=="Win"),]$gold_per_min
gold_per_mean_Loss <- sample[which(sample$WL=="Loss"),]$gold_per_min
sample_mean_Win <- mean(gold_per_mean_Win)
sample_mean_Loss <- mean(gold_per_mean_Loss)
SE = sqrt(
  var(gold_per_mean_Win)/length(gold_per_mean_Win) + 
    var(gold_per_mean_Loss)/length(gold_per_mean_Loss)
)
Z <- (sample_mean_Win - sample_mean_Loss)/SE
2 * pnorm(Z , lower.tail = FALSE)
