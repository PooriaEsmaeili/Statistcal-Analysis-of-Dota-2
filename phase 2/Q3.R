setwd("/Users/peiman/Desktop/Project phase 2/DOTA2")
mydata <- read.csv("DOTA2.csv")
sample_size <- 15
WL_small_sample <- mydata[sample(nrow(mydata), sample_size), ]$WL
p_hat <- length(which(WL_small_sample == 'Win' ))/sample_size
p_hat

WL_levels = c ("Win" , "Loss")
sim <- t(replicate(n = 1000, sample(WL_levels, size = sample_size, replace = TRUE)))
sim <- data.frame(sim)
sim_success <- apply(sim, 1, function(x) length(which(x == "Win")))
p_value <- length(which(sim_success >= 11))/1000
p_value
hist(sim_success/sample_size)
