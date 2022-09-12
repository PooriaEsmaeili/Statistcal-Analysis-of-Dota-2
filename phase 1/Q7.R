setwd("/Users/peiman/Desktop/Project phase 1/DOTA2")
mydata <- read.csv("DOTA2.csv")
sample_size <- 300
sampled_data <- mydata[sample(nrow(mydata), sample_size), ]
df <- sampled_data[,c("xp_per_min","Class")]
set <- split( df , f = df$Class )

#1
significance_level <- 0.05
grand_mean <- mean(df$xp_per_min)
grand_mean
residulas <- sampled_data$xp_per_min - mean(df$xp_per_min)
SST <- sum(residulas ^ 2)
SST
mean_groups <- sapply(set, function(x) mean( x$xp_per_min ))
variance_groups <- sapply(set, function(x) var( x$xp_per_min ))
variance_groups
len_groups <- sapply(set, function(x) length(x$xp_per_min))
len_groups
SSG <- sum(((mean_groups - grand_mean)^2) * len_groups)
SSG
SSE <- SST - SSG
SSE
df_T <- sample_size - 1
df_T
df_G <- length(set) - 1
df_G
df_E <- df_T - df_G
df_E
MSG <- SSG / df_G
MSG
MSE <- SSE / df_E
MSE
F_statistic <- MSG / MSE
F_statistic
p_value <- pf(F_statistic, df_G, df_E, lower.tail = FALSE)
p_value
if(p_value < significance_level){
  print("Reject null hypothesis which claims that mean of groups are the same")
}else{
  print("Fail to reject null hypothesis which claims that mean of groups are the same ")
}

#2
significance_level <- 0.05
k <- 3
num_comparisons <- (k * (k - 1) / 2)
num_comparisons
modified_alpha <- significance_level / num_comparisons
modified_alpha
mean_groups <- sapply(set, function(x) mean( x$xp_per_min ))
mean_groups
len_groups <- sapply(set, function(x) length(x$xp_per_min))
len_groups
diff_means <- mean_groups[[1]] - mean_groups[[2]]
diff_means
SE_pairwise <- sqrt((MSE / len_groups[[1]]) + (MSE / len_groups[[2]]))
SE_pairwise
null_value <- 0
T_score <- (diff_means - null_value) / SE_pairwise
T_score
df_total <- df_E
df_total
if(T_score > 0){
  p_value <- 2 * pt(diff_means, df = df_total, lower.tail = FALSE)
}else{
  p_value <- 2 * pt(diff_means, df = df_total, lower.tail = TRUE)
}
p_value
if(p_value < modified_alpha){
  print("Reject null hypothesis")
}else{
  print("Fail to reject null hypothesis")
}