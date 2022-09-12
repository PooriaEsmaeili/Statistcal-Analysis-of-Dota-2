library(ggplot2)
dataset <- diamonds
#1
sampled_data <- dataset[sample(nrow(dataset), 200), ]
price_per_carat <-sampled_data$price / sampled_data$carat
df <- data.frame("price_per_carat" = price_per_carat, "diamond_color" = sampled_data$color)
p <- ggplot(df, aes(diamond_color, price_per_carat)) 
p + geom_boxplot()
p + geom_jitter(width = 0.2)
p + geom_boxplot() + geom_jitter(width = 0.2)
#4
df2 <- data.frame("price" = (sampled_data$price), "carat" = (sampled_data$carat) )
qplot(x = price, y = carat, data = df2, xlab = "price", ylab = "carat", main = "Without any transformation")
df2 <- data.frame("price" = log(sampled_data$price), "carat" = log(sampled_data$carat) )
qplot(x = price, y = carat, data = df2, xlab = "Log of carat", ylab = "Log of price", main = "Log transformation plot")

