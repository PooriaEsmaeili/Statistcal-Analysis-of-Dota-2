library(ggplot2)
library(moments)
#setwd("/Users/peiman/Desktop/Project phase 1/DOTA2")
mydata <- read.csv("DOTA2.csv") 
gold <- mydata[["gold"]]
#1
qplot(gold,
      geom="histogram",
      binwidth=1500,  
      main="Histogram for gold", 
      xlab="gold",  
      fill=I("blue"), 
      col=I("black"), 
      alpha=I(.2))
#2
x <- as.data.frame(gold)
fill <- "#4271AE"
line <- "#1F3552"
ggplot(x, aes(x = gold)) +
  geom_density(fill = fill, colour = line,
               alpha = 0.6) +
  scale_x_continuous(name = "gold") +
  scale_y_continuous(name = "Density") +
  ggtitle("Density plot of gold variable")
#4
mean <- mean(gold)
variance <- var(gold)
standard_deviation <- sd(gold)
skewness <- skewness(gold)
mean
variance
standard_deviation
skewness
#5
boxplot(gold)
quantiles <- quantile(gold)
upper_quartile <- quantiles[4]
lower_quartile <- quantiles[2]
IQR <- IQR(gold)
lower_inner_fence <- max(c(min(gold), lower_quartile - 1.5 * IQR))
lower_outer_fence <- max(c(min(gold), lower_quartile - 3 * IQR))
upper_inner_fence <- min(c(max(gold), upper_quartile + 1.5 * IQR))
upper_outer_fence <- min(c(max(gold), upper_quartile + 3 * IQR))
upper_quartile
lower_quartile
IQR
lower_inner_fence
lower_outer_fence
upper_inner_fence
upper_outer_fence
#6
OutVals = boxplot(gold)$out
boxplot(gold, col="grey", outcol="red")
