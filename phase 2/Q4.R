setwd("/Users/peiman/Desktop/Project phase 2/DOTA2")
mydata <- read.csv("DOTA2.csv")
Class_attr <- mydata$Class
sample_size <- 500
unbiased <- sample(Class_attr , sample_size , replace = FALSE)
prb <- ifelse(Class_attr == "AGI", 0.5, ifelse(Class_attr == "INT", 0.25, 0.25))
biased <- sample(Class_attr, sample_size, prob = prb)
original_prob <- c(prop.table(table(Class_attr)))
table(unbiased)
table(biased)
chisq.test(x = table(unbiased) ,p = original_prob)
chisq.test(x = table(biased) ,p = original_prob)
sample <- mydata[sample(nrow(mydata), sample_size)  , ]
tb <- table(sample[,c("radiant_win", "Class")])
tb
chisq.test(tb)
