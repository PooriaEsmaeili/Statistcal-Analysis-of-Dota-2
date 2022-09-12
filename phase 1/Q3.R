setwd("/Users/peiman/Desktop/Project phase 1/DOTA2")
mydata <- read.csv("DOTA2.csv") 
counts <- table(mydata[["Class"]])
#1
barplot(counts, main="Class")
#2
data <- data.frame(counts)
data2  <- data[order(data[,2],decreasing=TRUE),]
barplot(data2$Freq, names.arg=data2[,1], horiz = TRUE)
#3
counts

