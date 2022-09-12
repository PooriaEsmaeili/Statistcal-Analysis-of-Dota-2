setwd("/Users/peiman/Desktop/Project phase 1/DOTA2")
mydata <- read.csv("DOTA2.csv") 
df <- data.frame(mydata[,c("WL", "Class")])
#1
library(gmodels)
CrossTable(df$WL, df$Class, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)
#2
counts <- table(df$WL, df$Class)
barplot(counts, main="Segmented bar plot",
        xlab="Class", col=c("darkblue","red"),
        legend = rownames(counts))
#3
barplot(counts, main="Side by side bar charts",
        xlab="Class", col=c("darkblue","red"),
        legend = rownames(counts), beside = TRUE)
#4
mosaicplot(counts, color = TRUE, shade = TRUE, main = "Mosaic plot")
