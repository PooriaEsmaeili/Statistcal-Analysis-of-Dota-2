library(ggplot2)
setwd("/Users/peiman/Desktop/Project phase 1/DOTA2")
mydata <- read.csv("DOTA2.csv") 
#1
df <- do.call(rbind, Map(data.frame, xp_per_min=mydata[["xp_per_min"]], gold_per_min=mydata[["gold_per_min"]]))
ggplot(df, aes(gold_per_min, xp_per_min)) + geom_point(colour = "blue", size = 0.5)
#2
correlation_coefficient <- cor(df$gold_per_min, df$xp_per_min)
correlation_coefficient
#3
ggplot(df, aes(gold_per_min, xp_per_min)) + 
  geom_point(colour = "blue", size = 0.5) +
  geom_smooth(color = "black", se = FALSE)
ggplot(df, aes(gold_per_min, xp_per_min)) + 
  geom_point(colour = "blue", size = 0.5) +
  geom_smooth(color = "black", method = lm, se = FALSE)
#4
df2 <- do.call(rbind, Map(data.frame, hero_damage=mydata[["hero_damage"]], gold_per_min=mydata[["gold_per_min"]]))
ggplot(df2, aes(hero_damage, gold_per_min)) + geom_point(colour = "blue", size = 0.5)
correlation_coefficient2 <- cor(df2$hero_damage, df2$gold_per_min)
correlation_coefficient2
ggplot(df2, aes(hero_damage, gold_per_min)) + 
  geom_point(colour = "blue", size = 0.5) +
  geom_smooth(color = "black", se = FALSE)
ggplot(df2, aes(hero_damage, gold_per_min)) + 
  geom_point(colour = "blue", size = 0.5) +
  geom_smooth(color = "black", method = lm, se = FALSE)

df3 <- do.call(rbind, Map(data.frame, hero_damage=mydata[["hero_damage"]], xp_per_min=mydata[["xp_per_min"]]))
ggplot(df3, aes(hero_damage, xp_per_min)) + geom_point(colour = "blue", size = 0.5)
correlation_coefficient3 <- cor(df3$hero_damage, df3$xp_per_min)
correlation_coefficient3
ggplot(df3, aes(hero_damage, xp_per_min)) + 
  geom_point(colour = "blue", size = 0.5) +
  geom_smooth(color = "black", se = FALSE)
ggplot(df3, aes(hero_damage, xp_per_min)) + 
  geom_point(colour = "blue", size = 0.5) +
  geom_smooth(color = "black", method = lm, se = FALSE)
#5
library(hexbin)
hexbinplot(xp_per_min ~ gold_per_min, df, xbin = 20, type = "r")
ggplot(df, aes(x=xp_per_min,y=gold_per_min)) +
  stat_binhex(bins=300, color=c("#D7DADB")) +
  theme_classic(base_size=18) +
  labs(x = "xp per min", y = "gold per min")+
  scale_fill_gradient(low = "grey", high = "red")
#6
my_variables <- mydata[,c(17, 18, 19, 21, 25, 26)]
cormat <- round(cor(my_variables),2)
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
upper_tri
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
ggheatmap <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

