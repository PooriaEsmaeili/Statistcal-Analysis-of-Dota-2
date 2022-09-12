setwd("/Users/peiman/Desktop/Project phase 2/DOTA2")
mydata <- read.csv("DOTA2.csv")
sample_size <- 1000
sample <- mydata[sample(nrow(mydata), sample_size),]

##Backward Elimination
step0_model <- lm(gold_per_min ~ xp_per_min + hero_damage + kills + duration + hero_id , sample)
summary (step0_model)
step1_model <- lm(gold_per_min ~ xp_per_min + hero_damage + kills + duration , sample)
summary (step1_model)
step2_model <- lm(gold_per_min ~ xp_per_min + kills + duration , sample)
summary (step2_model)

## Forward Selection
#step1
step1_model1 <- lm(dribbling ~ age , sample)
summary (step1_model1)
step1_model2 <- lm(dribbling ~ preferred_foot , sample)
summary (step1_model2)
step1_model3 <- lm(dribbling ~ acceleration , sample)
summary (step1_model3)
step1_model4 <- lm(dribbling ~ body_type , sample)
summary (step1_model4)
step1_model5 <- lm(dribbling ~ height_cm , sample)
summary (step1_model5)