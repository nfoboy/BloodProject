library(tidyverse)
library(fpp)

bloodFromDownload <- as.tibble(read.csv("trainingDataForBlood.csv", stringsAsFactors = FALSE))
#create our own test and training sets
bloodFromDownload %>% select(-X) -> bloodFromDownload

## 70% of the sample size
smp_size <- floor(0.7 * nrow(bloodFromDownload))
set.seed(20264)
train_ind <- sample(seq_len(nrow(bloodFromDownload)), size = smp_size)

bloodTrain <- bloodFromDownload[train_ind, ]
bloodTest <- bloodFromDownload[-train_ind, ]

bloodFinalTestForCompetition <- as.tibble(read.csv("testDataForBlood.csv", stringsAsFactors = FALSE))


## set the seed to make your partition reproductible
myData = c(0, 0)
i = 1
while (i < 5000){
  set.seed(i)
  train_ind <- sample(seq_len(nrow(bloodFromDownload)), size = smp_size)
  
  bloodTrain <- bloodFromDownload[train_ind, ]
  bloodTest <- bloodFromDownload[-train_ind, ]
  bloodTrain %>% select(-Total.Volume.Donated..c.c..) -> bloodTrain
  model <- lm(Made.Donation.in.March.2007~ ., data = bloodTrain)
  pred.blood <- predict(model, newdata = bloodTest)
  result <- cor(round(pred.blood), bloodTest$Made.Donation.in.March.2007) 
  myData <-rbind(myData, c(result, i))
  i = i+1
  if(i%%500  == 0){
    print(i)
  }
}


blood.formula <- Made.Donation.in.March.2007 ~ 
    Number.of.Donations + Months.since.Last.Donation + Months.since.First.Donation
blood.logModel <- glm(formula = blood.formula, data = bloodTrain, family = "binomial") 
blood.prob <- predict(blood.logModel, newdata = bloodTest, type = 'response')
cor(round(blood.prob), bloodTest$Made.Donation.in.March.2007)

myData = c(0, 0)
i = 1
blood.formula <- Made.Donation.in.March.2007 ~ 
  Number.of.Donations + Months.since.Last.Donation + Months.since.First.Donation
while (i < 5000){
  set.seed(i)
  train_ind <- sample(seq_len(nrow(bloodFromDownload)), size = smp_size)
  
  bloodTrain <- bloodFromDownload[train_ind, ]
  bloodTest <- bloodFromDownload[-train_ind, ]
  
  blood.logModel <- glm(formula = blood.formula, data = bloodTrain, family = "binomial") 
  blood.prob <- predict(blood.logModel, newdata = bloodTest, type = 'response')
  result <- cor(round(blood.prob), bloodTest$Made.Donation.in.March.2007)
  myData <-rbind(myData, c(result, i))
  i = i+1
  if(i%%500  == 0){
    print(i)
  }
}

cv.fit <- cv.glmnet(as.matrix(prestige_train[,c(-4, -6)]), as.vector(prestige_train[,4]), nlambda = 100, alpha = 0.7, family = ”gaussian”)
