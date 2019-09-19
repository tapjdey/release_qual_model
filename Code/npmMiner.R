setwd("~/Work/release_qual/model/")
library(psych)
data = read.csv('npmMiner.csv')
data$Package = NULL
data$quality = data$issues / data$downloads1M
data$downloads1M = log(data$downloads1M)
data$issues = log(data$issues)
data$effort = log(data$effort+1)
data$loc = log(data$loc+1)
data$cyclomatic = log(data$cyclomatic +1)
#data$effort = log(data$effort+1)
#data$maintainability = log(data$maintainability+1)
multi.hist(data)
mod1 = (lm(issues~., data = data))
summary(mod1)


library(e1071)
set.seed(1)
tuneResult = tune(randomForest, issues~.-quality , 
                  data = data, 
                  ranges = list(ntree=seq(100,1000, 100), mtry = 1:3),
                  tunecontrol = tune.control(nrepeat = 10, cross = 2))



rf = randomForest(issues~.-quality, data= data,
                  ntree = tuneResult$best.parameters$ntree, 
                  mtry = 3, 
                  importance = T)

varImpPlot(rf, type = 1, cex = 2, main = 'Variable Importance Plot')
actual <- data$issues
predicted <- unname(predict(rf, data))
R2 <- 1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2))

rf2 = randomForest(issues~.-quality-downloads1M, data= data,
                  ntree = tuneResult$best.parameters$ntree, 
                  mtry = 3, 
                  importance = T)

varImpPlot(rf2, type = 1, cex = 2, main = 'Variable Importance Plot')
actual <- data$issues
predicted <- unname(predict(rf2, data))
R20 <- 1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2))

#######
mp = c()

for (n in 1:10){
  cross.valid <- data[sample(nrow(data)),]
  folds <- cut(seq(1,nrow(cross.valid)),breaks=2,labels=FALSE)
  perf = c()
for(i in 1:2){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- cross.valid[testIndexes, ]
    trainData <- cross.valid[-testIndexes, ]
    rf2 = randomForest(issues~.-quality, data= trainData,
                       ntree = tuneResult$best.parameters$ntree, 
                       mtry = 3, 
                       importance = T)
    actual <- testData$issues
    predicted <- unname(predict(rf2, testData))
    R20 <- 1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2))
    perf = c(perf, R20)
  }
mp = c(mp, mean(perf))
}
summary(mp)

###############################################
data$quality = log(data$quality)
rf = randomForest(quality~.-issues-downloads1M, data= data,
                  ntree = tuneResult$best.parameters$ntree, 
                  mtry = tuneResult$best.parameters$mtry, 
                  importance = T)

varImpPlot(rf, type = 1, cex = 2, main = 'Variable Importance Plot')
actual <- data$quality
predicted <- unname(predict(rf, data))
R2 <- 1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2))

#####

mp = c()

for (n in 1:10){
  cross.valid <- data[sample(nrow(data)),]
  folds <- cut(seq(1,nrow(cross.valid)),breaks=2,labels=FALSE)
  perf = c()
  for(i in 1:2){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- cross.valid[testIndexes, ]
    trainData <- cross.valid[-testIndexes, ]
    rf2 = randomForest(quality~.-issues-downloads1M, data= trainData,
                       ntree = tuneResult$best.parameters$ntree, 
                       mtry = 3, 
                       importance = T)
    actual <- testData$quality
    predicted <- unname(predict(rf2, testData))
    R20 <- 1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2))
    perf = c(perf, R20)
  }
  mp = c(mp, mean(perf))
}
summary(mp)

#################################################
library(bnlearn)
q = data$quality
data$quality = NULL
boot2 = bnlearn::boot.strength(data = data, R = 500, algorithm = "hc", 
                               algorithm.args = list(score = "bic-g",  restart = 100, perturb = 2))
plot(boot2)

avg.boot = averaged.network(boot2, threshold = 0.51)
graphviz.plot(avg.boot, shape = "ellipse")

data$quality = q
d = data$downloads1M
data$downloads1M = NULL
ie = data$issues
data$issues = NULL

boot2 = bnlearn::boot.strength(data = data, R = 500, algorithm = "hc", 
                               algorithm.args = list(score = "bic-g",  restart = 100, perturb = 2))
plot(boot2)

avg.boot = averaged.network(boot2, threshold = 0.6)
graphviz.plot(avg.boot, shape = "ellipse")
