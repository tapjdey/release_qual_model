setwd("~/Work/release_qual/model")

#library
library(plyr)
library(bnlearn)
library(car)
library(randomForest)


###################################
# Reading and combining data
###################################

#Reading .user data
filelist <- list.files(path = "../data/data1_May16/", pattern = "*4.new.user", full.names = TRUE)
#Reading New data
filelist1 <- list.files(path = "../data/data1_May16/", pattern = "*4.new$", full.names = TRUE)

UserFile = do.call(rbind, lapply(filelist, function(x) read.csv(file = x,sep=";",na.strings="(not set)")))
UserFile[,7] <- as.Date(UserFile[,7],"%Y%m%d")
UserFile[,8] <- as.integer(UserFile[,8])
UserFile[,9] <- as.integer(UserFile[,9])
UserFile[,10] <- as.numeric(UserFile[,10])
UserFile <- UserFile[order(UserFile[,7]),]

NewData <- do.call(rbind, lapply(filelist1, function(x) read.csv(file = x,sep=";")))
NewData[,3] <- as.Date(as.character(NewData[,3]),"%Y%m%d")
NewData <- NewData[order(NewData[,3]),]

total = merge(NewData,UserFile,all=T)
total = total[complete.cases(total),]
total$ga.deviceCategory <- factor(total$ga.deviceCategory)
total$ga.fatalExceptions <- NULL
#total$ga.timeOnSite  <- NULL



###############################################################################################################
# Data cleaning and pre-processing
###############################################################################################################


#2.0.0_317 last day,2.0.0_326 first 2 days,2.0.0_350 last day, 435 last day, 2.1.0_483 last 2, 503 first, 3.0.0_197 last
total = total[!rownames(total) %in% c(504,759,761,2010,12248,13245,13244,13825,49530,48963),]


#collapsing all releases to get uniform curve
releases = unique(total$ga.appVersion)
goodrelease = c()
rd = matrix(nrow = 0, ncol=11)

for (r in sort(releases)){
  y= total[total$ga.appVersion == r,]
  z= ddply(y, .(ga.date), summarise,
           nu = sum(ga.newUsers),
           nv = sum(ga.newVisits),
           tu = sum(ga.users),
           tv = sum(ga.visits),
           ex = sum(ga.exceptions),
           tos = sum(ga.timeOnSite))
  #removing suspicious releases
  #if (quantile(z$nu,0.1) == quantile(z$nu,0.9) || quantile(z$nv,0.1) == quantile(z$nv,0.9)) next()
  if (sum(z$nu) <1) next()
  #keeping only releases with >1 days of data 
  if(nrow(z) > 0){
    z$cnu = cumsum(z$nu)
    quantile(z$cnu)
    goodrelease = c(goodrelease,r)
    #png(paste0(r,".png"),width = 960, height = 960)
    #pairs.panels(z)
    #dev.off()
    rd = rbind(rd,(c(r, min(z$ga.date), max(z$ga.date),(z[which(z$cnu > quantile(z$cnu,0.5)),]$ga.date)[1],
                     (z[which(z$cnu > quantile(z$cnu,0.25)),]$ga.date)[1],(z[which(z$cnu > quantile(z$cnu,0.75)),]$ga.date)[1],
                     (z[which(z$cnu >= quantile(z$cnu,0.9)),]$ga.date)[1],sum(z$nu),sum(z$ex),sum(z$nv),sum(z$tos))))
  }
}

rd = as.data.frame(rd)
colnames(rd) = c("Release","Mindate","Maxdate","Median","25.Quant","75.Quant","90.Quant","Total.NU","ex","Total.NV","Time.On.Site")
rd$Total.NU = as.numeric(as.character(rd$Total.NU))
Total.NU = as.numeric(as.character(rd$Total.NU))
rd$Total.NV = as.numeric(as.character(rd$Total.NV))
rd$Time.On.Site = as.numeric(as.character(rd$Time.On.Site))


ex = as.numeric(as.character(rd$ex))
rd$ex = ex
rd$Mindate = as.Date(as.integer(as.character(rd$Mindate)),origin="1970-01-01")
rd$Maxdate = as.Date(as.integer(as.character(rd$Maxdate)),origin="1970-01-01")
rd$Median = as.Date(as.integer(as.character(rd$Median)),origin="1970-01-01")
rd$`25.Quant` = as.Date(as.integer(as.character(rd$`25.Quant`)),origin="1970-01-01")
rd$`75.Quant` = as.Date(as.integer(as.character(rd$`75.Quant`)),origin="1970-01-01")
rd$`90.Quant` = as.Date(as.integer(as.character(rd$`90.Quant`)),origin="1970-01-01")

rd$Release = as.character(rd$Release)

refr = rd

#Total.NU more believeable because no double counting while aggregating day by day stats

refr$Release = as.factor(refr$Release)
refr$duration = as.numeric(refr$`90.Quant` - refr$Mindate)
refr$f.duration = as.numeric(refr$Maxdate - refr$Mindate)
refr = refr[,-c(3:7)]
refr$Release = NULL

refr$VperU = refr$Total.NV / refr$Total.NU
refr$Time.On.Site = refr$Time.On.Site / refr$Total.NU

refr$Mindate = as.numeric(refr$Mindate)


refr = plyr::rename(refr,c('Mindate' = 'Start.Date'))

# tnv = refr$Total.NV
refr$Total.NV = NULL
# refr$durFromLast = NULL
refr$f.duration = NULL
# refr$Mindate = NULL

# Log transform
refr = data.frame(sapply(refr,function(x) log(x+1)))
colnames(refr) = c("Release.Date","New.Users","Exceptions","Usage.Intensity","Release.Duration","Usage.Frequency")

#Scaling
refrs = data.frame(sapply(refr,scale))
colnames(refrs) = c("Release.Date","New.Users","Exceptions","Usage.Intensity","Release.Duration","Usage.Frequency")

###############################################
# LR Model
###############################################

m = lm(Exceptions~., data = refr)

summary(m)

vif(m)


###############################################
# BN Model
###############################################

# Bootstrapped HC search
boot2 = bnlearn::boot.strength(data = refrs, R = 500,  m=500, algorithm = "hc", 
                               algorithm.args = list(score = "bic-g",  restart = 100, perturb = 2))
plot(boot2)

# Set Threshold according to plot
avg.boot = averaged.network(boot2, threshold = 0.51)
graphviz.plot(avg.boot, shape = "ellipse")



# Comparing Score with empty graph

emp = "[Release.Date][New.Users][Exceptions][Usage.Intensity][Release.Duration][Usage.Frequency]"
emp.net = model2network(emp)
b = score(avg.boot, refrs, type = "loglik-g") 
e = score(emp.net, refrs, type = "loglik-g")
empty.diff = b - e
empty.diff/b


###############################################
# RF Model
###############################################

cross.valid <- refr[sample(nrow(refr)),]
folds <- cut(seq(1,nrow(cross.valid)),breaks=10,labels=FALSE)
cv.dd = cross.valid$Exceptions
cross.valid$Exceptions = NULL
Rsq = numeric(0)

for(i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- cross.valid[testIndexes, ]
  trainData <- cross.valid[-testIndexes, ]
  yTest = cv.dd[testIndexes]
  yTrain = cv.dd[-testIndexes]
  
  rf <- randomForest(x=trainData, y=yTrain, ntree=500, importance = T)
  
  pred <- predict(rf, testData)
  
  R2 = (1 - (sum((yTest-pred)^2)/sum((yTest-mean(yTest))^2)) )
  Rsq = c(Rsq, (1 - (1-R2)*(12998/(12998-19))))
  
}

# Print R-squared value
Rsq
# variable importance plot
varImpPlot(rf)

################################################################################
# Quality Measure
################################################################################
refr$Quality = log(rd$ex/rd$Total.NU+1)
#refr$Quality = refr$Exceptions - refr$New.Users
summary(exp(refr$Quality)-1)

###############################################
# LR Model
###############################################

m = lm(Quality~.-New.Users-Exceptions, data = refr)

summary(m)

vif(m)


###############################################
# BN Model
###############################################
refr2 = refr[,-c(2,3)]


# Bootstrapped HC search
boot2 = bnlearn::boot.strength(data = refr2, R = 500,  m=500, algorithm = "hc", 
                               algorithm.args = list(score = "bic-g",  restart = 100, perturb = 2))
plot(boot2)

# Set Threshold according to plot
avg.boot = averaged.network(boot2, threshold = 0.99)
graphviz.plot(avg.boot, shape = "ellipse")





###############################################
# RF Model
###############################################

cross.valid <- refr2[sample(nrow(refr2)),]
folds <- cut(seq(1,nrow(cross.valid)),breaks=10,labels=FALSE)
cv.dd = cross.valid$Quality
cross.valid$Quality = NULL
Rsq = numeric(0)

for(i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- cross.valid[testIndexes, ]
  trainData <- cross.valid[-testIndexes, ]
  yTest = cv.dd[testIndexes]
  yTrain = cv.dd[-testIndexes]
  
  rf <- randomForest(x=trainData, y=yTrain, ntree=500, importance = T)
  
  pred <- predict(rf, testData)
  
  R2 = (1 - (sum((yTest-pred)^2)/sum((yTest-mean(yTest))^2)) )
  Rsq = c(Rsq, (1 - (1-R2)*(12998/(12998-19))))
  
}

# Print R-squared value
Rsq
# variable importance plot
varImpPlot(rf)