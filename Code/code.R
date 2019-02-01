setwd("~/Work/release_qual/model")

#library
library(plyr)
library(bnlearn)
library(car)
library(randomForest)
library(psych)
library(data.table)

## Functions ----------------------
inc <- function(x, n=1)
{
  eval.parent(substitute(x <- x + n))
}



###################################
# Reading and combining data-------
###################################

file.name = "UA-40745335-3"
# UA-40745335-3 - GA - mobile SIP for iOS
# UA-36442576-5 - Dev - communicator for Android
# UA-36442576-4 - GA - communicator for Android

#Reading .user data
UserFile =  read.csv(file = paste0("../data/data1_May16/",file.name,".new.user"), 
                     sep=";", na.strings="(not set)", stringsAsFactors=F)
UserFile = UserFile[,-c(2:5)]
UserFile$ga.date = as.Date(as.character(UserFile$ga.date),"%Y%m%d")
UserFile$ga.newUsers = as.integer(UserFile$ga.newUsers)
UserFile$ga.users = as.integer(UserFile$ga.users)
UserFile$ga.sessionsPerUser = as.numeric(UserFile$ga.sessionsPerUser)
UserFile = UserFile[complete.cases(UserFile),]
#Reading New data
NewFile =  read.csv(file = paste0("../data/data1_May16/",file.name,".new"), 
                    sep=";", na.strings="(not set)", stringsAsFactors=F)
NewFile[,3] <- as.Date(as.character(NewFile[,3]),"%Y%m%d")

#Merging
total = merge(NewFile,UserFile)
total$ga.fatalExceptions = NULL
total$ga.operatingSystemVersion = NULL
total$ga.sessionsPerUser = NULL



##############################################
# Data cleaning and pre-processing -----------
##############################################

#collapsing all releases 
# calculating daily numbers first for data correction
temp.total = ddply(total, .(ga.appVersion, ga.date), summarise,
                   exceptions = sum(ga.exceptions),
                   new.visits = sum(ga.newVisits),
                   visits = sum(ga.visits),
                   time.on.site = sum(ga.timeOnSite),
                   new.users = sum(ga.newUsers),
                   users = sum(ga.users))

# sorting by date
temp.total = temp.total[order(temp.total$ga.appVersion, temp.total$ga.date), ]

#Fixing Data
temp.total$new.visits.fix = temp.total$new.visits
temp.total$new.users.fix = temp.total$new.users

for (r in unique(temp.total$ga.appVersion)){
  t_nv = 0
  t_nu = 0
  tt = temp.total[which(temp.total$ga.appVersion == r), ]
  for (dt in as.character(tt$ga.date)){
    nv =  tt[ which(tt$ga.date == as.Date(dt)),'new.visits']
    nu =  tt[ which(tt$ga.date == as.Date(dt)),'new.users']
    av =  tt[ which(tt$ga.date == as.Date(dt)),'visits']
    au =  tt[ which(tt$ga.date == as.Date(dt)),'users']
    t_nv = t_nv + nv
    t_nu = t_nu + nu
    if (av > t_nv ){
      inc(temp.total[ which(temp.total$ga.appVersion == r &
        temp.total$ga.date == as.Date(dt)),'new.visits.fix'] ,  av - t_nv)
      t_nv = av
    }
    if (au > t_nu ){
      inc( temp.total[ which(temp.total$ga.appVersion == r &
          temp.total$ga.date == as.Date(dt)),'new.users.fix'], au - t_nu)
      t_nu = au
    }
  }
  tt = temp.total[which(temp.total$ga.appVersion == r), ]
  #png(paste0('./new_img/',file.name,'_',r,".png"),width = 960, height = 960)
  #pairs.panels(tt[,c(-1)])
  #dev.off()
}

# Checking cumulative sums for validation of fixes
temp.total.dt = data.table(temp.total)
temp.total.dt[ , cum.new.visits := cumsum(new.visits.fix), by=list(ga.appVersion)]
temp.total.dt[ , cum.new.users := cumsum(new.users.fix), by=list(ga.appVersion)]


# final collapse
final  = ddply(temp.total.dt, .(ga.appVersion), summarise,
               Exceptions = sum(exceptions),
               New.Visits = sum(new.visits.fix),
               Visits = sum(visits),
               Time.On.Site = sum(time.on.site),
               New.Users = sum(new.users.fix),
               Users = sum(users),
               Release.Date = min(ga.date),
               End.Date = max(ga.date)
               )
# Variable Construction
final$Release.Duration = as.numeric(final$End.Date - final$Release.Date + 1)
final$Usage.Frequency = final$New.Visits/final$New.Users
final$Usage.Intensity = final$Time.On.Site/final$New.Users


### Final DataSet ----------
rd = final[,c(2,6,8,10:12)]
rd$Release.Date = as.numeric(rd$Release.Date)

# Distribution
multi.hist(rd)

#log - transform due to shewness of data
refr = data.frame(sapply(rd,function(x) log(x+1)))
multi.hist(refr)

#Scaling
refrs = data.frame(sapply(refr,scale))
multi.hist(refrs)
save(refrs, file = 'refrs.Rdata')

##############################################
# LR Model -----------------------------------
##############################################

m = lm(Exceptions~., data = refr)
summary(m)
vif(m)

##############################################
# BN Model -----------------------------------
##############################################

# Bootstrapped HC search
boot2 = bnlearn::boot.strength(data = refrs, R = 500,  m=500, algorithm = "hc", 
                               algorithm.args = list(score = "bic-g",  restart = 100, perturb = 2))
plot(boot2)

# Set Threshold according to plot
avg.boot = averaged.network(boot2, threshold = 1)
graphviz.plot(avg.boot, shape = "ellipse")



# Comparing Score with empty graph

emp = "[Release.Date][New.Users][Exceptions][Usage.Intensity][Release.Duration][Usage.Frequency]"
emp.net = model2network(emp)
b = score(avg.boot, refrs, type = "loglik-g") 
e = score(emp.net, refrs, type = "loglik-g")
empty.diff = b - e
empty.diff/b


###############################################
# RF Model ------------------------------------
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

#############################################
# Quality Variable --------------------------
#############################################
refr$Quality = log(rd$Exceptions/rd$New.Users+1)
#refr$Quality = refr$Exceptions - refr$New.Users
summary(exp(refr$Quality)-1)

###############################################
# LR Model ------------------------------------
###############################################

m = lm(Quality~.-New.Users-Exceptions, data = refr)

summary(m)

vif(m)


###############################################
# BN Model
###############################################
refr2 = refr[,-c(1,2)]
refr2s = data.frame(sapply(refr2,scale))

# Bootstrapped HC search
boot2 = bnlearn::boot.strength(data = refr2s, R = 500,  m=500, algorithm = "hc", 
                               algorithm.args = list(score = "bic-g",  restart = 100, perturb = 2))
plot(boot2)

# Set Threshold according to plot
avg.boot = averaged.network(boot2, threshold = 1)
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

#####################################################
# Timeline
#####################################################
library(ggplot2)
library(reshape)
temp.total.dt = temp.total.dt[order(temp.total.dt$ga.appVersion, temp.total.dt$ga.date), ]

temp.total.dt[ , cum.ex := cumsum(exceptions), by=list(ga.appVersion)]
temp.total.dt[ , cum.nu := cumsum(new.users.fix), by=list(ga.appVersion)]
temp.total.dt[ , c.Q := cum.ex/cum.nu, by=list(ga.appVersion)]


mdf <- melt(temp.total.dt, id.vars=c("ga.appVersion", 'ga.date'), measure.vars = 'c.Q')
colnames(mdf) = c('Release.Version', 'Date', 'variable', 'Quality')
#mdf$Quality = mdf$Quality + 1e-18
ggplot(mdf, aes(x=Date, y=Quality, colour = Release.Version, 
                shape = Release.Version)) + geom_point() + 
  scale_shape_manual(values = 1:173) + geom_line() +
  scale_y_log10() + theme_bw()

