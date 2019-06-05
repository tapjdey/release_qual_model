setwd("~/Work/release_qual/model")

#library
library(plyr)
library(bnlearn)
library(car)
library(randomForest)
library(psych)
library(data.table)
library(e1071)

## Functions ----------------------
inc <- function(x, n=1)
{
  eval.parent(substitute(x <- x + n))
}

nlog = function(x){return (sign(x)*log(abs(x)+1))}
rcsum = function(x){
  y = c(0, x)
  y = y[1:length(y)-1]
  return (x-y)
}
rmse <- function(error)
{
  sqrt(mean(error^2))
}




###################################
# Reading and combining data-------
###################################

file.name = "UA-40745335-3"
# UA-40745335-3 - GA - mobile SIP for iOS
# UA-36442576-5 - Dev - communicator for Android
# UA-36442576-4 - GA - communicator for Android


iflag = ifelse (file.name == 'UA-40745335-3', 1, 0)

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
rd = rd[order(rd$Release.Date),]

refr = rd
refr$Release.Date = as.numeric(refr$Release.Date)

refr = data.frame(sapply(refr, nlog))

#Scaling
refrs = data.frame(sapply(refr,scale))
cor(refrs, method = 'spearman')
multi.hist(refrs)
#save(refrs, file = 'refrs.Rdata')

##############################################
# LR Model -----------------------------------
##############################################

m = lm(Exceptions~., data = refrs)
summary(m)
vif(m)

##############################################
# BN Model -----------------------------------
##############################################

# Bootstrapped HC search
boot2 = bnlearn::boot.strength(data = refrs, R = 500, m = 500,  algorithm = "hc", 
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
# RF Model: 10 times 2 fold cross-validation using optimal parameters
###############################################

set.seed(1)
tuneResult = tune(randomForest, Exceptions~. , 
                  data = refrs, 
                  ranges = list(ntree=seq(100,500, 100), mtry = 1:3),
                  tunecontrol = tune.control(nrepeat = 10, cross = 2))

rf = randomForest(Exceptions~., data= refrs,
                  ntree = tuneResult$best.parameters$ntree, 
                  mtry = tuneResult$best.parameters$mtry, 
                  importance = T)

# variable importance plot
varImpPlot(rf)
# Rsq = 1 - tuneResult$best.performance *(sample_size)/(sum((refrs$Exceptions - mean(refrs$Exceptions))^2))

#############################################
# Quality Variable --------------------------
#############################################
refr$Quality = log(rd$Exceptions/rd$New.Users+1)
#refr$Quality = refr$Exceptions - refr$New.Users
summary(exp(refr$Quality)-1)
refr2 = refr[,-c(1,2)]
refr2s = data.frame(sapply(refr2,scale))

###############################################
# LR Model ------------------------------------
###############################################

m = lm(Quality~.-New.Users-Exceptions, data = refr)

summary(m)

vif(m)


###############################################
# BN Model
###############################################


# Bootstrapped HC search
boot2 = bnlearn::boot.strength(data = refr2s, R = 500,  m=500, algorithm = "hc", 
                               algorithm.args = list(score = "bic-g",  restart = 100, perturb = 2))
plot(boot2)

# Set Threshold according to plot
avg.boot = averaged.network(boot2, threshold = 1)
graphviz.plot(avg.boot, shape = "ellipse")





###############################################
# RF Model: 10 times 2 fold cross-validation using optimal parameters
###############################################

set.seed(1)
tuneResult = tune(randomForest, Quality~. , 
                  data = refr2s, 
                  ranges = list(ntree=seq(100,500, 100), mtry = 1:3),
                  tunecontrol = tune.control(nrepeat = 10, cross = 2))

rf = randomForest(Quality~., data= refr2s,
                  ntree = tuneResult$best.parameters$ntree, 
                  mtry = tuneResult$best.parameters$mtry, 
                  importance = T)

# variable importance plot
varImpPlot(rf)
# Rsq = 1 - tuneResult$best.performance *(sample_size)/(sum((refrs$Exceptions - mean(refrs$Exceptions))^2))

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

########################################################
# Comparative trend plot
df = data.frame(Date = rd$Release.Date, Exceptions = refr$Exceptions, Quality = refr2$Quality )
par(mar=c(5, 4, 4, 6) + 0.2)
plot(df$Date, df$Exceptions, type = 'p', axes = F, pch = 10,
     xlab = '', ylab = '', main = 'Relative Trends of Two metrics')
axis(2, col="black",las=1)  ## las=1 makes horizontal labels
mtext("Exceptions (Log scale)",side=2,line=2.5)
box()
par(new=TRUE)
plot(df$Date, df$Quality, type = 'p', axes = F, 
     xlab = '', ylab = '', pch = 4, col = 'red')
axis(4, col="red",las=1, col.axis="red")  ## las=1 makes horizontal labels
mtext("Quality Variable (Log scale)",side=4,line=4, col = 'red')

## Draw the time axis
axis(1 , at= seq(16000, 17000, 90), cex.axis = 0.85,
     labels = format(as.Date(seq(16000, 17000, 90), origin = '1970-01-01'), '%b %Y'), las=3)
mtext("Date",side=1,col="black",line=4.2)  

## Add Legend
#legend("topright",legend=c("Exceptions","Quality Variable"),
#  text.col=c("black","red"),pch=c(16,4),col=c("black","red"))
abline(v = df$Date, col = 'blue', lty=5, lwd =0.1)
