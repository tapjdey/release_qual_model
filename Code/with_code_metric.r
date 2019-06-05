setwd("~/work/release_qual/model")

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



#Reading .user data
UserFile =  read.csv(file = paste0("../data/data1_May16/",file.name,".new.user"), 
                     sep=";", na.strings="(not set)", stringsAsFactors=F)
UserFile = UserFile[ , -c(2:5)]
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

##### Code Files ----------------

if (file.name == 'UA-40745335-3'){
    cfile = '~/rfa/ios_out.csv'
} else if (file.name == 'UA-36442576-5'){
    cfile = '~/rfa/andro_out_d.csv'
} else {
    cfile = '~/rfa/andro_out_ga.csv'
}

code = fread(cfile, header = T)
code = data.frame(code)
code$date = as.Date(code$date)
code = code[order(code$date),]
code = code[,c(1,5)]

#v <- unlist(lapply(code, is.numeric))  

#code[,v] = data.frame(sapply(code[,v], rcsum))
#code$loc = rcsum(code$loc)
code = code[-c(1), ]

refr = merge(rd, code, by.x='Release.Date', by.y = 'date')
refr$Release.Date = as.numeric(refr$Release.Date)


refr = data.frame(sapply(refr, nlog))

#Scaling
refrs = data.frame(sapply(refr,scale))
cor(refrs, method = 'spearman')
multi.hist(refrs)
#save(refrs, file = 'refrs.Rdata')



##############################################
# BN Model -----------------------------------
##############################################

# Bootstrapped HC search
set.seed(100)
boot2 = bnlearn::boot.strength(data = refrs, R = 500, m= 500, algorithm = "tabu", 
                               algorithm.args = list(score = "bic-g"))
plot(boot2)


# Set Threshold according to plot
avg.boot = averaged.network(boot2, threshold = 0.99)
avg.boot = cextend(avg.boot)
graphviz.plot(avg.boot, shape = "ellipse",
              highlight = list(nodes=bnlearn::mb(avg.boot, "Exceptions"), fill="orangered"))

avg.boot = drop.arc(avg.boot, from = 'Exceptions', to='Release.Duration')
graphviz.plot(avg.boot, shape = "ellipse", 
              highlight = list(nodes=bnlearn::mb(avg.boot, "Exceptions"), fill="orangered"))

# Comparing Score with empty graph

emp = "[Release.Date][New.Users][Exceptions][Usage.Intensity][Release.Duration][Usage.Frequency][loc]"
emp.net = model2network(emp)
avg.boot = cextend(avg.boot)

b = score(avg.boot, refrs, type = "loglik-g") 
e = score(emp.net, refrs, type = "loglik-g")
empty.diff = b - e
empty.diff/b



set.seed(1)
tuneResult = tune(randomForest, Exceptions~. , data = refrs, ranges = list(ntree=seq(100,500, 100), mtry = 1:3))
print(tuneResult)

rfModel = randomForest(Exceptions~., data= refr,
                       ntree = tuneResult$best.parameters$ntree, mtry = tuneResult$best.parameters$mtry, 
                       importance = T)
varImpPlot(rfModel)

#1- 0.39*10/sum((refrs$Exceptions - mean(refrs$Exceptions))^2)



