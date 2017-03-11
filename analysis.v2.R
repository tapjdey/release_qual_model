setwd("~/Work/release_qual/model")

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
total$ga.timeOnSite  <- NULL
total <- total[order(total[,3]),]

###########################
library(GGally)
library(ggplot2)
library(psych)

pairs.panels(total)
###########################
# AR

ardata = total

ardata$ga.date = NULL

ardata$exception= as.factor(ifelse(ardata$ga.exceptions>0,1,0))
ardata$ga.exceptions = NULL

brk = c(0,1,50,Inf)
ardata$newvisit = cut(ardata$ga.newVisits, breaks = brk, include.lowest = T)
ardata$ga.newVisits = NULL

hist(log(ardata$ga.visits+1))
brk = c(0,1,2,3,4,Inf)
ardata$logvisit = cut(log10(ardata$ga.visits+1), breaks=brk, include.lowest = T)
ardata$ga.visits=NULL

hist(ardata$ga.newUsers)
ardata$newuser = as.factor(ifelse(ardata$ga.newUsers>1,1,0))
ardata$ga.newUsers = NULL

hist(ardata$ga.users)
brk = c(0,4,Inf)
ardata$users = cut(ardata$ga.users, breaks=brk, include.lowest = T)
ardata$ga.users = NULL

hist(ardata$ga.sessionsPerUser)
brk = c(0,200,500, Inf)
ardata$sessionperuser = cut(ardata$ga.sessionsPerUser, breaks=brk, include.lowest = T)
ardata$ga.sessionsPerUser = NULL



#####

ardata$exception[ardata$exception==0] = NA
ardata$exception = factor(ardata$exception)

library(arules)
rules.all <- apriori(ardata, parameter = list(minlen=2, supp=0.2))
inspect(rules.all)

itemsets <- unique(generatingItemsets(rules.all))
itemsets = itemsets[is.maximal(itemsets)]
inspect(itemsets)

quality(rules.all) <- round(quality(rules.all), digits=3)

## order rules by lift (or you may sort by support or confidence)
rules.sorted <- sort(rules.all, by="lift")

inspect(rules.sorted)

#viz
library(arulesViz)
plot(rules.all)
plot(rules.all, method = "grouped")

plot(rules.all, method = "graph", control = list(type = "items"))
plot(rules.all, method = "paracoord", control = list(reorder = TRUE))

