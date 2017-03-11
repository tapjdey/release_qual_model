setwd("~/Work/release_qual/model")
#library
library(plyr)
library(GGally)
library(ggplot2)
library(psych)
library('DataCombine')

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

#############################
# Grouping by month
#############################

library(lubridate)
total$month = floor_date(total$ga.date,"month")

base = ddply(total, .(month, ga.appVersion, ga.operatingSystemVersion), summarise, 
             excep = ifelse(sum(ga.exceptions)>0,1,0), tot.newvisits = sum(ga.newVisits),
             tot.newusers = sum(ga.newUsers), tot.users = sum(ga.users), 
             tot.sessionperuser = sum(ga.sessionsPerUser))

timebase = slide(base, Var = "excep", GroupVar = "ga.appVersion", TimeVar = "month",NewVar = "excep1",slideBy = -1)
timebase = timebase[complete.cases(timebase),]
pairs.panels(timebase)

mod1 = lm(tot.newvisits~excep1, data = timebase)
summary(mod1)

timebase = ddply(base, .(month), summarise, texcep = ifelse(sum(excep)>0,1,0), newvis = sum(tot.newvisits))




