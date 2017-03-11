setwd("~/Work/release_qual/model/images")

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

###############################################################################################################
###############################################################################################################

#collapsing all releases to get uniform curve
releases = unique(total$ga.appVersion)
agg = data.frame()
for (r in sort(releases)){
  y= total[total$ga.appVersion == r,]
  z= aggregate.data.frame(y$ga.newUsers,by=list(unique.values= y$ga.date), FUN = sum)
  if(sum(z[,2]>0)){
  k = c()
  for (i in seq_along(z[,1])){k[i] = z[i,1] - z[1,1]}
  z$day = k
  z$unique.values = NULL
  if (sum(z$x) > 20){
  x=paste(r,"_NewUser.jpg",sep = "")

  z$appversion = rep(r,dim(z)[1])
  agg = rbind(agg,z)
  z1= aggregate.data.frame(y$ga.users ,by=list(unique.values= y$ga.date), FUN = sum)
  z1$day = k
  z1$unique.values = NULL
  x1=paste(r,"_TotalUser.jpg",sep = "")

  z2= aggregate.data.frame(y$ga.exceptions ,by=list(unique.values= y$ga.date), FUN = sum)
  z2$day = k
  z2$unique.values = NULL
  x2=paste(r,"_Exceptions.jpg",sep = "")
  
  if (sum(z2$x) > 0){
  jpeg(x)
  plot(z$day,z$x, type="h",sub=r,ylab = "New User",xlab = "days")
  dev.off()
  jpeg(x2)
  plot(z2$day,z2$x, type="h",sub=r,ylab = "Exceptions",xlab = "days")
  dev.off()
  jpeg(x1)
  plot(z1$day,z1$x, type="h",sub=r,ylab = "Total User",xlab = "days")
  dev.off()}
  
  }}
  
}
agg2 = aggregate.data.frame(agg$x, by=list(day= agg$day), FUN = sum)






(cor(total[sapply(total, is.numeric)]))
user = total$ga.users
total$ga.users <- NULL
new = total$ga.newUsers
total$ga.newUsers <- NULL

umod <- glm( user ~., data = total, family = poisson())
(summary(umod))

#####################################################################################################
# Script by Dr. Mockus
#####################################################################################################



releases = unique(total$ga.appVersion)
cvt <- function(date,fmt="%Y-%m-%d"){
  unclass(as.POSIXct(strptime(date,fmt))+0);
}
rcvt <- function(secs){
  zero <- strptime("1970/01/01","%Y/%m/%d");
  as.POSIXct(zero+secs);
}

total$ts = cvt(total$ga.date);
gad=c()
for (r in sort(releases)){
  d = sort(total$ts[total$ga.appVersion==r])[1];
  gad = c(gad, d)
}
names(gad) = sort(releases)

stats = c();
for (r in sort(releases)){
  a = total[total$ga.appVersion==r,]
  a = a[order(a$ts),];
  e = sum(a$ga.exceptions);
  v = sum(a$ga.visits);
  nu = sum(a$ga.newUsers);
  u = sum(a$ga.users);
  tt = (a$ts[dim(a)[1]] - a$ts[1])/3600/24/36.25;
  gad = a$ts[1]/3600/24/36.25+1970;
  e1 = sum(a$ga.exceptions>0);
  v1 = sum(a$ga.visits>0);
  nu1 = sum(a$ga.newUsers>0);
  u1 = sum(a$ga.users>0);
  
  stats = rbind(stats, c(e,v,nu,u,tt,e1,v1,nu1,u1,gad));
}
stats=data.frame(stats);
row.names(stats) = sort(releases);
names(stats) = c("e","v","nu","u","tt","e1","v1","nu1","u1","gad");

#the exceptions/per new user drops 10 times with more new users
tapply(stats$e,stats$nu>100,mean)/tapply(stats$nu,stats$nu>100,mean)

#the exceptions/per visit drops 200+ times with more new users
tapply(stats$e,stats$nu>100,mean)/tapply(stats$v,stats$nu>100,mean)


# lets investigate in a more refined fashion
gate=quantile(stats$nu,0:4/4)
stats$qq = rep(0,dim(stats)[1]);
for (i in 1:4){
  stats$qq[gate[i]<=stats$nu&gate[i+1]<stats$nu] = i;
}
stats$qq[stats$nu==max(stats$nu)] = 3;

tapply(stats$e,stats$qq,mean)/tapply(stats$v,stats$qq,mean)

tapply(stats$e,stats$qq,mean)/tapply(stats$nu,stats$qq,mean)

# a similar picture! More new users, higher quality (lower chances for a user to experience an exception).
###################################################################################################
tapply(stats$e,stats$u>100,mean)/tapply(stats$u,stats$u>100,mean)

tapply(stats$e,stats$u>100,mean)/tapply(stats$v,stats$u>100,mean)
