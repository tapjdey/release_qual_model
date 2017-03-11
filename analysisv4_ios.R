setwd("~/Work/release_qual/model/ios_img")
#library
library(plyr)
library(GGally)
library(ggplot2)
library(psych)
library(DataCombine)
library(reshape)
library(gridExtra)
library(grid)

#Reading .user data
filelist <- list.files(path = "../../data/data1_May16/", pattern = "*335-3.new.user", full.names = TRUE)
#Reading New data
filelist1 <- list.files(path = "../../data/data1_May16/", pattern = "*335-3.new$", full.names = TRUE)

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



###############################################################################################################
###############################################################################################################
library(plyr)
library(GGally)
library(ggplot2)
library(psych)

#2.0.0_317 last day,2.0.0_326 first 2 days,2.0.0_350 last day, 435 last day, 2.1.0_483 last 2, 503 first, 3.0.0_197 last
#total = total[!rownames(total) %in% c(504,759,761,2010,12248,13245,13244,13825,49530,48963),]


#collapsing all releases to get uniform curve
releases = unique(total$ga.appVersion)
goodrelease = c()
rd = matrix(nrow = 0, ncol=9)

for (r in sort(releases)){
  y= total[total$ga.appVersion == r,]
  z= ddply(y, .(ga.date), summarise,
           nu = sum(ga.newUsers),
           nv = sum(ga.newVisits),
           tu = sum(ga.users),
           tv = sum(ga.visits),
           ex = sum(ga.exceptions))

  #removing suspicious releases
#  if (quantile(z$nu,0.1) == quantile(z$nu,0.9) || quantile(z$nv,0.1) == quantile(z$nv,0.9)) next()
  #keeping only releases with >7 days of data or non zero number of exceptions
  if(nrow(z) > 7){
    #data splitting
    if(r == "6.2.11"){
      ts <- seq.POSIXt(as.POSIXlt(min(z$ga.date)), as.POSIXlt(max(z$ga.date)), by="day")
      df <- data.frame(ga.date=ts)
      df2 <- merge(df,z,all=T)
      df2[is.na(df2)] <- 0
      
      df2$ga.date = as.Date(df2$ga.date)
      z = df2[393:nrow(df2),]
      d = df2[1:392,]
      
      d$cnu = cumsum(d$nu)
      png(paste0(r,"_spr.png"),width = 960, height = 960)
      pairs.panels(d)
      dev.off()
      
      rd = rbind(rd,(c(paste0(r,"0"), min(d$ga.date), max(d$ga.date),(d[which(d$cnu > quantile(d$cnu,0.5)),]$ga.date)[1],
                       (d[which(d$cnu > quantile(d$cnu,0.25)),]$ga.date)[1],(d[which(d$cnu > quantile(d$cnu,0.75)),]$ga.date)[1],
                       (d[which(d$cnu >= quantile(d$cnu,0.9)),]$ga.date)[1],sum(d$nu),sum(d$ex))))
    } else if(r == "6.2.12"){
      ts <- seq.POSIXt(as.POSIXlt(min(z$ga.date)), as.POSIXlt(max(z$ga.date)), by="day")
      df <- data.frame(ga.date=ts)
      df2 <- merge(df,z,all=T)
      df2[is.na(df2)] <- 0
      
      df2$ga.date = as.Date(df2$ga.date)
      z = df2[860:nrow(df2),]
      d = df2[1:859,]
      
      d$cnu = cumsum(d$nu)
      png(paste0(r,"_spr.png"),width = 960, height = 960)
      pairs.panels(d)
      dev.off()
      rd = rbind(rd,(c(paste0(r,"0"), min(d$ga.date), max(d$ga.date),(d[which(d$cnu > quantile(d$cnu,0.5)),]$ga.date)[1],
                       (d[which(d$cnu > quantile(d$cnu,0.25)),]$ga.date)[1],(d[which(d$cnu > quantile(d$cnu,0.75)),]$ga.date)[1],
                       (d[which(d$cnu >= quantile(d$cnu,0.9)),]$ga.date)[1],sum(d$nu),sum(d$ex))))
    }
    
    z$cnu = cumsum(z$nu)
    quantile(z$cnu)
    goodrelease = c(goodrelease,r)
    png(paste0(r,".png"),width = 960, height = 960)
    pairs.panels(z)
    dev.off()
    
    
    rd = rbind(rd,(c(r, min(z$ga.date), max(z$ga.date),(z[which(z$cnu > quantile(z$cnu,0.5)),]$ga.date)[1],
                     (z[which(z$cnu > quantile(z$cnu,0.25)),]$ga.date)[1],(z[which(z$cnu > quantile(z$cnu,0.75)),]$ga.date)[1],
                     (z[which(z$cnu >= quantile(z$cnu,0.9)),]$ga.date)[1],sum(z$nu),sum(z$ex))))
  }
}

rd = as.data.frame(rd)
colnames(rd) = c("Release","Mindate","Maxdate","Median","25.Quant","75.Quant","90.Quant","Total.NU","ex")
Total.NU = as.numeric(as.character(rd$Total.NU))
rd$Total.NU = Total.NU
ex = as.numeric(as.character(rd$ex))
rd$ex = ex
rd$Mindate = as.Date(as.integer(as.character(rd$Mindate)),origin="1970-01-01")
rd$Maxdate = as.Date(as.integer(as.character(rd$Maxdate)),origin="1970-01-01")
rd$Median = as.Date(as.integer(as.character(rd$Median)),origin="1970-01-01")
rd$`25.Quant` = as.Date(as.integer(as.character(rd$`25.Quant`)),origin="1970-01-01")
rd$`75.Quant` = as.Date(as.integer(as.character(rd$`75.Quant`)),origin="1970-01-01")
rd$`90.Quant` = as.Date(as.integer(as.character(rd$`90.Quant`)),origin="1970-01-01")

rd$Release = as.character(rd$Release)

tick.col = c()
for (i in 1:nrow(rd)){
  if (ecdf(Total.NU)(rd[i,8]) <0.2){
    rd[i,1] = paste0("",rd[i,1])
  } else if (ecdf(Total.NU)(rd[i,8]) <0.5){
    rd[i,1] = paste0("*",rd[i,1])
  }else if (ecdf(Total.NU)(rd[i,8]) <0.8){
    rd[i,1] = paste0("***",rd[i,1])
  }else {
    rd[i,1] = paste0("******",rd[i,1])
  }
  
  if (rd[i,9] == 0) {
    tick.col = c(tick.col,"green")
  } else if (rd[i,9] < 50){
    tick.col = c(tick.col,"blue")
  } else if (rd[i,9] < 100){
    tick.col = c(tick.col,"orange")
  } else if (rd[i,9] < 200){
    tick.col = c(tick.col,"red")
  }
}


rd$`25.Quant` = NULL
rd$`75.Quant` = NULL
rd$ex = NULL
rd$Total.NU = NULL


library(stringr)
rd$lvl = as.numeric(str_split_fixed(rd$Release,"[.]",3)[,3])
rd = rd[order(rd[,6]),]  
rd$lvl = NULL
rd$Release = as.factor(rd$Release)

# release dates added manually
rd$Release.Date = as.Date(c("2014-02-24","2014-06-27","2014-10-28","2014-11-02","2015-04-07","2015-06-04","2015-12-05",NA,NA,NA,NA,NA))

r=melt(rd, id="Release")

ggplot(r) + geom_point(aes(x=Release, y=value, colour=variable)) + 
  scale_color_manual(values = c("red","blue","black","magenta","cyan"))+ scale_x_discrete(limits=rd$Release) +
  theme_bw() + theme(axis.text.y = element_text(colour=tick.col), axis.text.x = element_text(colour="black"))  +
  coord_flip()

################
# split checking code
################
y= total[total$ga.appVersion == r,]
z= ddply(y, .(ga.date), summarise,
         nu = sum(ga.newUsers),
         nv = sum(ga.newVisits),
         tu = sum(ga.users),
         tv = sum(ga.visits),
         ex = sum(ga.exceptions))


ts <- seq.POSIXt(as.POSIXlt(min(z$ga.date)), as.POSIXlt(max(z$ga.date)), by="day")
df <- data.frame(ga.date=ts)
df2 <- merge(df,z,all=T)
df2[is.na(df2)] <- 0
diff(df2$nu)
#########
plot(df2$ga.date,df2$nu)
abline(v=df2$ga.date[which.max(abs(diff(df2$nu)))], col="blue")

