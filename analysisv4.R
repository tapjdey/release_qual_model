setwd("~/Work/release_qual/model/images")
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
filelist <- list.files(path = "../../data/data1_May16/", pattern = "*4.new.user", full.names = TRUE)
#Reading New data
filelist1 <- list.files(path = "../../data/data1_May16/", pattern = "*4.new$", full.names = TRUE)

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
total = total[!rownames(total) %in% c(504,759,761,2010,12248,13245,13244,13825,49530,48963),]


#collapsing all releases to get uniform curve
releases = unique(total$ga.appVersion)
goodrelease = c()
rd = matrix(nrow = 0, ncol=10)

for (r in sort(releases)){
  y= total[total$ga.appVersion == r,]
  z= ddply(y, .(ga.date), summarise,
           nu = sum(ga.newUsers),
           nv = sum(ga.newVisits),
           tu = sum(ga.users),
           tv = sum(ga.visits),
           ex = sum(ga.exceptions))
  #removing suspicious releases
  if (quantile(z$nu,0.1) == quantile(z$nu,0.9) || quantile(z$nv,0.1) == quantile(z$nv,0.9)) next()
  #keeping only releases with >7 days of data or non zero number of exceptions
  if(nrow(z) > 7){
    z$cnu = cumsum(z$nu)
    quantile(z$cnu)
    goodrelease = c(goodrelease,r)
    png(paste0(r,".png"),width = 960, height = 960)
    pairs.panels(z)
    dev.off()
    rd = rbind(rd,(c(r, min(z$ga.date), max(z$ga.date),(z[which(z$cnu > quantile(z$cnu,0.5)),]$ga.date)[1],
                     (z[which(z$cnu > quantile(z$cnu,0.25)),]$ga.date)[1],(z[which(z$cnu > quantile(z$cnu,0.75)),]$ga.date)[1],
                     (z[which(z$cnu >= quantile(z$cnu,0.9)),]$ga.date)[1],sum(z$nu),sum(z$ex),sum(z$nv))))
  }
}

rd = as.data.frame(rd)
colnames(rd) = c("Release","Mindate","Maxdate","Median","25.Quant","75.Quant","90.Quant","Total.NU","ex","Total.NV")
rd$Total.NU = as.numeric(as.character(rd$Total.NU))
Total.NU = as.numeric(as.character(rd$Total.NU))
rd$Total.NV = as.numeric(as.character(rd$Total.NV))

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
    tick.col = c(tick.col,"black")
  } else if (rd[i,9] < 300){
    tick.col = c(tick.col,"blue")
  } else if (rd[i,9] < 750){
    tick.col = c(tick.col,"orange")
  }else {
    tick.col = c(tick.col,"red")
  }
}


rd$`25.Quant` = NULL
rd$`75.Quant` = NULL
rd$ex = NULL
rd$Total.NU = NULL
rd$Total.NV = NULL


# release dates added manually
manrd = data.frame("Release"=as.factor(c("******2.1.2_568","***2.1.2_574","******2.1.4_577","******2.0.0_340","******2.0.1_372","******2.1.0_527")),
                   "Release Date"=as.Date(c("2015-10-02","2016-01-06","2016-01-22","2014-03-17","2014-05-20","2014-12-15")))
nrd = merge(rd,manrd,all = T)

r=melt(nrd, id="Release")


ggplot(r) + geom_point(aes(x=Release, y=value, colour=variable)) + 
  scale_color_manual(values = c("red","blue","black","magenta","green"))+ ylab("Year")+  
  theme_bw(base_size = 20) +
  theme(axis.text.y = element_text(colour=tick.col, size=20), axis.title = element_text(size = 20),
        axis.text.x = element_text(colour="black", size=20)) +
  coord_flip()

##########################################################
library(bnlearn)
refr$Release = as.factor(refr$Release)
refr$duration = as.numeric(refr$`90.Quant` - refr$Mindate)
refr$f.duration = as.numeric(refr$Maxdate - refr$Mindate)
refr[,3:7] = NULL
refr$Release = NULL

refr$VperU = refr$Total.NV / refr$Total.NU
refr$durFromLast = rep(0, nrow(refr))
for (i in 2:nrow(refr)){
  refr[i,8] = refr[i,1] - refr[(i-1),1]
}
refr2 = refr
refr2$Mindate = as.numeric(refr2$Mindate)
refr = refr2

refr$Total.NU = log(refr$Total.NU+1)
refr$duration = log(refr$duration+1)
refr$ex = log(refr$ex + 1)
refr$VperU = log(refr$VperU)
refr$durFromLast = log(abs(refr$durFromLast)+1)

tnv = refr$Total.NV
refr$Total.NV = NULL
refr$f.duration = NULL
refr$Mindate = NULL

refrs = data.frame(sapply(refr,scale))

pdag1 = iamb(refr)
plot(pdag1)
dag1 = cextend(pdag1)
plot(dag1)
(b1 = bn.fit(dag1,refrs))
#print(b1$Total.NU)

dag2 = tabu(refr)
plot(dag2)
(b2 = bn.fit(dag2, refrs))
(var2 = b2$Total.NU$sd**2 + b2$ex$sd**2 + b2$duration$sd**2 + b2$VperU$sd**2 + b2$durFromLast$sd**2)

dag3 = rsmax2(refr, restrict = "si.hiton.pc", maximize = "tabu",
            test = "zf", alpha = 0.1, score = "bic-g")
plot(dag3)
(b3 = bn.fit(dag3, refrs))
(var3 = b3$Total.NU$sd**2 + b3$ex$sd**2 + b3$duration$sd**2 + b3$VperU$sd**2 + b3$durFromLast$sd**2)

# discrete? 
drefr = discretize(refr, method = "hartemink", breaks = 3, ibreaks = 4, idisc = "quantile")
# deal
library(deal)
deal.net = network(drefr)
prior = jointprior(deal.net)
deal.net = learn(deal.net, drefr, prior)$nw
deal.best = autosearch(deal.net, drefr, prior)
bnlearn:::fcat(deal::modelstring(deal.best$nw))
#catnet
library(catnet)
netlist = vector(200, mode = "list")
ndata = nrow(drefr)
nodes = names(drefr)
netlist = lapply(netlist, function(net) {
 boot = drefr[sample(ndata, replace = TRUE), ]
 nets = cnSearchOrder(boot)
 best = cnFindBIC(nets, ndata)
 cnMatEdges(best)
 })
sa = custom.strength(netlist, nodes = nodes)
plot(sa)
avg.catnet = averaged.network(sa, threshold = 0.7)
avg.catnet
plot(avg.catnet)
catfit = bn.fit(avg.catnet,drefr)
#boot
(boot = boot.strength(data = drefr, R = 200, algorithm = "hc",
                     algorithm.args = list(score = "bde", iss = 10)))
avg.boot = averaged.network(boot, threshold = 0.85)
plot(avg.boot)

#pcalg - doesn't work
# library(pcalg)
# suffStat = list(dm = sapply(drefr,as.integer)-1, nlev = sapply(drefr, nlevels),adaptDF = FALSE)
# pcalg.net = pc(suffStat, indepTest = disCItest, p = ncol(drefr),alpha = 0.05)

#mbde

#refr$Total.NU = log(refr$Total.NU+1)
drefr = discretize(refr, method = "hartemink", breaks = 3, ibreaks = 4, idisc = "quantile")
nodes = names(drefr)

start = random.graph(nodes = nodes, method = "melancon", num = 500, 
                     burn.in = 10^5, every = 100)

netlist = lapply(start, function(net) {tabu(drefr, score = "bde", 
                                            iss = 1, start = net, tabu = 50) })
arcs = custom.strength(netlist, nodes = nodes, cpdag = F)
plot(arcs)
bn.mbde = averaged.network(arcs, threshold = 0.12)
plot(bn.mbde)
(b = bn.fit(bn.mbde, refr))
(var4 = b$Total.NU$sd**2 + b$ex$sd**2 + b$duration$sd**2 + b$VperU$sd**2 + b$durFromLast$sd**2)


#########################
custom = paste("[duration][ex|Total.NU:duration][durFromLast][Total.NU|duration:durFromLast][VperU|Total.NU]")
custom.net = model2network(custom)
library(Rgraphviz)
graphviz.plot(custom.net, shape = "ellipse")
(b5 = bn.fit(custom.net, refrs))
(var5 = b5$Total.NU$sd**2 + b5$ex$sd**2 + b5$duration$sd**2 + b5$VperU$sd**2 + b5$durFromLast$sd**2)

###########################################################
# plot2 = qplot(y=rd$Release, x=log(Total.NU),xlab = "Release Size", ylab = "")
# 
# 
# 
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# 
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(1, 5))) #1 rows, 4 columns
# print(plot2, vp = vplayout(1, 1:2))  
# print(plot1, vp = vplayout(1, 3:5))
# 
# 
# 
# ########################
# # Looking for generic difference between exception & non exceptions
# Mode <- function(x) {
#    x <- na.omit(x)
#    ux <- unique(x)
#    ux[which.max(tabulate(match(x, ux)))]
#  }
# 
# tab <- as.data.frame(matrix(NA,nrow=5,ncol=12))
# names(tab) <- as.vector(t(outer(c("Excep","No.Excep","All"), c("mean","median","mode","sd"), paste, sep="."))) 
# row.names(tab) <- c("New.User","Total.user","New.Visit","Tot.Visit","Session.P.U")
# 
# tab[1:5,9] <- round(colMeans(total[,c(11,12,5,6,13)]),2)
# tab[1:5,10] <- round(apply(total[,c(11,12,5,6,13)],2,median),2)
# tab[1:5,11] <- round(apply(total[,c(11,12,5,6,13)],2,Mode),2)
# tab[1:5,12] <- round(apply(total[,c(11,12,5,6,13)],2,sd),2)
# 
# y = total[which(total$ga.exceptions == 0),c(11,12,5,6,13)]
# tab[1:5,1] <- round(colMeans(y),2)
# tab[1:5,2] <- round(apply(y,2,median),2)
# tab[1:5,3] <- round(apply(y,2,Mode),2)
# tab[1:5,4] <- round(apply(y,2,sd),2)
# 
# y = total[which(total$ga.exceptions != 0),c(11,12,5,6,13)]
# tab[1:5,5] <- round(colMeans(y),2)
# tab[1:5,6] <- round(apply(y,2,median),2)
# tab[1:5,7] <- round(apply(y,2,Mode),2)
# tab[1:5,8] <- round(apply(y,2,sd),2)
# 
# tab
# 
# ## Aggregated on release level
# filtered = total[which(total$ga.appVersion %in% goodrelease),]
# agg.f = ddply(filtered,.(ga.appVersion), summarise,
#                      nu = sum(ga.newUsers),
#                      nv = sum(ga.newVisits),
#                      tu = sum(ga.users),
#                      tv = sum(ga.visits),
#                      spu = sum(ga.sessionsPerUser),
#                      ex = sum(ga.exceptions))
# 
# agg.filtered = data.frame(scale(agg.f[,2:6]))
# 
# tab <- as.data.frame(matrix(NA,nrow=5,ncol=12))
# names(tab) <- as.vector(t(outer(c("Excep","No.Excep","All"), c("mean","median","mode","sd"), paste, sep="."))) 
# row.names(tab) <- c("New.User","Total.user","New.Visit","Tot.Visit","Session.P.U")
# 
# tab[1:5,9] <- round(colMeans(agg.filtered),2)
# tab[1:5,10] <- round(apply(agg.filtered,2,median),2)
# tab[1:5,11] <- round(apply(agg.filtered,2,Mode),2)
# tab[1:5,12] <- round(apply(agg.filtered,2,sd),2)
# 
# y = agg.filtered[which(agg.f$ex == 0),]
# tab[1:5,1] <- round(colMeans(y),2)
# tab[1:5,2] <- round(apply(y,2,median),2)
# tab[1:5,3] <- round(apply(y,2,Mode),2)
# tab[1:5,4] <- round(apply(y,2,sd),2)
# 
# y = agg.filtered[which(agg.f$ex != 0),]
# tab[1:5,5] <- round(colMeans(y),2)
# tab[1:5,6] <- round(apply(y,2,median),2)
# tab[1:5,7] <- round(apply(y,2,Mode),2)
# tab[1:5,8] <- round(apply(y,2,sd),2)
# 
# tab
# ############
# 
# total$binexcep = as.factor(ifelse(total$ga.exceptions>0,1,0))
# mod = glm(binexcep ~ ga.newUsers + ga.newVisits + ga.users + ga.visits + ga.sessionsPerUser, data = total,
#           family = binomial)
# 
# summary(mod)
