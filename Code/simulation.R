setwd("~/Work/release_qual/model")

library(bnlearn)
library(plyr)
load("refrs.Rdata")
N=1000

calcSimHelper = function(orig.net, result.net ){
  if ((compare(orig.net, result.net)$fp + 
       compare(orig.net, result.net)$fn) == 0){e = 1} else{ e=0}
  if ((compare(orig.net, result.net)$tp == 6 && 
       compare(orig.net, result.net)$fp < 2 &&
       compare(orig.net, result.net)$fn < 2)||
      (compare(orig.net, result.net)$tp == 8 && 
       compare(orig.net, result.net)$fp < 2 &&
       compare(orig.net, result.net)$fn < 2)) { obo = 1} else{obo = 0}
  return(list('Exceptionsact'=e, 'off.by.one'=obo))
}


# Generate Random Graph -------------------------------
custom = paste0("[Release.Date][New.Users][Exceptions|Release.Date:New.Users]",
               "[Usage.Intensity|New.Users][Release.Duration|Release.Date:New.Users]",
               "[Usage.Frequency|Release.Date:New.Users]")
testbn = model2network(custom)
graphviz.plot(testbn, shape = 'ellipse')

sim_result = data.frame("Itr"= 0, "Threshold" = 0, "Method" = 'Test', 
                      "Exact" = 0,"off.by.one" = 0, stringsAsFactors = F)

emp0 = "[Release.Date][New.Users][Exceptions][Usage.Intensity][Release.Duration][Usage.Frequency]"
emp = model2network(emp0)


for (i in 1:N){

# Generate test data ----------------------------------
testdata = rbn(testbn, n=1000, refrs)

# Method 1 : HC ---------------------------------------
boot2 = bnlearn::boot.strength(data = testdata, R = 500, algorithm = "hc", 
                               algorithm.args = list(score = "bic-g",  restart = 100, perturb = 2))
for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot2, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'HC',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}

# Method 2 : Hybrid - rsmax2 - si.hiton.pc --------------
boot2 = bnlearn::boot.strength(data = testdata, R = 500, algorithm = "rsmax2", 
                                 algorithm.args = list(restrict = "si.hiton.pc", 
                                                       maximize = "hc", maximize.args=list(score = "bic-g")))

for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot2, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'Hybrid- si.hiton.pc',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}


# Method 3 : Hybrid - rsmax2 - mmpc --------------
boot2 = bnlearn::boot.strength(data = testdata, R = 500, algorithm = "rsmax2", 
                                 algorithm.args = list(restrict = "mmpc", 
                                                       maximize = "hc", maximize.args=list(score = "bic-g")))

for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot2, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'Hybrid- mmpc',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}

# Method 4 : Hybrid - rsmax2 - gs --------------
boot2 = bnlearn::boot.strength(data = testdata, R = 500, algorithm = "rsmax2", 
                                 algorithm.args = list(restrict = "gs", 
                                                       maximize = "hc", maximize.args=list(score = "bic-g")))

for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot2, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'Hybrid- gs',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}

# Method 5 : Hybrid - rsmax2 - mmpc --------------
boot2 = bnlearn::boot.strength(data = testdata, R = 500, algorithm = "rsmax2", 
                                 algorithm.args = list(restrict = "iamb", 
                                                       maximize = "hc", maximize.args=list(score = "bic-g")))

for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot2, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'Hybrid- iamb',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}

# Method 6 : Hybrid - rsmax2 - mmpc --------------
boot2 = bnlearn::boot.strength(data = testdata, R = 500, algorithm = "rsmax2", 
                                 algorithm.args = list(restrict = "chow.liu", 
                                                       maximize = "hc", maximize.args=list(score = "bic-g")))

for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot2, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'Hybrid- cl',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}

# Method 7: MAP Bayesian Model Averaging -------------
nodes2.t = names(testdata)

start2.t = random.graph(nodes = nodes2.t, method = "melancon", num = 1000, 
                        burn.in = 10^5, every = 100)

netlist2.t = lapply(start2.t, function(net) {hc(testdata, score = "bic-g", start = net) })
boot3.t = custom.strength(netlist2.t, nodes = nodes2.t, cpdag = F)

for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot3.t, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'MAP',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}

################################################################
# Discretization 
################################################################

# D1 - Hartemink method -------------------------
tst.d = bnlearn::discretize(testdata, method = "hartemink", ibreaks = 3)


# Method 1D1: HC - Discretized
boot2 = bnlearn::boot.strength(data = tst.d, R = 500, algorithm = "hc", 
                                  algorithm.args = list(score = "bic",  
                                                        restart = 100, perturb = 2))
for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot2, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'HC-D-H',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}

# Method 2D1 : Hybrid - rsmax2 - si.hiton.pc --------------
boot2 = bnlearn::boot.strength(data = tst.d, R = 500, algorithm = "rsmax2", 
                                 algorithm.args = list(restrict = "si.hiton.pc", 
                                                       maximize = "hc", maximize.args=list(score = "bic")))

for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot2, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'Hybrid- si.hiton.pc -D-H',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}


# Method 3D1 : Hybrid - rsmax2 - mmpc --------------
boot2 = bnlearn::boot.strength(data = tst.d, R = 500, algorithm = "rsmax2", 
                                 algorithm.args = list(restrict = "mmpc", 
                                                       maximize = "hc", maximize.args=list(score = "bic")))

for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot2, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'Hybrid- mmpc -D-H',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}

# Method 4D1 : Hybrid - rsmax2 - gs --------------
boot2 = bnlearn::boot.strength(data = tst.d, R = 500, algorithm = "rsmax2", 
                                 algorithm.args = list(restrict = "gs", 
                                                       maximize = "hc", maximize.args=list(score = "bic")))

for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot2, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'Hybrid- gs -D-H',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}

# Method 5D1 : Hybrid - rsmax2 - mmpc --------------
boot2 = bnlearn::boot.strength(data = tst.d, R = 500, algorithm = "rsmax2", 
                                 algorithm.args = list(restrict = "iamb", 
                                                       maximize = "hc", maximize.args=list(score = "bic")))

for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot2, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'Hybrid- iamb -D-H',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}

# Method 6D1 : Hybrid - rsmax2 - mmpc --------------
boot2 = bnlearn::boot.strength(data = tst.d, R = 500, algorithm = "rsmax2", 
                                 algorithm.args = list(restrict = "chow.liu", 
                                                       maximize = "hc", maximize.args=list(score = "bic")))

for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot2, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'Hybrid- cl -D-H',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}

# Method 7D1 : MAP Bayesian Model Averaging -------------
nodes2.t = names(tst.d)

start2.t = random.graph(nodes = nodes2.t, method = "melancon", num = 1000, 
                        burn.in = 10^5, every = 100)

netlist2.t = lapply(start2.t, function(net) {hc(tst.d, score = "bic", start = net) })
boot3.t = custom.strength(netlist2.t, nodes = nodes2.t, cpdag = F)

for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot3.t, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'MAP -D-H',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}

# Method 8D1 - catnet -------------------------
library(catnet)
netlist = vector(200, mode = "list")
ndata = nrow(tst.d)
nodes = names(tst.d)
netlist = lapply(netlist, function(net) {
  boot = tst.d[sample(ndata, replace = TRUE), ]
  nets = cnSearchOrder(boot)
  best = cnFindBIC(nets, ndata)
  cnMatEdges(best)
})
sa = bnlearn::custom.strength(netlist, nodes = nodes)
for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(sa, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'Catnet -D-H',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
}

# Method 9D1 - Deal ----------------------------
library(deal)
deal.net = network(tst.d)
prior = jointprior(deal.net)
deal.net = learn(deal.net, tst.d, prior)$nw
deal.best = autosearch(deal.net, tst.d, prior)
dealnet = model2network(deal::modelstring(deal.best$nw))
res = calcSimHelper(testbn, dealnet)
sim_result = rbind(sim_result, c(as.numeric(i), 0, 'Deal -D-H',  
                                 as.numeric(res[[1]]), as.numeric(res[[2]] )))

# Discretize: D2 - Equal frequency ------------------

library(arules)
tst.d = data.frame(sapply(testdata, arules::discretize, method = "frequency", categories=3))

# Method 1D2: HC - Discretized
boot2 = bnlearn::boot.strength(data = tst.d, R = 500, algorithm = "hc", 
                               algorithm.args = list(score = "bic",  
                                                     restart = 100, perturb = 2))
for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot2, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'HC-D-F',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}

# Method 2D2 : Hybrid - rsmax2 - si.hiton.pc --------------
boot2 = bnlearn::boot.strength(data = tst.d, R = 500, algorithm = "rsmax2", 
                                 algorithm.args = list(restrict = "si.hiton.pc", 
                                                       maximize = "hc", maximize.args=list(score = "bic")))

for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot2, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'Hybrid- si.hiton.pc -D-F',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}


# Method 3D2 : Hybrid - rsmax2 - mmpc --------------
boot2 = bnlearn::boot.strength(data = tst.d, R = 500, algorithm = "rsmax2", 
                                 algorithm.args = list(restrict = "mmpc", 
                                                       maximize = "hc", maximize.args=list(score = "bic")))

for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot2, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'Hybrid- mmpc -D-F',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}

# Method 4D2 : Hybrid - rsmax2 - gs --------------
boot2 = bnlearn::boot.strength(data = tst.d, R = 500, algorithm = "rsmax2", 
                                 algorithm.args = list(restrict = "gs", 
                                                       maximize = "hc", maximize.args=list(score = "bic")))

for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot2, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'Hybrid- gs -D-F',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}

# Method 5D2 : Hybrid - rsmax2 - mmpc --------------
boot2 = bnlearn::boot.strength(data = tst.d, R = 500, algorithm = "rsmax2", 
                                 algorithm.args = list(restrict = "iamb", 
                                                       maximize = "hc", maximize.args=list(score = "bic")))

for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot2, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'Hybrid- iamb -D-F',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}

# Method 6D2 : Hybrid - rsmax2 - mmpc --------------
boot2 = bnlearn::boot.strength(data = tst.d, R = 500, algorithm = "rsmax2", 
                                 algorithm.args = list(restrict = "chow.liu", 
                                                       maximize = "hc", maximize.args=list(score = "bic")))

for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot2, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'Hybrid- cl -D-F',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}

# Method 7D2 : MAP Bayesian Model Averaging -------------
nodes2.t = names(tst.d)

start2.t = random.graph(nodes = nodes2.t, method = "melancon", num = 1000, 
                        burn.in = 10^5, every = 100)

netlist2.t = lapply(start2.t, function(net) {hc(tst.d, score = "bic", start = net) })
boot3.t = custom.strength(netlist2.t, nodes = nodes2.t, cpdag = F)

for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot3.t, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'MAP-D-F',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}

# Method 8D2 - catnet -------------------------
library(catnet)
netlist = vector(200, mode = "list")
ndata = nrow(tst.d)
nodes = names(tst.d)
netlist = lapply(netlist, function(net) {
  boot = tst.d[sample(ndata, replace = TRUE), ]
  nets = cnSearchOrder(boot)
  best = cnFindBIC(nets, ndata)
  cnMatEdges(best)
})
sa = bnlearn::custom.strength(netlist, nodes = nodes)
for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(sa, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'Catnet-D-F',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
}

# Method 9D2 - Deal ----------------------------
library(deal)
deal.net = network(tst.d)
prior = jointprior(deal.net)
deal.net = learn(deal.net, tst.d, prior)$nw
deal.best = autosearch(deal.net, tst.d, prior)
dealnet = model2network(deal::modelstring(deal.best$nw))
res = calcSimHelper(testbn, dealnet)
sim_result = rbind(sim_result, c(as.numeric(i), 0, 'Deal -D-F',  
                                 as.numeric(res[[1]]), as.numeric(res[[2]] )))
    
    
# Discretize: D3 - Equal Interval ------------------

library(arules)
tst.d = data.frame(sapply(testdata, arules::discretize, method = "interval", categories=3))

# Method 1D3: HC - Discretized
boot2 = bnlearn::boot.strength(data = tst.d, R = 500, algorithm = "hc", 
                               algorithm.args = list(score = "bic",  
                                                     restart = 100, perturb = 2))
for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot2, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'HC-D-I',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}

# Method 2D3 : Hybrid - rsmax2 - si.hiton.pc --------------
boot2 = bnlearn::boot.strength(data = tst.d, R = 500, algorithm = "rsmax2", 
                                 algorithm.args = list(restrict = "si.hiton.pc", 
                                                       maximize = "hc", maximize.args=list(score = "bic")))

for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot2, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'Hybrid- si.hiton.pc -D-I',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}


# Method 3D3 : Hybrid - rsmax2 - mmpc --------------
boot2 = bnlearn::boot.strength(data = tst.d, R = 500, algorithm = "rsmax2", 
                                 algorithm.args = list(restrict = "mmpc", 
                                                       maximize = "hc", maximize.args=list(score = "bic")))

for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot2, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'Hybrid- mmpc -D-I',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}

# Method 4D3 : Hybrid - rsmax2 - gs --------------
boot2 = bnlearn::boot.strength(data = tst.d, R = 500, algorithm = "rsmax2", 
                                 algorithm.args = list(restrict = "gs", 
                                                       maximize = "hc", maximize.args=list(score = "bic")))

for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot2, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'Hybrid- gs -D-I',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}

# Method 5D3 : Hybrid - rsmax2 - mmpc --------------
boot2 = bnlearn::boot.strength(data = tst.d, R = 500, algorithm = "rsmax2", 
                                 algorithm.args = list(restrict = "iamb", 
                                                       maximize = "hc", maximize.args=list(score = "bic")))

for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot2, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'Hybrid- iamb -D-I',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}

# Method 6D3 : Hybrid - rsmax2 - mmpc --------------
boot2 = bnlearn::boot.strength(data = tst.d, R = 500, algorithm = "rsmax2", 
                                 algorithm.args = list(restrict = "chow.liu", 
                                                       maximize = "hc", maximize.args=list(score = "bic")))

for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot2, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'Hybrid- cl -D-I',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}

# Method 7D3 : MAP Bayesian Model Averaging -------------
nodes2.t = names(tst.d)

start2.t = random.graph(nodes = nodes2.t, method = "melancon", num = 1000, 
                        burn.in = 10^5, every = 100)

netlist2.t = lapply(start2.t, function(net) {hc(tst.d, score = "bic", start = net) })
boot3.t = custom.strength(netlist2.t, nodes = nodes2.t, cpdag = F)

for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot3.t, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'MAP-D-I',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}

# Method 8D3 - catnet -------------------------
library(catnet)
netlist = vector(200, mode = "list")
ndata = nrow(tst.d)
nodes = names(tst.d)
netlist = lapply(netlist, function(net) {
  boot = tst.d[sample(ndata, replace = TRUE), ]
  nets = cnSearchOrder(boot)
  best = cnFindBIC(nets, ndata)
  cnMatEdges(best)
})
sa = bnlearn::custom.strength(netlist, nodes = nodes)
for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(sa, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'Catnet-D-I',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
}

# Method 9D3 - Deal ----------------------------
library(deal)
deal.net = network(tst.d)
prior = jointprior(deal.net)
deal.net = learn(deal.net, tst.d, prior)$nw
deal.best = autosearch(deal.net, tst.d, prior)
dealnet = model2network(deal::modelstring(deal.best$nw))
res = calcSimHelper(testbn, dealnet)
sim_result = rbind(sim_result, c(as.numeric(i), 0, 'Deal -D-I',  
                                 as.numeric(res[[1]]), as.numeric(res[[2]] )))
    
# Discretize: D4 - k-means ------------------

library(arules)
tst.d = data.frame(sapply(testdata, arules::discretize, method = "cluster", categories=3))

# Method 1D4: HC - Discretized
boot2 = bnlearn::boot.strength(data = tst.d, R = 500, algorithm = "hc", 
                               algorithm.args = list(score = "bic",  
                                                     restart = 100, perturb = 2))
for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot2, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'HC-D-K',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}

# Method 2D4 : Hybrid - rsmax2 - si.hiton.pc --------------
boot2 = bnlearn::boot.strength(data = tst.d, R = 500, algorithm = "rsmax2", 
                                 algorithm.args = list(restrict = "si.hiton.pc", 
                                                       maximize = "hc", maximize.args=list(score = "bic")))

for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot2, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'Hybrid- si.hiton.pc -D-K',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}


# Method 3D4 : Hybrid - rsmax2 - mmpc --------------
boot2 = bnlearn::boot.strength(data = tst.d, R = 500, algorithm = "rsmax2", 
                                 algorithm.args = list(restrict = "mmpc", 
                                                       maximize = "hc", maximize.args=list(score = "bic")))

for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot2, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'Hybrid- mmpc -D-K',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}

# Method 4D4 : Hybrid - rsmax2 - gs --------------
boot2 = bnlearn::boot.strength(data = tst.d, R = 500, algorithm = "rsmax2", 
                                 algorithm.args = list(restrict = "gs", 
                                                       maximize = "hc", maximize.args=list(score = "bic")))

for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot2, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'Hybrid- gs -D-K',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}

# Method 5D4 : Hybrid - rsmax2 - mmpc --------------
boot2 = bnlearn::boot.strength(data = tst.d, R = 500, algorithm = "rsmax2", 
                                 algorithm.args = list(restrict = "iamb", 
                                                       maximize = "hc", maximize.args=list(score = "bic")))

for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot2, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'Hybrid- iamb -D-K',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}

# Method 6D4 : Hybrid - rsmax2 - mmpc --------------
boot2 = bnlearn::boot.strength(data = tst.d, R = 500, algorithm = "rsmax2", 
                                 algorithm.args = list(restrict = "chow.liu", 
                                                       maximize = "hc", maximize.args=list(score = "bic")))

for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot2, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'Hybrid- cl -D-K',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}

# Method 7D4 : MAP Bayesian Model Averaging -------------
nodes2.t = names(tst.d)

start2.t = random.graph(nodes = nodes2.t, method = "melancon", num = 1000, 
                        burn.in = 10^5, every = 100)

netlist2.t = lapply(start2.t, function(net) {hc(tst.d, score = "bic", start = net) })
boot3.t = custom.strength(netlist2.t, nodes = nodes2.t, cpdag = F)

for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(boot3.t, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'MAP-D-K',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
  
}

# Method 8D4 - catnet -------------------------
library(catnet)
netlist = vector(200, mode = "list")
ndata = nrow(tst.d)
nodes = names(tst.d)
netlist = lapply(netlist, function(net) {
  boot = tst.d[sample(ndata, replace = TRUE), ]
  nets = cnSearchOrder(boot)
  best = cnFindBIC(nets, ndata)
  cnMatEdges(best)
})
sa = bnlearn::custom.strength(netlist, nodes = nodes)
for (t in seq(0.55, 1, 0.05 )){
  avg.boot = averaged.network(sa, threshold = t)
  avg.boot =tryCatch({cextend(avg.boot)}, error=function(err){emp})
  res = calcSimHelper(testbn, avg.boot)
  sim_result = rbind(sim_result, c(as.numeric(i), as.numeric(t), 'Catnet-D-K',  
                                   as.numeric(res[[1]]), as.numeric(res[[2]] )))
}

# Method 9D4 - Deal ----------------------------
library(deal)
deal.net = network(tst.d)
prior = jointprior(deal.net)
deal.net = learn(deal.net, tst.d, prior)$nw
deal.best = autosearch(deal.net, tst.d, prior)
dealnet = model2network(deal::modelstring(deal.best$nw))
res = calcSimHelper(testbn, dealnet)
sim_result = rbind(sim_result, c(as.numeric(i), 0, 'Deal -D-K',  
                                 as.numeric(res[[1]]), as.numeric(res[[2]] )))
}



sim_result = sim_result[-c(1),]
sim_result$Exact = as.numeric(sim_result$Exact)
sim_result$off.by.one = as.numeric(sim_result$off.by.one)
sim_result$Itr = as.integer(sim_result$Itr)
sim_result$Threshold = as.numeric(sim_result$Threshold)
final_res1 = ddply(sim_result, .(Method), summarise,
                   Exact = sum(Exact)/(10*N),
                   Off_by_one = sum(off.by.one)/(10*N)
)

final_res2 = ddply(sim_result, .(Method, Threshold), summarise,
                  Exact = sum(Exact)/N,
                  Off_by_one = sum(off.by.one)/N
                  )


summary(sim_result)

final_res1 = final_res1[final_res1$Off_by_one > 0, ]
final_res1[order(final_res1$Off_by_one, decreasing = T),]

final_res2 = final_res2[final_res2$Method == 'MAP'| final_res2$Method == 'HC',]

final_res2[order(final_res2$Exact, decreasing = T),]
