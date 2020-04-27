# parallel cores and check time
# there is also a function to give you normal r jags output

timer <- proc.time()
mj <- jags(data = dat, parameters.to.save = params, model = "model.txt",
           n.chains = setts$n.chains, 
           n.adapt = 100, 
           n.burnin = setts$n.burn,
           n.iter = setts$n.iter, 
           n.thin = setts$n.thin,
           n.cores = 3,
           parallel = TRUE)
time.taken <- proc.time() - timer
time.taken[3]