#########################################
# ---------------------------------------
# Functions
# ---------------------------------------
#########################################

# One simulation of the Monty Hall game
onerun <- function(.){ # Function of no arguments
  doors <- 1:3
  prize.door <- sample(doors, size=1)
  choice <- sample(doors, size=1)
  
  if (choice==prize.door) return(0) else return(1) # Always switch
}

# Many simulations of Monty Hall games
MontyHall <- function(runs, cores=detectCores()){
  require(parallel)
  # clusterApply() for Windows
  if (Sys.info()[1] == "Windows"){
    cl <- makeCluster(cores)
    runtime <- system.time({
      avg <- mean(unlist(clusterApply(cl=cl, x=1:runs, fun=onerun)))
    })[3]
    stopCluster(cl) # Don't forget to do this--I frequently do
    
    # mclapply() for everybody else
  } else {
    runtime <- system.time({
      avg <- mean(unlist(mclapply(X=1:runs, FUN=onerun, mc.cores=cores)))
    })[3]
  }
  return(list(avg=avg, runtime=runtime))
}

#########################################
# ---------------------------------------
# Outputs
# ---------------------------------------
#########################################

run1 <- rbind(c(MontyHall(1e5, cores=1), "cores"=1))
run2 <- rbind(c(MontyHall(1e5, cores=4), "cores"=4))
run3 <- rbind(c(MontyHall(1e5, cores=2), "cores"=2))
run4 <- rbind(c(MontyHall(1e5, cores=8), "cores"=8))

rbind(run1, run2, run3, run4)