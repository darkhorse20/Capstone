setwd("./")

source('RvEstimators.R')
source('TothSetup.R')

nu <- 0.0001; mu<- 0.1; lamb_da <- 0.5; gamma <- 0.8; 
eventRate <- 1
phi <- 0.05
zeta_g <- 0.0;
# frac_g <- 1.0;
band <- 40
burnin <- 0


L_vals <- c(1:20)
alpha <- gamma +1
nb <- 0
ns <- 0
agent_L <- 0
agentDone <- FALSE
withAgent <- FALSE
# set.seed(9062013)
set.seed(9)
numSims <- 2

bookState <- as.data.frame(matrix(0,nrow=2*band+1, ncol=1));


runBooksSimulation <- function(.) {

  numEvents <- 500;
  # Generate initial book
  LL <- 1 #Total number of levels in buy and sell books
  
  #Book setup
  L <- 40 #Set number of price levels to be included in iterations
  
  #   Price <<- -LL:LL
  Price <- round(seq(0,LL,0.00100000), digits=3)
  n_L <- LL/0.001
  cat('No of levels: ', n_L)
  # Book shape is set to equal long-term average from simulation
  buySize <- c(rep(5000,(n_L/2)-8),4000,3500,3000,2500,2000,1500,1000,500,rep(0,(n_L/2)+1))
  sellSize <- c(rep(0,(n_L/2)),0,500,1000,1500,2000,2500,3000,3500,4000,rep(5000,(n_L/2)-8))
  book <- data.frame(Price, buySize, sellSize ) 
  count <- 0
  eventType <- c("LB","LS","CB","CS","MB","MS")
  eventDescr <- NA
  midPrxs <- c(rep(0, numEvents))
  fracArr <- c(rep(0, numEvents))
  curr_L <- 0
  curr_b_s <- 1
  
  for(count in 1:numEvents){
    #generateEvent()
    generateTothEvent(book, curr_L, curr_b_s)
    #generateEventZIModified();
    midPrxs[count] <- mid(book) 
#     fracArr[count] <- frac_g;
    
  }
  
  return(book)
  
}

# Many simulations of Monty Hall games
monteCarlo <- function(sims, cores=detectCores()){
  require(parallel)
  # clusterApply() for Windows
  if (Sys.info()[1] == "Windows"){
    cl <- makeCluster(cores)
    clusterExport(cl, list("generateTothEvent", "marketOrder", "bestOffer", "bestBid", "mid",
                           "askPosn", "bidPosn", "midPosn","limitOrder", "limitBuyOrder",
                           "limitSellOrder", "cancelOrder", "cancelBuyOrder", "cancelSellOrder",
                           "pick", "pickToth", "L", "LL", "nu", "alpha", "nb","ns",
                           "lamb_da", "zeta_g", "withAgent", "agentDone", "phi", "band",
                           "mu","marketBuyOrder", "marketSellOrder", "calculateFracVolume",
                           "msLag","mbLag"))
    runtime <- system.time({
      books <- clusterApply(cl=cl, x=1:sims, fun=runBooksSimulation)
    })[3]
    stopCluster(cl) # Don't forget to do this--I frequently do
    
    # mclapply() for everybody else
  } else {
    runtime <- system.time({
      books <- mclapply(X=1:sims, FUN=runBooksSimulation, mc.cores=cores)
    })[3]
  }
  return(books)
}

#runBooksSimulation()

 allbooks <- monteCarlo(4, cores=4)

#   bookState[,1] <- bookState[,1] + dynamicBookShape(band)
#   cat('Done with simulation: ',n,'\n')

# tradeLog <- eventLog[(eventLog$Type=="MB")|(eventLog$Type=="MS"),]
# tradeSigns <- ifelse(tradeLog$Type=="MB",+1,-1);
# someTrades <- 1:100;
#  plot(tradeSigns[someTrades],type="b",col="blue",xlab=NA, ylab="Trade Sign",main="Epsilon Intel. trades")
#  bookPlot(20);
# 
# if(withAgent == TRUE) {
#   par(new = T)
#   plot((1:numEvents), midPrxs[1:numEvents],col="orange",type="l", ylim = c(45,55), xlab="Price", ylab="Num of Events",main="Mid Prx Process")
#   
# } else {
#   plot((1:numEvents), midPrxs[1:numEvents],col="blue",type="l", ylim = c(49.98,50.02), xlab="Price", ylab="Num of Events",main="Mid Prx Process")
#   
# }

#plot(fracArr)
# aveBookShape <- bookState[,1]/numSims;
# plot(aveBookShape, col="red",type="l",xlab="Price",ylab="Quantity", main="Average Book Shape")
# plot(book$Price[(midPosn()-band):(midPosn()+band)],c( book$buySize[(midPosn()-band):midPosn()], book$sellSize[(midPosn()+1):(midPosn()+band )]), col="red",type="l",xlab="Price",ylab="Quantity", main="Steady State Book Shape")


########################################################################################
# End of time series generation
########################################################################################

#rv <- ZHOU(midPrxs, 5)
# rv <-TSRV(midPrxs, 1)
# 
# cat('Realized Vol: ', rv)
# 
# u_star <- sqrt(rv/(2*nu))
# cat('U_star is :', u_star)