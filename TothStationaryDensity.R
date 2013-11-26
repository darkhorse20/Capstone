setwd("./")

source('RvEstimators.R')
source('TothSetup.R')
runPar <- TRUE

nu <- 0.0001; mu<- 0.1; lamb_da <- 0.5; gamma <- 0.8; 
eventRate <- 1
phi <- 0.05
zeta_g <- 0.2;
# frac_g <- 1.0;
band <- 300
burnin <- 0


L_vals <- c(1:20)
alpha <- gamma +1
agent_L <- 0
agentDone <- FALSE
withAgent <- FALSE
# set.seed(9062013)
#set.seed(9)
numSims <- 16
numCores <- 16
# Generate initial book
LL <- 10 #Total number of levels in buy and sell books

#Book setup
L <- 40 #40Set number of price levels to be included in iterations
scale <- 0.01

numEvents <- 5000;
bookState <- as.data.frame(matrix(0,nrow=2*band+1, ncol=1));
aveBook <- as.data.frame(matrix(0,nrow=(round(LL/scale) + 1), ncol=3));
allMidPrxs <- c(rep(0, numEvents))


runBooksSimulation <- function(.) {
  
  #   Price <<- -LL:LL
  Price <- round(seq(0,LL,scale), digits=3)
  n_L <- LL/scale
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
  set.seed(round(runif(1,min=1, max=10000000)))
  
  for(count in 1:numEvents){
    #generateEvent()
    ret_values <- generateTothEvent(book, curr_L, curr_b_s)
    book <- ret_values$Book
    curr_L <- ret_values$CL
    curr_b_s <- ret_values$BS
    
    #generateEventZIModified();
    midPrxs[count] <- mid(book) 
    #     fracArr[count] <- frac_g;
    
  }
  
  return(list(Book=book,MP=midPrxs))
  
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
                           "pick", "pickToth", "L", "LL", "nu", "alpha", 
                           "lamb_da", "zeta_g", "withAgent", "agentDone", "phi", "band",
                           "mu","marketBuyOrder", "marketSellOrder", "calculateFracVolume",
                           "msLag","mbLag", "numEvents"))
    runtime <- system.time({
      res <- clusterApply(cl=cl, x=1:sims, fun=runBooksSimulation)
    })[3]
    stopCluster(cl) # Don't forget to do this--I frequently do
    
    # mclapply() for everybody else
  } else {
    runtime <- system.time({
      res <- mclapply(X=1:sims, FUN=runBooksSimulation, mc.cores=cores)
    })[3]
  }
  return(res)
}

if(runPar == FALSE) {
  mc_res <- runBooksSimulation()
  plot(mc_res$MP)
  
} else {
  
  mc_res <- monteCarlo(numSims, numCores)
  
  for(k in 1:length(mc_res)) {
   bookState <<- bookState + dynamicBookShape(mc_res[[k]]$Book, band)
    aveBook <- aveBook + mc_res[[k]]$Book
    allMidPrxs <- allMidPrxs + mc_res[[k]]$MP
    
    
  }
  aveBookShape <- bookState[,1]/numSims;
  aveBook <- aveBook/numSims
  
  plot(aveBookShape, col="red",type="l",xlab="Price",ylab="Quantity", main="Average Book Shape")
  #plot(checkBook$Price[(midPosn(checkBook)-band):(midPosn(checkBook)+band)],c( checkBook$buySize[(midPosn(checkBook)-band):midPosn(checkBook)], checkBook$sellSize[(midPosn(checkBook)+1):(midPosn(checkBook)+band )]), col="red",type="l",xlab="Price",ylab="Quantity", main="Steady State Book Shape")
  aveMidPrxs <-  allMidPrxs/numSims
  
  plot(aveMidPrxs)
  
}




cat('Done with simulations: ',numSims,'\n')

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


########################################################################################
# End of time series generation
########################################################################################

rv <- ZHOU(aveMidPrxs, 5)
# rv <-TSRV(midPrxs, 1)
# 
 cat('Realized Vol: ', rv)
# 
 u_star <- sqrt(rv/(2*nu))
 cat('U_star is :', u_star)

########################################
# Plot book density comparison
########################################
u_values <- seq(0.0,3.0,0.1)
rho_inf <- lamb_da/nu


getEmpBookDensityRatio <- function(indx) {
  return(aveBookShape[band - indx*100]/rho_inf)
}

getAnalyticBookDensityRatio <- function(indx) {
  return( 1 - exp(-(indx/u_star)))
}

getEmpBookDensityRatioSellSide <- function(indx) {
  return(aveBookShape[band + indx*100]/rho_inf)
}

plot(u_values, sapply(u_values,FUN=getEmpBookDensityRatio), type="l", col="blue", main="Book Density Comparison")
par(new = 'T')
plot(u_values, sapply(u_values,FUN=getAnalyticBookDensityRatio), type="p", col="red", main="Book Density Comparison")
par(new = 'T')
plot(u_values, sapply(u_values,FUN=getEmpBookDensityRatioSellSide), type="l", col="green", main="Book Density Comparison")

