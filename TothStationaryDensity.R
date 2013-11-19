setwd("./")

source('RvEstimators.R')
source('TothSetup.R')

nu <- 0.0001; mu<- 0.1; lamb_da <- 0.5; gamma <- 0.8; 
eventRate <- 1
phi <- 0.05
zeta_g <- 0.0;
frac_g <- 1.0;
band <- 40
burnin <- 0
numEvents <- 500;
midPrxs <- c(rep(0, numEvents))
fracArr <- c(rep(0, numEvents))
curr_L <- 0
L_vals <- c(1:20)
alpha <- gamma +1
nb <- 0
ns <- 0
agent_L <- 0
agentDone <- FALSE
withAgent <- FALSE
# set.seed(9062013)
set.seed(9)
numSims <- 1

bookState <- as.data.frame(matrix(0,nrow=2*band+1, ncol=1));

 for (n in 1:numSims) {
  initializeBook5();
  
  for(count in 1:numEvents){
  #generateEvent()
  generateTothEvent(count);
  #generateEventZIModified();
  midPrxs[count] <- mid() 
  fracArr[count] <- frac_g;
  
#   if(numEvents > burnin) {
#     bookState[,1] <- bookState[,1] + dynamicBookShape(band)
#   }
#   cat('Count: ', count, '\n')
}
   bookState[,1] <- bookState[,1] + dynamicBookShape(band)
   cat('Done with simulation: ',n,'\n')
 }

tradeLog <- eventLog[(eventLog$Type=="MB")|(eventLog$Type=="MS"),]
tradeSigns <- ifelse(tradeLog$Type=="MB",+1,-1);
someTrades <- 1:100;
 plot(tradeSigns[someTrades],type="b",col="blue",xlab=NA, ylab="Trade Sign",main="Epsilon Intel. trades")
 bookPlot(20);

if(withAgent == TRUE) {
  par(new = T)
  plot((1:numEvents), midPrxs[1:numEvents],col="orange",type="l", ylim = c(45,55), xlab="Price", ylab="Num of Events",main="Mid Prx Process")
  
} else {
  plot((1:numEvents), midPrxs[1:numEvents],col="blue",type="l", ylim = c(49.98,50.02), xlab="Price", ylab="Num of Events",main="Mid Prx Process")
  
}

#plot(fracArr)
# aveBookShape <- bookState[,1]/numSims;
# plot(aveBookShape, col="red",type="l",xlab="Price",ylab="Quantity", main="Average Book Shape")
plot(book$Price[(midPosn()-band):(midPosn()+band)],c( book$buySize[(midPosn()-band):midPosn()], book$sellSize[(midPosn()+1):(midPosn()+band )]), col="red",type="l",xlab="Price",ylab="Quantity", main="Steady State Book Shape")


########################################################################################
# End of time series generation
########################################################################################

#rv <- ZHOU(midPrxs, 5)
rv <-TSRV(midPrxs, 1)

cat('Realized Vol: ', rv)

u_star <- sqrt(rv/(2*nu))
cat('U_star is :', u_star)