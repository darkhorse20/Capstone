# Epsilon Intelligence order book simulation

setwd("C:/aya/Documents/MFE/Baruch/Capstone/")

#Book setup
L <- 40 #Set number of price levels to be included in iterations

# Generate initial book
LL <- 1000 #Total number of levels in buy and sell books

mbLag <- 1;
msLag <- 1;

# Initialize book with asymptotic depth of 5 shares
initializeBook5 <- function()
{
    Price <<- -LL:LL    
    # Book shape is set to equal long-term average from simulation
    buySize <<- c(rep(50,LL-8),50,40,40,30,30,20,20,10,rep(0,LL+1))
    sellSize <<- c(rep(0,LL),0,10,20,20,30,30,40,40,50,rep(50,LL-8))
    book <<- data.frame(Price, buySize, sellSize ) 
    eventLog <<- as.data.frame(matrix(0,nrow=numEvents,ncol=2))
    colnames(eventLog)<<-c("Type","Price")
    count <<- 0
    eventType <<- c("LB","LS","CB","CS","MB","MS")
    eventDescr <<- NA
}


#Various utility functions
bestOffer <- function(){min(book$Price[book$sellSize>0])}
bestBid <- function(){max(book$Price[book$buySize>0])}
spread <- function(){bestOffer()-bestBid()}
mid <- function(){(bestOffer()+bestBid())/2}

#Functions to find mid-market
bidPosn<-function()length(book$buySize[book$Price<=bestBid()])
askPosn<-function()length(book$sellSize[book$Price<=bestOffer()])
midPosn<-function(){floor((bidPosn()+askPosn())/2)}

#Display center of book
go <- function(){book[(midPosn()-20):(midPosn()+20),]}

#Display book shape
bookShape<-function(band){c(book$buySize[LL+1+(-band:0)],book$sellSize[LL+1+1:band])}
dynamicBookShape<-function(band){c(book$buySize[LL+1+mid()+(-band:0)],book$sellSize[LL+1+mid()+1:band])}

bookPlot<-function(band){
  plot((-band:band),bookShape(band),
	col="red",type="l",xlab="Price",ylab="Quantity")
	}

#Choose from L whole numbers in (1,...,L) with uniform probability
pick <- function(m){sample(1:m,1)}

pickToth <- function(m){sample(-m:m,1)}

# Switch logging on
logging <- T

#calculate market buy/sell quantity. fraction of the volume for a mkt buy/sell
calculateFracVolume <- function()
{
  u_rv <- runif(1,zeta_g,1);
  frac_g <<- 1 - exp(log(u_rv/zeta_g)/(zeta_g-1));
  
}


#Buy limit order
limitBuyOrder <- function(price=NA){
			if (is.na(price))
            	{prx <<- (bestOffer()-pick(L))}
            else prx <<-price  
            if(logging==T){eventLog[count,]<<- c("LB",prx)} 
            book$buySize[book$Price==prx]<<-book$buySize[book$Price==prx]+1} 

#Sell limit order
limitSellOrder <- function(price=NA){
            if (is.na(price))
            	{prx <<- (bestBid()+pick(L))}
            else prx <<-price  
            if(logging==T){eventLog[count,] <<- c("LS",prx)}  
            book$sellSize[book$Price==prx]<<-book$sellSize[book$Price==prx]+1} 

#Limit Order for Toth
limitOrder <- function(price=NA) {
  pickVal <- pickToth(L)
  if(pickVal < 0) {
    if (is.na(price))
    {prx <<- (bestOffer()+pickVal)}
    else prx <<-price  
    if(logging==T){eventLog[count,]<<- c("LB",prx)} 
    book$buySize[book$Price==prx]<<-book$buySize[book$Price==prx]+1
    
  } else {
    if (is.na(price))
    {prx <<- (bestBid()+pickVal)}
    else prx <<-price  
    if(logging==T){eventLog[count,] <<- c("LS",prx)}  
    book$sellSize[book$Price==prx]<<-book$sellSize[book$Price==prx]+1
  }
}

#Cancel buy order            
cancelBuyOrder<-function(price=NA){
				q<-pick(nb) 
                tmp <- cumsum(rev(book$buySize))  #Cumulative buy size from 0
                posn <- length(tmp[tmp>=q]) #gives position in list where cumulative size >q
                prx <<- book$Price[posn] 
                if (!is.na(price)) {prx <<-price} 
                if(logging==T){eventLog[count,]<<- c("CB",prx)} 
                book$buySize[posn]<<-book$buySize[posn]-1}
                  

#Cancel sell order
cancelSellOrder<-function(price=NA){
                     q<-pick(ns) 
                     tmp <- cumsum(book$sellSize)  #Cumulative sell size from 0
                     posn <- length(tmp[tmp<q])+1 
                     prx <<- book$Price[posn] 
                     if (!is.na(price)) {prx <<-price}  
                     if(logging==T){eventLog[count,]<<- c("CS",prx)} 
                     book$sellSize[posn]<<-book$sellSize[posn]-1}
    
cancelOrder <- function(price=NA) {
  cancelSellOrder();
  cancelBuyOrder();
}


#Market buy order
marketBuyOrder <- function(){
                    mbLag <<- 1;
                    msLag <<- msLag+1;
                    prx <<- bestOffer() 
                     if(logging==T){eventLog[count,]<<- c("MB",prx)} 
                    calculateFracVolume();
                    volume <- frac_g * book$sellSize[book$Price==prx];
                     book$sellSize[book$Price==prx]<<- round(book$sellSize[book$Price==prx]-(volume));
                    
                    
}

#Market sell order
marketSellOrder <- function(){
                      msLag <<- 1;
                      mbLag <<- mbLag+1;                      
                     prx <<- bestBid() 
                     if(logging==T){eventLog[count,]<<- c("MS",prx)} 
                      calculateFracVolume();
                      volume <- frac_g * book$buySize[book$Price==prx];
                     book$buySize[book$Price==prx]<<-round(book$buySize[book$Price==prx]-(volume));
                   
                      
}

## calculate the market buy/sell probability based on powerlaw distribution
marketOrder <- function(){

   probBuy <- 1/gamma * (mbLag)^ ( - (gamma) );   
   probSell <- 1/gamma * (msLag)^ ( - (gamma) );
  
   probBuy1 <- (mu/eventRate) * probBuy/(probBuy + probSell);
   probSell1 <- (mu/eventRate) * probSell/(probBuy + probSell);
    
#  cat('Prob Buy1:' , probBuy1, '\n');
#  cat('Prob Sell1:', probSell1, '\n');
   
  probMaktOrder <- c(probBuy1, probSell1);
  m <- sample(1:2,1, replace = TRUE, probMaktOrder);
  switch(m,
         marketBuyOrder(),
         marketSellOrder()
         )

}


#Generate an event and update the buy and sell books
#Note that limit orders may be placed inside the spread
generateEvent <- function()
    {
    nb <<- sum(book$buySize[book$Price>=(bestOffer()-L)]); # Number of cancelable buy orders
    ns <<- sum(book$sellSize[book$Price<=(bestBid()+L)]); # Number of cancelable sell orders
    eventRate <<- nb*delta+ns*delta + mu +2*L*alpha;
    probEvent <- c(L*alpha,L*alpha,nb*delta,ns*delta,mu)/eventRate;
    m <- sample(1:5, 1, replace = TRUE, probEvent); #Choose event type
    switch(m,
    		limitBuyOrder(),
    		limitSellOrder(),
    		cancelBuyOrder(),
    		cancelSellOrder(),
    		marketOrder()
    		);
    
    }

generateTothEvent <- function(iter)
{
  nb <<- sum(book$buySize[book$Price>=(bestOffer()-L)]); # Number of cancelable buy orders
  ns <<- sum(book$sellSize[book$Price<=(bestBid()+L)]); # Number of cancelable sell orders
  
  limOrd <- rpois(1, lambda = lamb_da)
  if(limOrd == 1) {
    limitOrder()
  }
  
  mktOrd <- rpois(1, lambda = mu*(1-phi))
  if(mktOrd ==1) {

    ## Agent ORder ##
#     if(iter > burnin & iter < (burnin + 2000) ) {
#       m <- sample(1:2,1, replace = TRUE, c(0.05,0.95))
#       #m <- rpois(1, lambda = 0.05)
#       switch (m,
#         marketSellOrder(),
#         marketOrder()
#       );
#       
#     } else {
#       marketOrder()  
#     }
     marketOrder()
  }
  
  mktSellAgent <- rpois(1, lambda = mu*phi)
  if(mktSellAgent ==1) {
    marketSellOrder();
  }
   
  cxlOrd <- rpois(1, lambda = nu)
  if(cxlOrd ==1) {
    cancelOrder()
  }
  

  
   
#   cxlOrd <- rpois(1, lambda = nu)
#   if(cxlOrd ==1) {
#     cancelSellOrder()
#   }
}

#####################################################################################################
# End of setup
#####################################################################################################

# Simulate and generate time series of returns
#alpha <- 0.0001; mu<- 0.1; delta <- 0.2; gamma <- 0.5; # Asymptotic book depth is 5
nu <- 0.0001; mu<- 0.1; lamb_da <- 0.2; gamma <- 0.5; 
eventRate <- 1
phi <- 0.05
zeta_g <- 0.65;
frac_g <- 0.0;
band <- 100
burnin <- 4000
numEvents <- 10000;
initializeBook5();
midPrxs <- c(rep(0, numEvents))
fracArr <- c(rep(0, numEvents))

set.seed(9062013)

bookState <- as.data.frame(matrix(0,nrow=2*band+1, ncol=1));
midprx <- c(rep(0,numEvents));


for(count in 1:numEvents){
  #generateEvent()
  generateTothEvent(count);
  midPrxs[count] <- mid() 
  fracArr[count] <- frac_g;
  
  if(numEvents > burnin) {
    bookState[,1] <- bookState[,1] + dynamicBookShape(band)
  }
  cat('Count: ', count, '\n')
  }

tradeLog <- eventLog[(eventLog$Type=="MB")|(eventLog$Type=="MS"),]
tradeSigns <- ifelse(tradeLog$Type=="MB",+1,-1);
someTrades <- 1:100;
plot(tradeSigns[someTrades],type="b",col="blue",xlab=NA, ylab="Trade Sign",main="Epsilon Intel. trades")
bookPlot(20);

plot((1:numEvents), midPrxs[1:numEvents],col="red",type="l", xlab="Price", ylab="Num of Events",main="Mid Prx Process")
#plot(fracArr)
aveBookShape <- bookState[,1]/numEvents;
plot((-band:band),aveBookShape, col="red",type="l",xlab="Price",ylab="Quantity", main="Average Book Shape")


########################################################################################
# End of time series generation
########################################################################################


