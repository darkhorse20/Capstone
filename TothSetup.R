# Epsilon Intelligence order book simulation
# library(msversion)
# addpkg("VGAM", "0.8-7-r214")
# library("VGAM")


setwd("./")

#Book setup
L <- 40 #Set number of price levels to be included in iterations

# Generate initial book
LL <- 1 #Total number of levels in buy and sell books

mbLag <- 1;
msLag <- 1;


# Initialize book with asymptotic depth of 5 shares
initializeBook5 <- function()
{
  
  #   Price <<- -LL:LL
  Price <<- round(seq(0,LL,0.00100000), digits=3)
  n_L <<- LL/0.001
  cat('No of levels: ', n_L)
  # Book shape is set to equal long-term average from simulation
  buySize <<- c(rep(5000,(n_L/2)-8),4000,3500,3000,2500,2000,1500,1000,500,rep(0,(n_L/2)+1))
  sellSize <<- c(rep(0,(n_L/2)),0,500,1000,1500,2000,2500,3000,3500,4000,rep(5000,(n_L/2)-8))
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
dynamicBookShape<-function(band){c(book$buySize[(midPosn()-band):midPosn()],book$sellSize[midPosn()+1:band])}

bookPlot<-function(band){
  plot((-band:band),bookShape(band),
       col="red",type="l",xlab="Price",ylab="Quantity")
}

#Choose from L whole numbers in (1,...,L) with uniform probability
pick <- function(m){sample(1:m,1)}

pickToth <- function(m){sample(-m:m,1)}
pickDecimal <-function(m){sample( c(seq(-m,-0.001,0.001), seq(0.001,m,0.001)) ,1)}

# Switch logging on
logging <- T


#calculate market buy/sell quantity. fraction of the volume for a mkt buy/sell
calculateFracVolume <- function()
{
  u_rv <- runif(1,zeta_g,1.0);
  frac_g <<- 1 - exp(log(u_rv/zeta_g)/(zeta_g-1));
#   frac_g <<- 1 - exp(log(-u_rv)/zeta_g);
}


#Buy limit order
limitBuyOrder <- function(price=NA){
  if (is.na(price))
  {prx <<- (bestOffer()-round(pick(L)*0.001,digits=3))}
  else prx <<-price  
  if(logging==T){eventLog[count,]<<- c("LB",prx)} 
  book$buySize[book$Price==prx]<<-book$buySize[book$Price==prx]+1} 

#Sell limit order
limitSellOrder <- function(price=NA){
  if (is.na(price))
  {prx <<- (bestBid()+round(pick(L)*0.001,digits=3))}
  else prx <<-price  
  if(logging==T){eventLog[count,] <<- c("LS",prx)}  
  book$sellSize[book$Price==prx]<<-book$sellSize[book$Price==prx]+1} 

#Limit Order for Toth
limitOrder <- function(price=NA, nr_ords) {
#    cat('Limit Orders: ', nr_ords, '\n')
#   for(cnt in 1:nr_ords) {
#     posn <- pick(2*L)
#   
#     if(posn <= L) {
#       #This is a buy order
#       lvl <- askPosn() - (L-posn)
#       book$buySize[lvl]<<-book$buySize[lvl]+1
#     
#     } else {
#       lvl <- bidPosn() + (posn-L)
#       book$sellSize[lvl]<<-book$sellSize[lvl]+1
#     
#     }
#   }
  
  startBuy<-askPosn()
  m <- length(nr_ords)/2
  startSell<- bidPosn()
  
  for(cnt in 1:m ){
      
    if(nr_ords[cnt] > 0) {
      book$buySize[startBuy-cnt]<<-book$buySize[startBuy-cnt]+1
    }
    
    if(nr_ords[m + cnt] > 0) {
      book$sellSize[startSell+cnt]<<-book$sellSize[startSell+cnt]+1
    }
      
  }     
#       book$buySize[startBuy-cnt]<<-book$buySize[startBuy-cnt]+nr_ords[cnt]
#       book$sellSize[startSell+cnt]<<-book$sellSize[startSell+cnt]+nr_ords[m + cnt]
      
    
    
#     if(pickVal < 0) {
#       if (is.na(price))
#       {prx <<- (mid()+ pickVal)}
#       else prx <<-price  
#       prx <- round(prx,digits=1)
#       if(logging==T){eventLog[count,]<<- c("LB",prx)} 
#       #     cat('Limit Buy prx: ',prx, ' at size: ', book$buySize[book$Price==prx],'\n')
#       book$buySize[book$Price==prx]<<-book$buySize[book$Price==prx]+1
#       cat('Limit Buy\n');
#       
#     } else {
#       if (is.na(price))
#       {prx <<- (mid()+ pickVal)}
#       else prx <<-price  
#       prx <- round(prx,digits=1)
#       if(logging==T){eventLog[count,] <<- c("LS",prx)}  
#       #     cat('Limit Sell prx: ',prx, ' at size: ', book$sellSize[book$Price==prx],'\n')
#       book$sellSize[book$Price==prx]<<-book$sellSize[book$Price==prx]+1
#       cat('Limit Sell\n');
#     }
#   }

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

cancelOrder <- function(price=NA, nr_cxls) {
#   cat('Cxl Order\n');
  
  for(i in 1:nr_cxls) {
    c <- sample(1:2,1)
    switch(c,
           cancelSellOrder(),
           cancelBuyOrder())
    
  }

#   startBuy<-midPosn()-band-1
#   m <- length(nr_cxls)/2
#   startSell<-midPosn()+1
#   
#   for(cnt in 1:m ){
#     
#     book$buySize[startBuy+cnt]<<-book$buySize[startBuy+cnt]-nr_cxls[cnt]
#     
#     book$sellSize[startSell+cnt]<<-book$sellSize[startSell+cnt]-nr_cxls[m + cnt]
#   }
  
}


#Market buy order
marketBuyOrder <- function(){
  mbLag <<- 1;
  msLag <<- msLag+1;
  prx <<- bestOffer() 
#   cat('In mkt sell, best offer: ', prx,'\n')
  if(logging==T){eventLog[count,]<<- c("MB",prx)} 
  calculateFracVolume();
  volume <- ceiling(frac_g * book$sellSize[book$Price==prx]);
   cat('MKT Buy Vol: ', volume,'Vol at best offer: ',book$sellSize[book$Price==prx], '\n')
  book$sellSize[book$Price==prx]<<- book$sellSize[book$Price==prx]-volume;
#   cat('New Buy size: ', book$sellSize[book$Price==prx],'\n')
  curr_L <<- curr_L - 1 
  
}

#Market sell order
marketSellOrder <- function(){
  msLag <<- 1;
  mbLag <<- mbLag+1;                      
  prx <<- bestBid() 
#   cat('In mkt sell, best bid: ', prx,'\n')
  if(logging==T){eventLog[count,]<<- c("MS",prx)} 
  calculateFracVolume();
  volume <- ceiling(frac_g * book$buySize[book$Price==prx]);
  cat('MKT Sell Vol: ', volume,'Vol at best bid: ',book$buySize[book$Price==prx], '\n')
  book$buySize[book$Price==prx]<<-book$buySize[book$Price==prx]-(volume);
#   cat('New Buy size: ', book$buySize[book$Price==prx],'\n')
  curr_L <<- curr_L - 1
  
}

## 
# prob_vector_L <- function(L_qty) {
#   p_L <- (alpha)*(1/L_qty^(alpha+1))
# }

## calculate the market buy/sell probability based on powerlaw distribution
marketOrder <- function(){
  
  if(curr_L <= 0) {
    u <- runif(1,0.0,1.0)
    
    curr_L <<- round(exp(-log(u)/(alpha+1)))
    curr_b_s <<- sample(1:2,1, replace = TRUE, c(0.5,0.5));
  }
  cat('Current L:', curr_L, '\n')
  
  switch(curr_b_s,
         marketBuyOrder(),
         marketSellOrder()
         )
  
}



##Agent order, he's always selling
newAgentOrder <- function() {
  if(agent_L <= 0 & agentDone == FALSE) {
    prob_L <- lapply(L_vals, prob_vector_L);
    curr_L <<- sample(L_vals,1, replace = TRUE, prob_L);
    
  }
  
  prx <<- bestBid() 
  if(logging==T){eventLog[count,]<<- c("MS",prx)} 
  calculateFracVolume();
  volume <- frac_g * book$buySize[book$Price==prx];
  book$buySize[book$Price==prx]<<-round(book$buySize[book$Price==prx]-(volume));
  agent_L <<- round(agent_L - volume)
  if(agent_L <= 0) { agentDone <- TRUE }
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

generateEventZIModified <- function()
{
  nb <<- sum(book$buySize[book$Price>=(bestOffer()-round(L*0.001, digits=3))]); # Number of cancelable buy orders
  ns <<- sum(book$sellSize[book$Price<=(bestBid()+round(L*0.001, digits=3))]); # Number of cancelable sell orders
  eventRate <<- nb*nu+ns*nu + mu +2*L*lamb_da;
  probEvent <- c(L*lamb_da,L*lamb_da,nb*nu,ns*nu,mu)/eventRate;
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
  nb <<- sum(book$buySize[book$Price>=(bestOffer()-round(L*0.001, digits=3))]); # Number of cancelable buy orders
  ns <<- sum(book$sellSize[book$Price<=(bestBid()+round(L*0.001, digits=3))]); # Number of cancelable sell orders
  eventRate <<- nb*nu+ns*nu + mu +2*L*lamb_da;
  
  limOrds <- rpois(2*L, lambda = lamb_da)
#   if(limOrds > 0) {
    limitOrder(nr_ords=limOrds)  
#   }
  
  mktOrd <- rpois(1, lambda = mu)
  if(mktOrd >0) {
    
    ## Agent ORder ##
    if(withAgent == TRUE) {
      if(iter > burnin & iter < (burnin + 3000) ) {
        m <- sample(1:2,1, replace = TRUE, c(0.05,0.95))
        if(m ==1) {
          cat('Agent Order');
        }
        switch (m,
                newAgentOrder(),
                marketOrder()
                );
        
      } else {
        marketOrder()  
      }
      
    } else {
      marketOrder()
    }

  }
  
  cxlOrd <- rpois(1, lambda = nu*(nb+ns))
  cancelOrder(nr_cxls = cxlOrd)
  
}

#####################################################################################################
# End of setup
#####################################################################################################
