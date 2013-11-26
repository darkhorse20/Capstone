# Epsilon Intelligence order book simulation


setwd("./")

#Book setup
L <- 100 #Set number of price levels to be included in iterations

# Generate initial book
LL <- 1 #Total number of levels in buy and sell books
# 
# mbLag <- 1;
# msLag <- 1;


# Initialize book with asymptotic depth of 5 shares
initializeBook5 <- function(book)
{
  
  #   Price <- -LL:LL
  Price <- round(seq(0,LL,0.00100000), digits=3)
  n_L <- LL/0.001
  cat('No of levels: ', n_L)
  # Book shape is set to equal long-term average from simulation
  buySize <- c(rep(5000,(n_L/2)-8),4000,3500,3000,2500,2000,1500,1000,500,rep(0,(n_L/2)+1))
  sellSize <- c(rep(0,(n_L/2)),0,500,1000,1500,2000,2500,3000,3500,4000,rep(5000,(n_L/2)-8))
  book <- data.frame(Price, buySize, sellSize ) 
  eventLog <- as.data.frame(matrix(0,nrow=numEvents,ncol=2))
  colnames(eventLog)<-c("Type","Price")
  count <- 0
  eventType <- c("LB","LS","CB","CS","MB","MS")
  eventDescr <- NA
}


#Various utility functions
bestOffer <- function(book){min(book$Price[book$sellSize>0])}
bestBid <- function(book){max(book$Price[book$buySize>0])}
spread <- function(book){bestOffer(book)-bestBid(book)}
mid <- function(book){(bestOffer(book)+bestBid(book))/2}

#Functions to find mid-market
bidPosn<-function(book)length(book$buySize[book$Price<=bestBid(book)])
askPosn<-function(book)length(book$sellSize[book$Price<=bestOffer(book)])
midPosn<-function(book){floor((bidPosn(book)+askPosn(book))/2)}

#Display center of book
go <- function(book){book[(midPosn(book)-20):(midPosn(book)+20),]}

#Display book shape
bookShape<-function(book, band){c(book$buySize[LL+1+(-band:0)],book$sellSize[LL+1+1:band])}
dynamicBookShape<-function(book, band){c(book$buySize[(midPosn(book)-band):midPosn(book)],book$sellSize[(midPosn(book)+1):(midPosn(book)+band)])}

bookPlot<-function(book, band){
  plot((-band:band),bookShape(book, band),
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
  frac_g <- 1 - exp(log(u_rv/zeta_g)/(zeta_g-1));
  #   frac_g <- 1 - exp(log(-u_rv)/zeta_g);
  return(frac_g)
}


#Buy limit order
limitBuyOrder <- function(book, price=NA){
  if (is.na(price))
  {prx <- (bestOffer(book)-round(pick(L)*0.001,digits=3))}
  else prx <-price  
  #   if(logging==T){eventLog[count,]<- c("LB",prx)} 
  book$buySize[book$Price==prx]<-book$buySize[book$Price==prx]+1} 

#Sell limit order
limitSellOrder <- function(book, price=NA){
  if (is.na(price))
  {prx <- (bestBid(book)+round(pick(L)*0.001,digits=3))}
  else prx <-price  
  #   if(logging==T){eventLog[count,] <- c("LS",prx)}  
  book$sellSize[book$Price==prx]<-book$sellSize[book$Price==prx]+1} 

#Limit Order for Toth
limitOrder <- function(book, price=NA, nr_ords) {
  
  startBuy<-askPosn(book)
  m <- length(nr_ords)/2
  startSell<- bidPosn(book)
  
  for(cnt in 1:m ){
    
    if(nr_ords[cnt] > 0) {
      book$buySize[startBuy-cnt]<-book$buySize[startBuy-cnt]+1
    }
    
    if(nr_ords[m + cnt] > 0) {
      book$sellSize[startSell+cnt]<-book$sellSize[startSell+cnt]+1
    }
    
  }    
  return(book)  
  
}

#Cancel buy order            
cancelBuyOrder<-function(book, nb, price=NA){
  q<-pick(nb) 
  tmp <- cumsum(rev(book$buySize))  #Cumulative buy size from 0
  posn <- length(tmp[tmp>=q]) #gives position in list where cumulative size >q
  prx <- book$Price[posn] 
  if (!is.na(price)) {prx <-price} 
  #   if(logging==T){eventLog[count,]<- c("CB",prx)} 
  book$buySize[posn]<-book$buySize[posn]-1
  return(book)
}


#Cancel sell order
cancelSellOrder<-function(book, ns, price=NA ){
  q<-pick(ns) 
  tmp <- cumsum(book$sellSize)  #Cumulative sell size from 0
  posn <- length(tmp[tmp<q])+1 
  prx <- book$Price[posn] 
  if (!is.na(price)) {prx <-price}  
  #   if(logging==T){eventLog[count,]<- c("CS",prx)} 
  book$sellSize[posn]<-book$sellSize[posn]-1
  return(book)
}

cancelOrder <- function(book, nr_cxls, nb, ns, price=NA) {
  #   cat('Cxl Order\n');
  
  for(i in 1:nr_cxls) {
    c <- sample(1:2,1)
    switch(c,
           book <- cancelSellOrder(book, ns),
           book <- cancelBuyOrder(book, nb))
  }
  
  return(book)
}


#Market buy order
marketBuyOrder <- function(book, curr_L){
  
  prx <- bestOffer(book) 
  #   cat('In mkt sell, best offer: ', prx,'\n')
  #   if(logging==T){eventLog[count,]<- c("MB",prx)} 
  #   frac <- calculateFracVolume();
  volume <- ceiling(calculateFracVolume() * book$sellSize[book$Price==prx]);
  cat('MKT Buy Vol: ', volume,'Vol at best offer: ',book$sellSize[book$Price==prx], '\n')
  book$sellSize[book$Price==prx]<- book$sellSize[book$Price==prx]-volume;
  #   cat('New Buy size: ', book$sellSize[book$Price==prx],'\n')
  curr_L <- curr_L - 1 
  return(list(Book=book, CL=curr_L))
}

#Market sell order
marketSellOrder <- function(book, curr_L){
  
  prx <- bestBid(book) 
  #   cat('In mkt sell, best bid: ', prx,'\n')
  #   if(logging==T){eventLog[count,]<- c("MS",prx)} 
  
  volume <- ceiling(calculateFracVolume() * book$buySize[book$Price==prx]);
  cat('MKT Sell Vol: ', volume,'Vol at best bid: ',book$buySize[book$Price==prx], '\n')
  book$buySize[book$Price==prx]<-book$buySize[book$Price==prx]-volume;
  #   cat('New Buy size: ', book$buySize[book$Price==prx],'\n')
  curr_L <- curr_L - 1
  
  return(list(Book=book, CL=curr_L))
}



## calculate the market buy/sell probability based on powerlaw distribution
marketOrder <- function(book, curr_L, curr_b_s){
  
  if(curr_L <= 0) {
#     curr_b_s <- rbinom(1,1, c(0.5,0.5));
#     if(curr_b_s ==1) curr_b_s <- 2
#     else curr_b_s <- 1
    #curr_b_s <- sample(1:2,1, replace = TRUE, c(0.5,0.5)); 
    curr_b_s <- sample(1:2,1); 
#     u <- runif(1,0.0,1.0)
    u <- runif(1)
    curr_L <- round(exp(-log(u)/(alpha+1)))

    
  }
  # doing this to look like sample's output
#   if(curr_b_s == 0) curr_b_s <- 1
#   else curr_b_s <-2
  
  cat('Current L:', curr_L, 'Buy-Sell', curr_b_s ,'\n')
  
  switch(curr_b_s,
         res <- marketBuyOrder(book, curr_L),
         res <- marketSellOrder(book, curr_L)
  )
  book <- res$Book
  curr_L <- res$CL
  
  return (list(Book=book, CL=curr_L, BS=curr_b_s))
}


##Agent order, he's always selling
newAgentOrder <- function() {
  if(agent_L <= 0 & agentDone == FALSE) {
    prob_L <- lapply(L_vals, prob_vector_L);
    curr_L <- sample(L_vals,1, replace = TRUE, prob_L);
    
  }
  
  prx <- bestBid() 
  #   if(logging==T){eventLog[count,]<- c("MS",prx)} 
  calculateFracVolume();
  volume <- frac_g * book$buySize[book$Price==prx];
  book$buySize[book$Price==prx]<-round(book$buySize[book$Price==prx]-(volume));
  agent_L <- round(agent_L - volume)
  if(agent_L <= 0) { agentDone <- TRUE }
}

#Generate an event and update the buy and sell books
#Note that limit orders may be placed inside the spread
generateEvent <- function()
{
  nb <- sum(book$buySize[book$Price>=(bestOffer()-L)]); # Number of cancelable buy orders
  ns <- sum(book$sellSize[book$Price<=(bestBid()+L)]); # Number of cancelable sell orders
  eventRate <- nb*delta+ns*delta + mu +2*L*alpha;
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
  nb <- sum(book$buySize[book$Price>=(bestOffer()-round(L*0.001, digits=3))]); # Number of cancelable buy orders
  ns <- sum(book$sellSize[book$Price<=(bestBid()+round(L*0.001, digits=3))]); # Number of cancelable sell orders
  eventRate <- nb*nu+ns*nu + mu +2*L*lamb_da;
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

generateTothEvent <- function(book, curr_L, curr_b_s)
{
  withAgent <- FALSE 
  nb <- sum(book$buySize[book$Price>=(bestOffer(book)-round(L*0.001, digits=3))]); # Number of cancelable buy orders
  ns <- sum(book$sellSize[book$Price<=(bestBid(book)+round(L*0.001, digits=3))]); # Number of cancelable sell orders
  eventRate <- nb*nu+ns*nu + mu +2*L*lamb_da;
  
  limOrds <- rpois(2*L, lambda = lamb_da)
  
  book <- limitOrder(book, nr_ords=limOrds)  
  
  
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
                res <- marketOrder(book, curr_L, curr_b_s)
                
                
        );
        
      } else {
        res <- marketOrder(book, curr_L, curr_b_s)
        
      }
      
    } else {
      res <- marketOrder(book, curr_L, curr_b_s)
    }
    
    book <- res$Book
    curr_L <- res$CL
    curr_b_S <- res$BS
    
  }
  
  cxlOrd <- rpois(1, lambda = nu*(nb+ns))
  book <- cancelOrder(book, nr_cxls = cxlOrd, nb, ns)
  
  #Get the midPR after this time step
  
  return (list(Book=book, CL=curr_L, BS=curr_b_s))
  
}

#####################################################################################################
# End of setup
#####################################################################################################
