setwd("\\\\Msad\\root\\NA\\NY\\users\\anuroopy\\My Documents\\aya\\MFE\\Capstone")

source('RvEstimators.R')

runPar <- FALSE

nu <- 0.0001; mu<- 0.1; lamb_da <- 0.5; gamma <- 0.8; 
phi <- 0.05
zeta_g <- 0.65;
band <- 150
burnin <- 5000

# Generate initial book
LL <- 10 #Total number of levels in buy and sell books

#Book setup
L <- 100 #40Set number of price levels to be included in iterations
scale <- 0.01
multiplier <-  100
numEvents <- 10000;

aveMidPrxsTable <- read.table(file = "C:\\MSDE\\anuroopy\\Capstone\\TothBookAveMidPrxs.csv")
aveMidPrxs <- aveMidPrxsTable[,1]

aveBookShapeTable <- read.table(file = "C:\\MSDE\\anuroopy\\Capstone\\TothBookAveBookShape.csv")
aveBookShape <- as.numeric(aveBookShapeTable[,1])

bookErrsTable <- read.table(file = "C:\\MSDE\\anuroopy\\Capstone\\TothBookBookShapes.csv")
bookErrs <- as.numeric(bookErrsTable[,1])

allBookShapesTable <- read.table(file = "C:\\MSDE\\anuroopy\\Capstone\\TothBookAll.csv")
allBookShapes <- as.numeric(aveBookShapeTable[,1])

plot(aveBookShape, col="red",type="l",xlab="Price",ylab="Quantity", main="Average Book Shape")

plot(aveMidPrxs, type = "l")
  
numEvents <- length(aveMidPrxs)

# aveMidPrxs <- aveMidPrxs[burnin:15000]
oldAveMidPrxs <- aveMidPrxs
#aveMidPrxs <- oldAveMidPrxs[4000:length(oldAveMidPrxs)]

# aveMidPrxs <- log(oldAveMidPrxs)
#rv <- ZHOU(aveMidPrxs, 1)
# rv <-TSRV(aveMidPrxs, 1)
stdev <- sd(aveMidPrxs)

cat('Std. Dev: ', stdev)
rv = stdev*stdev

cat('Realized Vol: ', rv)
u_star <- sqrt(rv/(2*nu))
cat('U_star is :', u_star)
# u_star <- 0.5

########################################
# Plot book density comparison
########################################
u_values <- seq(0.0,1.5,0.01)
rho_inf <- lamb_da/nu

getEmpBookDensityRatio <- function(indx) {
  return(aveBookShape[band - indx*multiplier]/rho_inf)
}

getAnalyticBookDensityRatio <- function(indx) {
  return( 1 - exp(-(indx/u_star)))
}

getEmpBookDensityRatioSellSide <- function(indx) {
  return(aveBookShape[band + indx*multiplier]/rho_inf)
}

getAveEmpBookDensity <- function(indx) {
  return( ( getEmpBookDensityRatioSellSide(indx) + getEmpBookDensityRatio(indx) )/2 )
}

getMeanErrorUpper <- function(indx) {
  return( (((aveBookShape[band - indx*multiplier] +aveBookShape[band + indx*multiplier])/2 + bookErrs[band - indx*multiplier])/rho_inf) )
}

getMeanErrorLower <- function(indx) {
  return( (((aveBookShape[band - indx*multiplier] +aveBookShape[band + indx*multiplier])/2  - bookErrs[band - indx*multiplier])/rho_inf) )
}

u_tgt <- 0.49
getTargetRatio <- function(u) {
  return( 1 - exp(-(u/u_tgt)))
}

# plot(u_values, sapply(u_values,FUN=getEmpBookDensityRatio), type="l", col="blue", 
#      main="Book Density Comparison", ylim=c(0.0,1.0), ylab="rho/rho_inf")
# par(new = 'T')

plot(u_values, sapply(u_values,FUN=getAnalyticBookDensityRatio), type="p", col="red", 
     main="Book Density Comparison", ylim=c(0.0,1.0), ylab="rho/rho_inf")
par(new = 'T')

# plot(u_values, sapply(u_values,FUN=getEmpBookDensityRatioSellSide), type="l", col="green", 
#      main="Book Density Comparison", ylim=c(0.0,1.0), ylab="rho/rho_inf")
# par(new = 'T')

plot(u_values, sapply(u_values,FUN=getTargetRatio), type="p", col="yellow", 
     main="Book Density Comparison", ylim=c(0.0,1.0), ylab="rho/rho_inf")

par(new = 'T')
plot(u_values, sapply(u_values,FUN=getAveEmpBookDensity), type="l", col="black", 
     main="Book Density Comparison", ylim=c(0.0,1.0), ylab="rho/rho_inf")

par(new = 'T')
lower <- unlist(sapply(u_values,FUN=getMeanErrorLower))
upper <- unlist(sapply(u_values,FUN=getMeanErrorUpper))

arrows(x0=u_values,y0=upper,x1=u_values,y1=lower,angle=90, code=3, length=0.01, col='orange');
legend('bottomright',legend=c('Emp. Density', 
'Analytical Density, obs. vol.',
'Analytical Density - Toth'),col=c('black','red','yellow'),pch='-',lwd=1)

par(new = 'F')
