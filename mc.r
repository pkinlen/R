
# Philip Kinlen, Aug 2014

# To load the following script into an R session, could type:
#     source("/home/pnelnik/dev/R/scripts/mc.r")
# ( assuming that's the correct file-path)


# run a monte carlo
# numSim is the integer number of simulations
# randSeed will only be reset if it is non zero
# the simFunction is a function of zero arguments that returns a numerical value
runMC <- function( numSim, randSeed, simFunction){
  
  if ( randSeed != 0)
     set.seed(randSeed)
  
  sims         <- replicate(numSim, simFunction())
  meanResult   <- mean(sims) 
  stdDevResult <- sd(sims) / sqrt(numSim)
  
  return (  list(meanResult=meanResult, stdDevResult=stdDevResult))
}

# Generate a simulated lognormal asset price
getSimLNAssetPrice <- function(asset, years){
  vol    <- asset[["vol"]]
  drift  <- asset[["meanGrowth"]] - 0.5 * vol * vol
  return ( asset[["spot"]] * exp( years * drift - vol * sqrt(years) * rnorm(1)))
}

eqAsset <- function(spot, meanGrowth, vol){
  
  return( list( spot=spot, meanGrowth=meanGrowth, vol=vol))
}

option <- function( strike, maturityYrs, isCall){
   # isCall is a boolean, true for call, false for put
   
   return( list( strike=strike, maturityYrs=maturityYrs, isCall=isCall))
}
  
optionPayout <- function( spotAtMaturity, optionObj ){
    if ( optionObj[["isCall"]] )    
       return ( max (0, spotAtMaturity - optionObj[["strike"]]))
    else
       return ( max (0, optionObj[["strike"]] - spotAtMaturity))
}

optPortfolio <- function( options, quantities){
  return (list(options=options, quantities=quantities))
}

optPortVal <- function( optPortfolio, eqUnderlying){
   years          <- 1   # could / should extend this
   spotAtMaturity <- getSimLNAssetPrice(eqUnderlying, years)
   payouts        <- optionPayout(spotAtMaturity, optPortfolio[["options"]])
   
   return (sum(payouts))
}

simulatePortfolio <- function(){
   # create an asset
   spot       <- 100
   meanGrowth <- 0.02
   vol        <- 0.15
   
   underlying <- eqAsset(spot, meanGrowth, vol)
   
   #                   ( strike,  maturityYrs,    isCall)
   putOpt     <- option(    105,            1,     FALSE)
   callOpt    <- option(    110,            1,      TRUE)
   
   options    <- c( putOpt, callOpt)          
   quantities <- c( 1,      -1)        # long put, short call
   
   portfolio  <- optPortfolio(options, quantities)     
   val        <- optPortVal(portfolio, underlying)
     
   portValFn  <- function(){
       return (optPortVal(portfolio, underlying))
   }
   
   seed      <- 0  
   numSim    <- 10000
      
   mcRes <- runMC(numSim, seed, portValFn )  
       
   return(mcRes)   
}

  



