# To see what the animated images look like,
# have a look at the following URL:
#    http://abitofmaths.blogspot.hk/2014/08/pretty-pictures-with-r.html

# Author: Philip Kinlen, Aug 2014
###################################################################
library( caTools)  # library required for write.gif(..)

###################################################################
animateRipples   <- function(){
  # for MS Windows might need to set the separator argument to a back-slash  
  filePath       <- paste(getwd(), "animatedRipples.gif", sep="/")
  numImagePerSec <- 25
  numSecForCycle <- 1
  
  numImages      <- numImagePerSec * numSecForCycle
  
  phases         <- -2 * pi * ( 0:(numImages-1)) /numImages 
  
  # The following line does the main work generating the data. 
  # The call to sapply(..) does the work that would have been done by a for-loop in
  # a language such as a Java.
  zeds           <- sapply(phases, rippleMat, simplify="array")    

  delayCS        <- 100 / numImagePerSec  # in hundreths of a second
  
  write.gif( zeds, filePath, col= getCustomColors(), delay = delayCS )  
}
###################################################################
animateSpiral     <- function(){
  # for MS Windows might need to set the separator argument to a back-slash
  filePath        <- paste(getwd(), "animatedSpiral.gif", sep="/")
  
  numImagesPerSec <- 20
  numSecForCycle  <- 5
  
  numImages       <- numImagesPerSec * numSecForCycle
  
  phases          <- 2 * pi * ( 0:(numImages-1)) * getSpiralGamma() / numImages

  # The following line does the main work generating the data. 
  # The call to sapply(..) does the work that would have been done by a for-loop in
  # a language such as a Java.
  zeds            <- sapply( phases, spiralMat, simplify="array")
  
  delayCS         <- 100 / numImagesPerSec  # in hundreths of a second
  
  write.gif( zeds, filePath, col= getCustomColors(), delay = delayCS )
}
###################################################################
# We use the beta and gamma when we are setting the 'z'
#   z        <- sin(phi * beta / gamma)
# where the phi is the angle ( in polar coordinates)
# and z the variable that's used to determine the color.
getSpiralBeta <- function(){
  # please use an integer
  return ( 1 )
}
getSpiralGamma <- function(){
  # please use an integer
  return (2)   # please don't use 1, or else the spiral will disappear!
}
###################################################################
getNumPixels <- function(){
  return ( 200)
}
###################################################################
getCustomColors   <- function(){
  maxCol     <- 255
  
  rising     <- 0:maxCol
  falling    <- maxCol - rising
  
  myRed      <- c(falling, rising) * 0.2 + 0.2
  myGreen    <- c(falling, rising) * 0.8 + 0.2
  myBlue     <- c(falling, rising) * 0.4 + 0.2
  
  return (rgb(  myRed, myGreen, myBlue,  maxCol, maxColorValue=maxCol ))
}  
###################################################################
# generate a matrix representation of a ripple
rippleMat <- function( phase, numPx = getNumPixels(), decay = 2.5, freq = 12 * pi){
   x <- seq( -1,  1, 2/ (numPx-1))
   y <- x 
   
   # We have a matrix r[i,j] = sqrt( x[i] * x[i] + y[j] * y[j])
   r <- sqrt( outer( x*x, y*y, "+"))   
   
   h <- exp( - decay * r ) * cos( phase + freq * r )
   return (h)
}
###################################################################
spiralMat    <- function(phase, beta = getSpiralBeta(), gamma = getSpiralGamma() , 
                         numPx= getNumPixels(), generateImage= FALSE){

  alpha    <- 0.1   # the main spiral follows: r = exp( alpha * phi)
  
  x        <- seq(-1, 1, 2 / (numPx - 1))
  y        <- x
  matR     <- sqrt(outer(x*x, y*y, "+"))
  
  matX     <- matrix(rep(x, numPx), numPx, numPx, F)  
  matY     <- matrix(rep(y, numPx), numPx, numPx, T)  
  theta    <- ( atan2(matY, matX) + pi) + phase

  # Note that:
  #      r >= exp( (2 * pi * (rev-1) + theta) * alpha)  
  # and  r <= exp( (2 * pi *  rev    + theta) * alpha)
  # where rev is the number of revolutions 
  rev      <- ceiling((log(matR)/ alpha  - theta) /( 2 * pi))

  phi      <- rev * 2 * pi + theta
  z        <- sin(phi * beta / gamma)

  if ( generateImage)
    image( z,  col = getCustomColors())
  else 
    return (z)
}
###################################################################
