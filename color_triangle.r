
# to load this file, could try:
#    source("~/dev/R/scripts/color_triangle.r")

# Author: Philip Kinlen, Aug 2014
library(grid) # required for grid.raster(.)

# To see what this triangle looks like, have a look at the following URL:
#       http://abitofmaths.blogspot.hk/2014/08/pretty-pictures-with-r.html

# numPix should be an integer,
# and gapX < (numPix / 2) 
# where gapX is the minimum number of horizontal pixels from the triangle to the edge of the image
RGBTriangle <- function(numPix = 100, gapX = 10, doSmoothing=FALSE) {
  # the verticle gap between the triangle and edge of image
  gapX            <- gapX 
  gapY            <- numPix * ( 0.5 - sqrt(3)/4) + gapX * sqrt(3)/2

  xArr            <- 1:numPix
  yArr            <- numPix:1   # The function call grid.raster(..) below will put the elements with 
                                # highest row number at the bottom.
                                # For our array, the last shall be at the bottom.
  # The x's are constant in the matrix columns, y's are constant along the matrix rows.
  xMat            <- matrix(rep(xArr, numPix), numPix, numPix, F)  
  yMat            <- matrix(rep(yArr, numPix), numPix, numPix, T)  

  m1              <- sqrt(3)                                 # slope
  c1              <- gapY + 0.25 - m1 * gapX                 # intercept
  
  m2              <- -sqrt(3)                                # slope
  c2              <- gapY + 0.25 - m2 * (numPix - gapX + 1)  # intercept,
  
  height          <- numPix + 0.5 - 2 * gapY          # Height of triangle in pixels

  red             <- matrix(mapply(rightFade, xMat, yMat, m1, c1, m2, c2, gapY, height), numPix, numPix, T)
  green           <- matrix(mapply(leftFade,  xMat, yMat, m1, c1, m2, c2, gapY, height), numPix, numPix, T)
  blue            <- matrix(mapply(topFade,   xMat, yMat, m1, c1, m2, c2, gapY, height), numPix, numPix, T)
  
  col             <- rgb(red, green, blue)
  dim(col)        <- dim(red)
  grid.raster(col, interpolate=doSmoothing) 
  # One possible source of minor confusion is that when we write a cartesian co-ordinate (x,y)
  # the x comes first.
  # but in a matrix, we specify the row index first, which corresponds to y
} 

#################################################################
topFade <- function( x, y, m1, c1, m2, c2, gapY, height){
   if ( isInsideTriangle( x, y, m1, c1, m2, c2, gapY)){
     res <- (y - gapY) / height    # A rounding error may cause res to be outside the range [0,1]
     return (max(0, min(1, res)))  # If left untreated that could cause an error in grid.raster(.)
   } else
     return (0)     
}
#################################################################
leftFade <- function( x, y, m1, c1, m2, c2, gapY, height){
  if ( isInsideTriangle( x, y, m1, c1, m2, c2, gapY)){ 
    res <- sqrt(3) * ((y - c2) / m2 - x) / ( 2 * height) 
    return (max(0, min(1, res))) 
  } else
    return (0)     
}  
#################################################################
rightFade <- function( x, y, m1, c1, m2, c2, gapY, height){
  if ( isInsideTriangle( x, y, m1, c1, m2, c2, gapY)){
    res <- 0.5 * (m1 * x + c1 - y) / height
    return (max(0, min(1, res))) 
  } else
    return (0)     
}  
#################################################################
isInsideTriangle <- function(x,y, m1, c1, m2 ,c2, gapY){
  
  if (  ( y >  gapY ) 
      & ( y < ( m1 * x + c1 ) ) 
      & ( y < ( m2 * x + c2 ) ) )
    return(T)
  else 
    return (F)
}
